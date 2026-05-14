magic_code_expires_minutes <- function() 10L

magic_code_max_attempts <- function() 5L

trusted_session_minutes <- function() {
  value <- suppressWarnings(as.integer(Sys.getenv("TRUSTED_SESSION_MINUTES", unset = "10")))
  if (is.na(value)) {
    value <- 10L
  }
  max(1L, min(60L, value))
}

truthy_env <- function(name, default = FALSE) {
  value <- tolower(trimws(Sys.getenv(name, unset = "")))
  if (!nzchar(value)) {
    return(isTRUE(default))
  }
  value %in% c("1", "true", "yes", "y", "on")
}

allow_dev_auth_code_display <- function() {
  truthy_env("ALLOW_DEV_AUTH_CODE_DISPLAY", default = FALSE)
}

owner_approval_configured <- function() {
  nzchar(trimws(Sys.getenv("APP_MAIN_OWNER_EMAIL", unset = "")))
}

app_main_owner_email <- function(required = TRUE) {
  email <- Sys.getenv("APP_MAIN_OWNER_EMAIL", unset = "")
  if (!nzchar(trimws(email))) {
    if (isTRUE(required)) {
      stop("Set APP_MAIN_OWNER_EMAIL before using organizer access controls.", call. = FALSE)
    }
    return("")
  }
  validate_email(email, field = "Main owner email")
}

is_main_owner_email <- function(email) {
  email <- validate_email(email, field = "Organizer email")
  identical(email, app_main_owner_email())
}

require_main_owner <- function(email) {
  if (!is_main_owner_email(email)) {
    stop("Only the main owner can manage organizer access.", call. = FALSE)
  }
  invisible(TRUE)
}

organizer_auth_secret <- function() {
  secret <- Sys.getenv("APP_AUTH_SECRET", unset = "")
  if (!nzchar(secret)) {
    secret <- Sys.getenv("ORGANIZER_AUTH_SECRET", unset = "")
  }
  if (!nzchar(secret)) {
    secret <- Sys.getenv("POLL_CREATION_SECRET", unset = "")
  }
  if (!nzchar(secret) && allow_dev_auth_code_display()) {
    secret <- "development-only-organizer-auth-secret"
  }
  if (!nzchar(secret)) {
    stop("Set APP_AUTH_SECRET or ORGANIZER_AUTH_SECRET before using email login.", call. = FALSE)
  }
  secret
}

app_auth_secret <- organizer_auth_secret

generate_magic_code <- function() {
  value <- sum(as.integer(openssl::rand_bytes(4)) * c(1, 256, 65536, 16777216))
  sprintf("%06d", abs(value) %% 1000000L)
}

validate_magic_code <- function(code) {
  code <- gsub("[^0-9]", "", as.character(code %||% ""))
  if (!grepl("^[0-9]{6}$", code)) {
    stop("Enter the 6-digit code.", call. = FALSE)
  }
  code
}

hash_magic_code <- function(email, code, secret = organizer_auth_secret()) {
  email <- validate_email(email, field = "Email")
  code <- validate_magic_code(code)
  digest::digest(paste(email, code, secret, sep = ":"), algo = "sha256", serialize = FALSE)
}

base64url_encode <- function(value) {
  raw_value <- charToRaw(as.character(value %||% ""))
  encoded <- openssl::base64_encode(raw_value)
  encoded <- chartr("+/", "-_", encoded)
  sub("=+$", "", encoded)
}

base64url_decode <- function(value) {
  value <- as.character(value %||% "")
  value <- chartr("-_", "+/", value)
  padding <- (4L - nchar(value) %% 4L) %% 4L
  if (padding > 0L) {
    value <- paste0(value, paste(rep("=", padding), collapse = ""))
  }
  rawToChar(openssl::base64_decode(value))
}

trusted_session_signature <- function(payload, secret = app_auth_secret()) {
  digest::hmac(secret, payload, algo = "sha256")
}

issue_trusted_session_token <- function(scope, email, poll_id = NULL, minutes = trusted_session_minutes()) {
  scope <- sanitize_text(scope, max_chars = 40, required = TRUE, field = "Session scope")
  if (!scope %in% c("organizer", "participant")) {
    stop("Session scope is invalid.", call. = FALSE)
  }
  email <- validate_email(email, field = "Session email")
  if (!is.null(poll_id)) {
    poll_id <- suppressWarnings(as.integer(poll_id))
    if (is.na(poll_id)) {
      stop("Session poll is invalid.", call. = FALSE)
    }
  }
  expires_at <- as_utc_string(add_minutes(now_utc(), minutes))
  payload <- list(
    scope = scope,
    email = email,
    poll_id = poll_id,
    expires_at = expires_at
  )
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  payload_encoded <- base64url_encode(payload_json)
  paste(payload_encoded, trusted_session_signature(payload_encoded), sep = ".")
}

verify_trusted_session_token <- function(token, expected_scope, poll_id = NULL) {
  token <- as.character(token %||% "")
  expected_scope <- sanitize_text(expected_scope, max_chars = 40, required = TRUE, field = "Session scope")
  if (!nzchar(token) || !grepl("^[A-Za-z0-9_-]+\\.[a-f0-9]+$", token)) {
    return(list(valid = FALSE, message = "Session token is missing or malformed."))
  }
  parts <- strsplit(token, "\\.", fixed = FALSE)[[1]]
  if (length(parts) != 2L) {
    return(list(valid = FALSE, message = "Session token is malformed."))
  }
  payload_encoded <- parts[[1]]
  supplied_signature <- parts[[2]]
  expected_signature <- trusted_session_signature(payload_encoded)
  if (!identical(supplied_signature, expected_signature)) {
    return(list(valid = FALSE, message = "Session token signature is invalid."))
  }
  payload <- tryCatch(
    jsonlite::fromJSON(base64url_decode(payload_encoded), simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(payload) || is.null(payload$scope) || is.null(payload$email) || is.null(payload$expires_at)) {
    return(list(valid = FALSE, message = "Session token payload is invalid."))
  }
  if (!identical(as.character(payload$scope), expected_scope)) {
    return(list(valid = FALSE, message = "Session token scope is invalid."))
  }
  email <- tryCatch(validate_email(payload$email, field = "Session email"), error = function(e) "")
  if (!nzchar(email)) {
    return(list(valid = FALSE, message = "Session token email is invalid."))
  }
  expires_at <- parse_utc_timestamp(payload$expires_at)
  if (is.na(expires_at) || expires_at <= now_utc()) {
    return(list(valid = FALSE, message = "Session token has expired."))
  }
  token_poll_id <- if (is.null(payload$poll_id) || is.na(payload$poll_id)) NULL else suppressWarnings(as.integer(payload$poll_id))
  if (!is.null(poll_id)) {
    poll_id <- suppressWarnings(as.integer(poll_id))
    if (is.na(poll_id) || is.null(token_poll_id) || !identical(token_poll_id, poll_id)) {
      return(list(valid = FALSE, message = "Session token poll scope is invalid."))
    }
  }
  list(
    valid = TRUE,
    scope = expected_scope,
    email = email,
    poll_id = token_poll_id,
    expires_at = as.character(payload$expires_at)
  )
}
