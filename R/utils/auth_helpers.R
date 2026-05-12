magic_code_expires_minutes <- function() 10L

magic_code_max_attempts <- function() 5L

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
  secret <- Sys.getenv("ORGANIZER_AUTH_SECRET", unset = "")
  if (!nzchar(secret)) {
    secret <- Sys.getenv("POLL_CREATION_SECRET", unset = "")
  }
  if (!nzchar(secret) && allow_dev_auth_code_display()) {
    secret <- "development-only-organizer-auth-secret"
  }
  if (!nzchar(secret)) {
    stop("Set ORGANIZER_AUTH_SECRET before using organizer login.", call. = FALSE)
  }
  secret
}

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
  email <- validate_email(email, field = "Organizer email")
  code <- validate_magic_code(code)
  digest::digest(paste(email, code, secret, sep = ":"), algo = "sha256", serialize = FALSE)
}
