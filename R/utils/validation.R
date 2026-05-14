sanitize_text <- function(value, max_chars = 500, required = FALSE, field = "This field") {
  value <- paste(as.character(value %||% ""), collapse = "\n")
  value <- gsub("[[:cntrl:]&&[^\n\t]]", "", value, perl = TRUE)
  value <- trimws(value)
  if (required && !nzchar(value)) {
    stop(paste(field, "is required."), call. = FALSE)
  }
  if (nchar(value, type = "chars") > max_chars) {
    stop(paste(field, "must be", max_chars, "characters or fewer."), call. = FALSE)
  }
  value
}

html_escape <- function(value) {
  htmltools::htmlEscape(as.character(value %||% ""), attribute = FALSE)
}

normalize_email <- function(email) {
  email <- as.character(email %||% "")
  email <- email[!is.na(email)]
  email <- paste(email, collapse = "")
  tolower(trimws(email))
}

normalize_email_vector <- function(email) {
  email <- as.character(email %||% character())
  email[is.na(email)] <- ""
  tolower(trimws(email))
}

validate_email <- function(email, field = "Email") {
  email <- normalize_email(email)
  if (!nzchar(email)) {
    stop(paste(field, "is required."), call. = FALSE)
  }
  pattern <- "^[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}$"
  if (!grepl(pattern, email)) {
    stop(paste("Enter a valid", tolower(field), "address."), call. = FALSE)
  }
  email
}

validate_optional_email <- function(email, field = "Email") {
  email <- normalize_email(email)
  if (!nzchar(email)) {
    return(NA_character_)
  }
  validate_email(email, field = field)
}

validate_duration <- function(duration_minutes) {
  duration_minutes <- suppressWarnings(as.integer(duration_minutes))
  if (is.na(duration_minutes) || duration_minutes < 5 || duration_minutes > 1440) {
    stop("Meeting duration must be between 5 and 1440 minutes.", call. = FALSE)
  }
  duration_minutes
}

validate_token <- function(token, field = "Link token") {
  token <- trimws(as.character(token %||% ""))
  if (!grepl("^[a-f0-9]{64}$", token)) {
    stop(paste(field, "is invalid."), call. = FALSE)
  }
  token
}

generate_token <- function(bytes = 32) {
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(bytes))), collapse = "")
}

hash_token <- function(token) {
  token <- validate_token(token)
  digest::digest(token, algo = "sha256", serialize = FALSE)
}

availability_choices <- function() {
  c(
    "Available and preferred" = "preferred",
    "Available" = "available",
    "Unavailable" = "unavailable"
  )
}

availability_label <- function(value) {
  labels <- c(
    preferred = "Available and preferred",
    available = "Available",
    unavailable = "Unavailable",
    missing = "Missing response"
  )
  unname(labels[as.character(value)] %||% "Missing response")
}

validate_availability <- function(value) {
  value <- as.character(value %||% "")
  allowed <- unname(availability_choices())
  if (!value %in% allowed) {
    stop("Choose an availability value for every proposed time.", call. = FALSE)
  }
  value
}

parse_expected_participants <- function(text) {
  text <- sanitize_text(text, max_chars = 10000, required = FALSE, field = "Expected participants")
  if (!nzchar(text)) {
    return(data.frame(
      name = character(),
      email = character(),
      organization = character(),
      is_required = integer(),
      stringsAsFactors = FALSE
    ))
  }

  parsed <- tryCatch(
    utils::read.csv(
      text = text,
      header = FALSE,
      stringsAsFactors = FALSE,
      strip.white = TRUE,
      na.strings = "",
      col.names = c("name", "email", "organization", "is_required")
    ),
    error = function(e) {
      stop("Expected participants must use comma-separated rows: name,email,organization,required.", call. = FALSE)
    }
  )

  if (ncol(parsed) < 2) {
    stop("Expected participants must include at least name and email.", call. = FALSE)
  }

  parsed$name <- vapply(parsed$name, sanitize_text, character(1), max_chars = 120, required = TRUE, field = "Expected participant name")
  parsed$email <- vapply(parsed$email, validate_email, character(1), field = "Expected participant email")
  parsed$organization <- vapply(parsed$organization %||% "", sanitize_text, character(1), max_chars = 160, required = FALSE, field = "Expected participant organization")
  required_value <- tolower(trimws(as.character(parsed$is_required %||% "")))
  parsed$is_required <- ifelse(required_value %in% c("1", "true", "yes", "y", "required"), 1L, 0L)
  parsed <- unique(parsed[c("name", "email", "organization", "is_required")])
  parsed[!duplicated(parsed$email), , drop = FALSE]
}

app_base_url <- function(session) {
  base_url <- Sys.getenv("APP_BASE_URL", unset = "")
  if (!nzchar(base_url)) {
    protocol <- session$clientData$url_protocol %||% "http:"
    hostname <- session$clientData$url_hostname %||% "127.0.0.1"
    port <- session$clientData$url_port %||% ""
    pathname <- session$clientData$url_pathname %||% "/"
    host <- if (nzchar(port)) paste0(hostname, ":", port) else hostname
    base_url <- paste0(protocol, "//", host, pathname)
  }
  sub("/+$", "", base_url)
}

build_app_link_params <- function(session, params) {
  if (is.null(names(params)) || any(!nzchar(names(params)))) {
    stop("Link parameters must be named.", call. = FALSE)
  }
  base_url <- app_base_url(session)
  query <- paste(
    paste0(
      utils::URLencode(names(params), reserved = TRUE),
      "=",
      utils::URLencode(as.character(params), reserved = TRUE)
    ),
    collapse = "&"
  )
  separator <- if (grepl("\\?", base_url, fixed = FALSE)) "&" else "?"
  paste0(base_url, separator, query)
}

build_app_link <- function(session, parameter, token) {
  build_app_link_params(session, stats::setNames(list(token), parameter))
}

build_organizer_poll_link <- function(session, response_token) {
  response_token <- validate_token(response_token, field = "Response link token")
  build_app_link_params(session, list(organizer = "login", poll = response_token))
}

can_create_poll <- function(params, creation_secret = Sys.getenv("POLL_CREATION_SECRET", unset = "")) {
  if (!nzchar(creation_secret)) {
    return(FALSE)
  }
  supplied_secret <- params[["create"]] %||% ""
  identical(supplied_secret, creation_secret)
}

safe_error_message <- function(error) {
  message <- conditionMessage(error)
  if (!nzchar(message)) {
    return("Something went wrong. Please check the form and try again.")
  }
  unsafe_pattern <- paste(
    c("sqlite", "database", "sql", "constraint", "no such table", "file", "path", "traceback", "stack", "token"),
    collapse = "|"
  )
  if (grepl(unsafe_pattern, message, ignore.case = TRUE)) {
    return("Something went wrong while saving. Please check the form and try again.")
  }
  message
}
