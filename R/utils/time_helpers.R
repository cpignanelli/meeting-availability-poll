now_utc <- function() {
  as.POSIXct(Sys.time(), tz = "UTC")
}

utc_timestamp <- function(time = now_utc()) {
  format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

parse_utc_timestamp <- function(value) {
  value <- as.character(value %||% character())
  if (length(value) == 0) {
    return(as.POSIXct(character(), tz = "UTC"))
  }
  parsed <- as.POSIXct(rep(NA_real_, length(value)), origin = "1970-01-01", tz = "UTC")
  valid <- !is.na(value) & nzchar(value)
  parsed[valid] <- as.POSIXct(strptime(value[valid], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), tz = "UTC")
  parsed
}

parse_local_datetime <- function(date_value, time_value, timezone) {
  if (is.null(date_value) || is.na(date_value)) {
    stop("Choose a date for each proposed time.", call. = FALSE)
  }
  time_value <- trimws(as.character(time_value %||% ""))
  if (!grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", time_value)) {
    stop("Enter proposed times using 24-hour HH:MM format, such as 09:30 or 14:00.", call. = FALSE)
  }
  timezone <- validate_timezone(timezone)
  parsed <- as.POSIXct(
    paste(as.character(date_value), time_value),
    format = "%Y-%m-%d %H:%M",
    tz = timezone
  )
  if (is.na(parsed)) {
    stop("One of the proposed times could not be parsed. Check the date, time, and time zone.", call. = FALSE)
  }
  parsed
}

add_minutes <- function(datetime, minutes) {
  as.POSIXct(datetime + as.difftime(minutes, units = "mins"), origin = "1970-01-01", tz = attr(datetime, "tzone") %||% "UTC")
}

as_utc_string <- function(datetime) {
  utc_timestamp(as.POSIXct(datetime, tz = "UTC"))
}

format_option_label <- function(start_datetime, end_datetime, timezone) {
  timezone <- validate_timezone(timezone)
  start_local <- as.POSIXct(parse_utc_timestamp(start_datetime), tz = timezone)
  end_local <- as.POSIXct(parse_utc_timestamp(end_datetime), tz = timezone)
  start_label <- format(start_local, "%a, %b %d, %Y %I:%M %p", tz = timezone)
  end_label <- format(end_local, "%I:%M %p", tz = timezone)
  paste0(start_label, " - ", end_label, " ", timezone)
}

format_deadline_label <- function(response_deadline) {
  if (is.null(response_deadline) || is.na(response_deadline) || !nzchar(response_deadline)) {
    return("No deadline")
  }
  format(as.Date(response_deadline), "%b %d, %Y")
}

deadline_has_passed <- function(response_deadline, timezone) {
  if (is.null(response_deadline) || is.na(response_deadline) || !nzchar(response_deadline)) {
    return(FALSE)
  }
  timezone <- validate_timezone(timezone)
  today_local <- as.Date(format(Sys.time(), tz = timezone, usetz = FALSE))
  today_local > as.Date(response_deadline)
}

validate_timezone <- function(timezone) {
  timezone <- trimws(as.character(timezone %||% ""))
  if (!nzchar(timezone) || !timezone %in% OlsonNames()) {
    stop("Choose a valid time zone.", call. = FALSE)
  }
  timezone
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
