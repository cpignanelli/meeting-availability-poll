now_utc <- function() {
  as.POSIXct(Sys.time(), tz = "UTC")
}

is_past_datetime <- function(datetime, reference_time = now_utc()) {
  as.numeric(as.POSIXct(datetime)) <= as.numeric(as.POSIXct(reference_time))
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

calendar_week_start <- function(date_value) {
  date_value <- as.Date(date_value)
  date_value - as.integer(format(date_value, "%w"))
}

calendar_week_label <- function(week_start) {
  week_start <- as.Date(week_start)
  week_end <- week_start + 6L
  if (identical(format(week_start, "%Y-%m"), format(week_end, "%Y-%m"))) {
    return(paste0(format(week_start, "%b "), as.integer(format(week_start, "%d")), "-", as.integer(format(week_end, "%d")), ", ", format(week_end, "%Y")))
  }
  paste0(
    format(week_start, "%b "),
    as.integer(format(week_start, "%d")),
    " - ",
    format(week_end, "%b "),
    as.integer(format(week_end, "%d")),
    ", ",
    format(week_end, "%Y")
  )
}

format_utc_offset <- function(datetime, timezone) {
  timezone <- validate_timezone(timezone)
  offset <- format(as.POSIXct(datetime, tz = "UTC"), "%z", tz = timezone)
  if (is.na(offset) || !grepl("^[+-][0-9]{4}$", offset)) {
    return("UTC offset unavailable")
  }
  paste0("UTC", substr(offset, 1, 3), ":", substr(offset, 4, 5))
}

is_all_day_option_label <- function(display_label) {
  grepl("all day", tolower(as.character(display_label %||% "")), fixed = TRUE)
}

ordinal_suffix <- function(day) {
  day <- as.integer(day)
  if (is.na(day)) {
    return("")
  }
  if (day %% 100L %in% 11:13) {
    return("th")
  }
  suffixes <- c("th", "st", "nd", "rd", rep("th", 6))
  suffixes[[day %% 10L + 1L]]
}

format_readable_date <- function(date_value, include_year = FALSE, ordinal = TRUE) {
  date_value <- as.Date(date_value)
  if (is.na(date_value)) {
    return("")
  }
  day <- as.integer(format(date_value, "%d"))
  day_label <- if (isTRUE(ordinal)) paste0(day, ordinal_suffix(day)) else as.character(day)
  label <- paste0(format(date_value, "%a, %b "), day_label)
  if (isTRUE(include_year)) {
    label <- paste0(label, ", ", format(date_value, "%Y"))
  }
  label
}

format_readable_clock <- function(datetime, timezone) {
  timezone <- validate_timezone(timezone)
  value <- format(as.POSIXct(datetime, tz = "UTC"), "%I:%M %p", tz = timezone)
  value <- sub("^0", "", value)
  sub(":00 ", " ", value, fixed = TRUE)
}

format_readable_time_range <- function(start_datetime, end_datetime, timezone) {
  start_local <- parse_utc_timestamp(start_datetime)
  end_local <- parse_utc_timestamp(end_datetime)
  start_ampm <- format(start_local, "%p", tz = timezone)
  end_ampm <- format(end_local, "%p", tz = timezone)
  start_time <- format_readable_clock(start_local, timezone)
  end_time <- format_readable_clock(end_local, timezone)
  if (identical(start_ampm, end_ampm)) {
    start_time <- sub(paste0(" ", start_ampm, "$"), "", start_time)
  }
  paste0(start_time, "-", end_time)
}

format_timezone_abbreviation <- function(datetime, timezone) {
  timezone <- validate_timezone(timezone)
  abbreviation <- format(as.POSIXct(datetime, tz = "UTC"), "%Z", tz = timezone)
  if (is.na(abbreviation) || !nzchar(abbreviation)) {
    return(format_utc_offset(datetime, timezone))
  }
  abbreviation
}

format_readable_option_label <- function(start_datetime, end_datetime, timezone, include_year = FALSE) {
  start_date <- as.Date(format(parse_utc_timestamp(start_datetime), "%Y-%m-%d", tz = timezone))
  end_date <- as.Date(format(parse_utc_timestamp(end_datetime), "%Y-%m-%d", tz = timezone))
  zone <- format_timezone_abbreviation(parse_utc_timestamp(start_datetime), timezone)
  if (identical(start_date, end_date)) {
    return(paste0(
      format_readable_date(start_date, include_year = include_year),
      ", ",
      format_readable_time_range(start_datetime, end_datetime, timezone),
      " ",
      zone
    ))
  }
  paste0(
    format_readable_date(start_date, include_year = include_year),
    ", ",
    format_readable_clock(parse_utc_timestamp(start_datetime), timezone),
    " to ",
    format_readable_date(end_date, include_year = include_year),
    ", ",
    format_readable_clock(parse_utc_timestamp(end_datetime), timezone),
    " ",
    zone
  )
}

format_readable_option_for_option <- function(option, timezone, include_year = FALSE) {
  if (is_all_day_option_label(option$display_label[[1]] %||% "")) {
    start_date <- as.Date(format(parse_utc_timestamp(option$start_datetime[[1]]), "%Y-%m-%d", tz = timezone))
    return(format_all_day_option_label(start_date, timezone))
  }
  format_readable_option_label(option$start_datetime[[1]], option$end_datetime[[1]], timezone, include_year = include_year)
}

timezone_with_offset_label <- function(timezone, reference_utc = NULL) {
  validate_timezone(timezone)
}

format_option_label <- function(start_datetime, end_datetime, timezone) {
  format_readable_option_label(start_datetime, end_datetime, timezone, include_year = TRUE)
}

format_all_day_option_label <- function(date_value, timezone) {
  timezone <- validate_timezone(timezone)
  date_value <- as.Date(date_value)
  if (is.na(date_value)) {
    stop("Choose a valid date for each all-day option.", call. = FALSE)
  }
  paste0(
    format_readable_date(date_value, include_year = TRUE),
    ", All day ",
    timezone
  )
}

resolve_duration_minutes <- function(duration_choice, custom_duration = NULL) {
  duration_choice <- as.character(duration_choice %||% "60")
  if (identical(duration_choice, "all_day")) {
    return(1440L)
  }
  if (identical(duration_choice, "custom")) {
    return(validate_duration(custom_duration))
  }
  validate_duration(duration_choice)
}

empty_selected_slots <- function() {
  data.frame(
    date = character(),
    start_time = character(),
    stringsAsFactors = FALSE
  )
}

slot_key <- function(date_value, start_time) {
  paste(as.character(date_value), as.character(start_time), sep = "|")
}

parse_slot_key <- function(key) {
  key <- as.character(key %||% "")
  parts <- strsplit(key, "\\|", fixed = FALSE)[[1]]
  if (length(parts) != 2L) {
    stop("Selected time data is malformed. Refresh the page and try again.", call. = FALSE)
  }
  date_value <- as.Date(parts[[1]])
  start_time <- parts[[2]]
  if (is.na(date_value) || !nzchar(start_time)) {
    stop("Selected time data is malformed. Refresh the page and try again.", call. = FALSE)
  }
  if (!identical(start_time, "all_day") && !grepl("^\\d{2}:\\d{2}$", start_time)) {
    stop("Selected time data is malformed. Refresh the page and try again.", call. = FALSE)
  }
  data.frame(date = as.character(date_value), start_time = start_time, stringsAsFactors = FALSE)
}

coerce_slot_selected <- function(value) {
  if (is.logical(value)) {
    return(isTRUE(value))
  }
  tolower(trimws(as.character(value %||% ""))) %in% c("true", "1", "yes", "selected")
}

apply_selected_slot_changes <- function(slots, changes, timezone, duration_minutes = NULL) {
  timezone <- validate_timezone(timezone)
  if (!is.null(duration_minutes)) {
    validate_duration(duration_minutes)
  }
  if (is.null(slots) || nrow(slots) == 0) {
    slots <- empty_selected_slots()
  }
  if (is.null(changes) || nrow(changes) == 0) {
    return(list(slots = slots, changed = FALSE, ignored_past = 0L))
  }
  if (!all(c("key", "selected") %in% names(changes))) {
    stop("Selected time data is malformed. Refresh the page and try again.", call. = FALSE)
  }

  changed <- FALSE
  ignored_past <- 0L
  for (i in seq_len(nrow(changes))) {
    parsed <- parse_slot_key(changes$key[[i]])
    date_value <- as.Date(parsed$date[[1]])
    start_time <- parsed$start_time[[1]]
    slot_start <- parse_local_datetime(date_value, if (identical(start_time, "all_day")) "00:00" else start_time, timezone)
    if (is_past_datetime(slot_start)) {
      ignored_past <- ignored_past + 1L
      next
    }

    key <- slot_key(date_value, start_time)
    keys <- slot_key(slots$date, slots$start_time)
    is_selected <- key %in% keys
    next_selected <- coerce_slot_selected(changes$selected[[i]])

    if (is_selected && !next_selected) {
      slots <- slots[keys != key, , drop = FALSE]
      changed <- TRUE
    } else if (!is_selected && next_selected) {
      slots <- rbind(
        slots,
        data.frame(date = as.character(date_value), start_time = start_time, stringsAsFactors = FALSE)
      )
      changed <- TRUE
    }
  }

  rownames(slots) <- NULL
  list(slots = slots, changed = changed, ignored_past = ignored_past)
}

selected_slots_to_options <- function(slots, duration_minutes, timezone) {
  timezone <- validate_timezone(timezone)
  duration_minutes <- validate_duration(duration_minutes)
  if (is.null(slots) || nrow(slots) == 0) {
    return(data.frame(
      start_datetime = character(),
      end_datetime = character(),
      display_label = character(),
      option_order = integer(),
      stringsAsFactors = FALSE
    ))
  }

  required_columns <- c("date", "start_time")
  if (!all(required_columns %in% names(slots))) {
    stop("Selected time data is malformed. Refresh the page and try again.", call. = FALSE)
  }

  rows <- lapply(seq_len(nrow(slots)), function(i) {
    date_value <- as.Date(slots$date[[i]])
    start_time <- as.character(slots$start_time[[i]])
    if (is.na(date_value)) {
      stop("One selected meeting date is invalid.", call. = FALSE)
    }

    if (identical(start_time, "all_day")) {
      start_local <- parse_local_datetime(date_value, "00:00", timezone)
      end_local <- add_minutes(start_local, 1440L)
      display_label <- format_all_day_option_label(date_value, timezone)
    } else {
      start_local <- parse_local_datetime(date_value, start_time, timezone)
      end_local <- add_minutes(start_local, duration_minutes)
      display_label <- format_option_label(
        as_utc_string(start_local),
        as_utc_string(end_local),
        timezone
      )
    }

    data.frame(
      start_datetime = as_utc_string(start_local),
      end_datetime = as_utc_string(end_local),
      display_label = display_label,
      option_order = i,
      stringsAsFactors = FALSE
    )
  })

  options <- do.call(rbind, rows)
  options <- options[order(options$start_datetime, options$end_datetime), , drop = FALSE]
  options <- options[!duplicated(options[c("start_datetime", "end_datetime")]), , drop = FALSE]
  options$option_order <- seq_len(nrow(options))
  rownames(options) <- NULL
  options
}

latest_option_date <- function(options, timezone) {
  timezone <- validate_timezone(timezone)
  if (is.null(options) || nrow(options) == 0 || !"start_datetime" %in% names(options)) {
    stop("Add at least one proposed meeting time.", call. = FALSE)
  }
  starts <- parse_utc_timestamp(options$start_datetime)
  if (any(is.na(starts))) {
    stop("One selected meeting time is invalid.", call. = FALSE)
  }
  max(as.Date(format(starts, tz = timezone, usetz = FALSE)))
}

effective_response_deadline <- function(manual_deadline = "", last_option_date) {
  last_option_date <- as.Date(last_option_date)
  if (is.na(last_option_date)) {
    stop("The final proposed meeting date could not be determined.", call. = FALSE)
  }

  if (is.null(manual_deadline) || length(manual_deadline) == 0 || is.na(manual_deadline) || !nzchar(as.character(manual_deadline))) {
    return(as.character(last_option_date))
  }

  manual_deadline <- as.Date(manual_deadline)
  if (is.na(manual_deadline)) {
    stop("Choose a valid response deadline.", call. = FALSE)
  }

  as.character(min(manual_deadline, last_option_date))
}

resolve_response_deadline <- function(use_manual_deadline, manual_deadline, options, timezone) {
  timezone <- validate_timezone(timezone)
  last_date <- latest_option_date(options, timezone)
  today_local <- as.Date(format(Sys.time(), tz = timezone, usetz = FALSE))

  if (isTRUE(use_manual_deadline)) {
    if (is.null(manual_deadline) || is.na(manual_deadline)) {
      stop("Choose an earlier response deadline or turn off the deadline option.", call. = FALSE)
    }
    if (as.Date(manual_deadline) < today_local) {
      stop("Choose today or a future date for the response deadline.", call. = FALSE)
    }
  } else {
    manual_deadline <- ""
  }

  deadline <- effective_response_deadline(manual_deadline, last_date)
  if (as.Date(deadline) < today_local) {
    stop("The final proposed meeting date has already passed. Choose future proposed times.", call. = FALSE)
  }
  deadline
}

format_deadline_label <- function(response_deadline) {
  if (is.null(response_deadline) || is.na(response_deadline) || !nzchar(response_deadline)) {
    return("No deadline")
  }
  format_readable_date(response_deadline, include_year = TRUE, ordinal = FALSE)
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
