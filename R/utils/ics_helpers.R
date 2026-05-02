ics_escape <- function(value) {
  value <- as.character(value %||% "")
  value <- gsub("\\\\", "\\\\\\\\", value)
  value <- gsub("\n", "\\\\n", value)
  value <- gsub(",", "\\\\,", value)
  value <- gsub(";", "\\\\;", value)
  value
}

ics_datetime <- function(utc_value) {
  format(parse_utc_timestamp(utc_value), "%Y%m%dT%H%M%SZ", tz = "UTC")
}

generate_ics <- function(poll, option, final_notes = "") {
  uid <- paste0("poll-", poll$poll_id[[1]], "-option-", option$option_id[[1]], "@meeting-availability")
  lines <- c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//Meeting Availability Poll//EN",
    "CALSCALE:GREGORIAN",
    "METHOD:PUBLISH",
    "BEGIN:VEVENT",
    paste0("UID:", uid),
    paste0("DTSTAMP:", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")),
    paste0("DTSTART:", ics_datetime(option$start_datetime[[1]])),
    paste0("DTEND:", ics_datetime(option$end_datetime[[1]])),
    paste0("SUMMARY:", ics_escape(poll$title[[1]])),
    paste0("DESCRIPTION:", ics_escape(paste(c(poll$description[[1]], final_notes), collapse = "\n\n"))),
    paste0("LOCATION:", ics_escape(poll$location_details[[1]] %||% "")),
    "END:VEVENT",
    "END:VCALENDAR"
  )
  paste(lines, collapse = "\r\n")
}
