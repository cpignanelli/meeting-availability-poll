testthat::test_that("email and token validation work", {
  testthat::expect_equal(validate_email(" User@Example.ORG "), "user@example.org")
  testthat::expect_true(is.na(validate_optional_email("")))
  testthat::expect_true(is.na(validate_optional_email(NA_character_)))
  testthat::expect_equal(validate_optional_email(" Optional@Example.ORG "), "optional@example.org")
  token <- generate_token()
  testthat::expect_match(token, "^[a-f0-9]{64}$")
  testthat::expect_match(hash_token(token), "^[a-f0-9]{64}$")
  testthat::expect_error(validate_token("not-a-token"), "invalid")
})

testthat::test_that("expected participant parsing validates rows", {
  parsed <- parse_expected_participants("Alex Lee,alex@example.org,Institute A,yes\nSam Patel,sam@example.org,Partner,no")
  testthat::expect_equal(nrow(parsed), 2)
  testthat::expect_equal(parsed$is_required, c(1L, 0L))
  testthat::expect_error(parse_expected_participants("Bad User,not-email,Org,yes"), "valid")
})

testthat::test_that("availability scoring uses configured weights", {
  testthat::expect_equal(score_value(c("preferred", "available", "unavailable", "missing")), c(2L, 1L, 0L, 0L))
})

testthat::test_that("UTC timestamp parsing supports multiple proposed times", {
  parsed <- parse_utc_timestamp(c("2026-05-07T13:00:00Z", "2026-05-04T15:00:00Z"))
  testthat::expect_s3_class(parsed, "POSIXct")
  testthat::expect_equal(length(parsed), 2)
  testthat::expect_false(any(is.na(parsed)))
})

testthat::test_that("readable date/time helpers format ordinals, ranges, and time zones", {
  start_utc <- "2026-07-01T13:00:00Z"
  end_utc <- "2026-07-01T14:30:00Z"
  winter_utc <- "2026-01-15T14:00:00Z"
  timezone <- "America/Toronto"

  testthat::expect_equal(vapply(c(1, 2, 3, 4, 11, 12, 13, 21), ordinal_suffix, character(1)), c("st", "nd", "rd", "th", "th", "th", "th", "st"))
  testthat::expect_equal(format_readable_date("2026-05-03"), "Sun, May 3rd")
  testthat::expect_equal(utc_timestamp(parse_utc_timestamp(start_utc)), start_utc)
  testthat::expect_equal(format_utc_offset(parse_utc_timestamp(winter_utc), timezone), "UTC-05:00")
  testthat::expect_equal(format_timezone_abbreviation(parse_utc_timestamp(start_utc), timezone), "EDT")
  testthat::expect_equal(format_timezone_abbreviation(parse_utc_timestamp(winter_utc), timezone), "EST")
  testthat::expect_equal(
    format_readable_option_label(start_utc, end_utc, timezone),
    "Wed, Jul 1st, 9-10:30 AM EDT"
  )
  testthat::expect_equal(format_deadline_label("2026-05-12"), "Tue, May 12, 2026")
})

testthat::test_that("readable interval helpers handle cross-day and all-day labels", {
  timezone <- "America/Toronto"
  start_utc <- "2026-07-02T03:30:00Z"
  end_utc <- "2026-07-02T05:00:00Z"

  testthat::expect_equal(
    format_readable_option_label(start_utc, end_utc, timezone),
    "Wed, Jul 1st, 11:30 PM to Thu, Jul 2nd, 1 AM EDT"
  )
  testthat::expect_equal(
    format_all_day_option_label("2026-07-01", timezone),
    "Wed, Jul 1st, 2026, All day America/Toronto"
  )
})

testthat::test_that("past datetime checks do not warn on mixed time zones", {
  local_time <- parse_local_datetime(Sys.Date() + 1L, "09:00", "America/Toronto")
  testthat::expect_warning(result <- is_past_datetime(local_time), NA)
  testthat::expect_type(result, "logical")
})

testthat::test_that("duration choices and selected slots convert to poll options", {
  timezone <- "America/Toronto"
  future_date <- as.Date(format(Sys.time(), tz = timezone, usetz = FALSE)) + 5L

  testthat::expect_equal(resolve_duration_minutes("60"), 60L)
  testthat::expect_equal(resolve_duration_minutes("all_day"), 1440L)
  testthat::expect_equal(resolve_duration_minutes("custom", 75), 75L)
  testthat::expect_error(resolve_duration_minutes("custom", 0), "between 5 and 1440")

  slots <- data.frame(
    date = as.character(c(future_date, future_date + 1L)),
    start_time = c("09:00", "all_day"),
    stringsAsFactors = FALSE
  )
  options <- selected_slots_to_options(slots, 60L, timezone)

  testthat::expect_equal(nrow(options), 2)
  testthat::expect_equal(options$option_order, c(1L, 2L))
  testthat::expect_true(any(grepl("all day", tolower(options$display_label), fixed = TRUE)))
})

testthat::test_that("batched selected slot changes preserve final selected state", {
  timezone <- "America/Toronto"
  future_date <- as.Date(format(Sys.time(), tz = timezone, usetz = FALSE)) + 5L
  first_key <- slot_key(future_date, "09:00")
  second_key <- slot_key(future_date, "10:00")

  first_batch <- data.frame(
    key = c(first_key, second_key),
    selected = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  first_result <- apply_selected_slot_changes(empty_selected_slots(), first_batch, timezone, 60L)
  testthat::expect_true(first_result$changed)
  testthat::expect_equal(nrow(first_result$slots), 2)
  testthat::expect_equal(first_result$ignored_past, 0L)

  second_batch <- data.frame(
    key = c(first_key, first_key, second_key),
    selected = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  second_result <- apply_selected_slot_changes(empty_selected_slots(), second_batch, timezone, 60L)
  testthat::expect_equal(slot_key(second_result$slots$date, second_result$slots$start_time), second_key)
})

testthat::test_that("effective response deadline is capped at the latest selected option date", {
  timezone <- "America/Toronto"
  today <- as.Date(format(Sys.time(), tz = timezone, usetz = FALSE))
  slots <- data.frame(
    date = as.character(c(today + 3L, today + 7L)),
    start_time = c("09:00", "13:00"),
    stringsAsFactors = FALSE
  )
  options <- selected_slots_to_options(slots, 60L, timezone)
  last_date <- latest_option_date(options, timezone)

  testthat::expect_equal(last_date, today + 7L)
  testthat::expect_equal(resolve_response_deadline(FALSE, "", options, timezone), as.character(today + 7L))
  testthat::expect_equal(resolve_response_deadline(TRUE, today + 4L, options, timezone), as.character(today + 4L))
  testthat::expect_equal(resolve_response_deadline(TRUE, today + 14L, options, timezone), as.character(today + 7L))
  testthat::expect_error(resolve_response_deadline(TRUE, today - 1L, options, timezone), "today or a future date")
})
