source(file.path(project_root, "R/modules/mod_respond_poll.R"), local = TRUE)

testthat::test_that("response calendar data groups options by local week and keeps availability ids", {
  timezone <- "America/Toronto"
  start_one <- parse_local_datetime(Sys.Date() + 3L, "09:00", timezone)
  start_two <- parse_local_datetime(Sys.Date() + 10L, "10:30", timezone)
  options <- data.frame(
    option_id = c(101L, 102L),
    poll_id = c(1L, 1L),
    start_datetime = c(as_utc_string(start_one), as_utc_string(start_two)),
    end_datetime = c(as_utc_string(add_minutes(start_one, 60)), as_utc_string(add_minutes(start_two, 60))),
    display_label = c("Option 1", "Option 2"),
    option_order = c(1L, 2L),
    stringsAsFactors = FALSE
  )

  calendar <- response_calendar_data(options, timezone)

  testthat::expect_equal(nrow(calendar), 2)
  testthat::expect_true(all(c("week_key", "local_date", "local_time", "timezone_label") %in% names(calendar)))
  testthat::expect_equal(calendar$option_id, c(101L, 102L))
  testthat::expect_match(default_response_week(calendar), "^week_")
})

testthat::test_that("response calendar can render a viewer-local timezone", {
  start_toronto <- parse_local_datetime(as.Date("2026-05-15"), "09:00", "America/Toronto")
  options <- data.frame(
    option_id = 101L,
    poll_id = 1L,
    start_datetime = as_utc_string(start_toronto),
    end_datetime = as_utc_string(add_minutes(start_toronto, 60)),
    display_label = "Option 1",
    option_order = 1L,
    stringsAsFactors = FALSE
  )

  calendar <- response_calendar_data(options, "America/Vancouver")

  testthat::expect_equal(calendar$local_time[[1]], "06:00")
  testthat::expect_equal(calendar$timezone_label[[1]], "PDT")
  testthat::expect_match(response_board_time_label(calendar[1, , drop = FALSE], "America/Vancouver"), "6")
})

testthat::test_that("response calendar renders one input per proposed option", {
  timezone <- "America/Toronto"
  start_one <- parse_local_datetime(Sys.Date() + 3L, "09:00", timezone)
  options <- data.frame(
    option_id = 101L,
    poll_id = 1L,
    start_datetime = as_utc_string(start_one),
    end_datetime = as_utc_string(add_minutes(start_one, 60)),
    display_label = "Option 1",
    option_order = 1L,
    stringsAsFactors = FALSE
  )

  html <- as.character(build_response_calendar_ui(shiny::NS("respond"), options, timezone))

  testthat::expect_true(grepl("respond-availability_101", html, fixed = TRUE))
  testthat::expect_true(grepl("data-availability-cycle", html, fixed = TRUE))
  testthat::expect_true(grepl("Pending", html, fixed = TRUE))
  testthat::expect_true(grepl("Preferred", html, fixed = TRUE))
  testthat::expect_true(grepl("Available", html, fixed = TRUE))
  testthat::expect_true(grepl("Unavailable", html, fixed = TRUE))
})

testthat::test_that("response calendar includes read-only participant rows without private fields", {
  timezone <- "America/Toronto"
  start_one <- parse_local_datetime(Sys.Date() + 3L, "09:00", timezone)
  options <- data.frame(
    option_id = 101L,
    poll_id = 1L,
    start_datetime = as_utc_string(start_one),
    end_datetime = as_utc_string(add_minutes(start_one, 60)),
    display_label = "Option 1",
    option_order = 1L,
    stringsAsFactors = FALSE
  )
  visible <- list(
    participants = data.frame(participant_id = 2L, name = "Alex Lee", submitted_at = "", updated_at = "", stringsAsFactors = FALSE),
    responses = data.frame(participant_id = 2L, option_id = 101L, availability = "available", stringsAsFactors = FALSE)
  )

  html <- as.character(build_response_calendar_ui(shiny::NS("respond"), options, timezone, visible_data = visible))

  testthat::expect_true(grepl("Alex Lee", html, fixed = TRUE))
  testthat::expect_true(grepl("availability-cycle-readonly", html, fixed = TRUE))
  testthat::expect_false(grepl("@", html, fixed = TRUE))
})

testthat::test_that("response board helpers summarize timed and all-day options", {
  timezone <- "America/Toronto"
  start_one <- parse_local_datetime(Sys.Date() + 3L, "09:00", timezone)
  timed_option <- data.frame(
    option_id = 101L,
    poll_id = 1L,
    start_datetime = as_utc_string(start_one),
    end_datetime = as_utc_string(add_minutes(start_one, 90)),
    display_label = "Option 1",
    option_order = 1L,
    stringsAsFactors = FALSE
  )
  all_day_option <- timed_option
  all_day_option$end_datetime <- as_utc_string(add_minutes(start_one, 1440))
  all_day_option$display_label <- "All day option"

  testthat::expect_match(response_board_time_label(timed_option, timezone), "9")
  testthat::expect_equal(response_board_duration_label(timed_option), "1 hour 30 minutes")
  testthat::expect_equal(response_board_time_label(all_day_option, timezone), "All day")
  testthat::expect_equal(response_board_duration_label(all_day_option), "All day")
})
