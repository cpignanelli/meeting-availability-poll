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
  testthat::expect_true(grepl("Preferred", html, fixed = TRUE))
  testthat::expect_true(grepl("Available", html, fixed = TRUE))
  testthat::expect_true(grepl("Unavailable", html, fixed = TRUE))
})
