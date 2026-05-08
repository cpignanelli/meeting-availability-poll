testthat::test_that("creation route requires a configured creation secret", {
  testthat::expect_false(can_create_poll(list(), creation_secret = ""))
  testthat::expect_true(can_create_poll(list(create = "secret-value"), creation_secret = "secret-value"))
  testthat::expect_false(can_create_poll(list(), creation_secret = "secret-value"))
  testthat::expect_false(can_create_poll(list(create = "wrong"), creation_secret = "secret-value"))
})

testthat::test_that("availability helper labels remain stable", {
  testthat::expect_equal(availability_short_label("preferred"), "Preferred")
  testthat::expect_equal(availability_short_label("available"), "Available")
  testthat::expect_equal(availability_short_label("unavailable"), "Unavailable")
  testthat::expect_equal(availability_short_label("missing"), "Missing")
  testthat::expect_equal(availability_hint("preferred"), "Can attend; works especially well")
})

testthat::test_that("response availability choices keep submitted values stable", {
  choices <- response_availability_choices()
  testthat::expect_equal(unname(choices), c("preferred", "available", "unavailable"))
  testthat::expect_equal(names(choices), c("Preferred", "Available", "Unavailable"))
  testthat::expect_equal(response_availability_cycle(), c("pending", "available", "preferred", "unavailable"))
  testthat::expect_equal(availability_icon("preferred"), "★")
})

testthat::test_that("duration labels use human-readable units", {
  testthat::expect_equal(format_duration_label(15), "15 minutes")
  testthat::expect_equal(format_duration_label(60), "1 hour")
  testthat::expect_equal(format_duration_label(90), "1 hour 30 minutes")
  testthat::expect_equal(format_duration_label(120), "2 hours")
  testthat::expect_equal(format_duration_label(1440), "All day")
})

testthat::test_that("poll display status includes expired open links", {
  today <- as.Date(format(Sys.time(), tz = "America/Toronto", usetz = FALSE))
  base_poll <- data.frame(
    status = "open",
    response_deadline = as.character(today),
    timezone = "America/Toronto",
    organizer_name = "Organizer",
    organizer_email = "organizer@example.org",
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(poll_display_status(base_poll), "open")

  past_options <- selected_slots_to_options(
    data.frame(date = as.character(today - 2L), start_time = "09:00", stringsAsFactors = FALSE),
    60L,
    "America/Toronto"
  )
  no_deadline_poll <- base_poll
  no_deadline_poll$response_deadline <- ""
  testthat::expect_equal(poll_display_status(no_deadline_poll, past_options), "expired")

  expired_poll <- base_poll
  expired_poll$response_deadline <- as.character(today - 1L)
  testthat::expect_equal(poll_display_status(expired_poll), "expired")

  closed_poll <- base_poll
  closed_poll$status <- "closed"
  testthat::expect_equal(poll_display_status(closed_poll), "closed")

  finalized_poll <- base_poll
  finalized_poll$status <- "finalized"
  testthat::expect_equal(poll_display_status(finalized_poll), "finalized")
})

testthat::test_that("closed contact message contains organizer contact without tokens", {
  poll <- data.frame(
    organizer_name = "Organizer Name",
    organizer_email = "organizer@example.org",
    stringsAsFactors = FALSE
  )
  message <- closed_poll_contact_message(poll)
  testthat::expect_match(message, "Organizer Name")
  testthat::expect_match(message, "organizer@example.org")
  testthat::expect_false(grepl("admin", message, ignore.case = TRUE))
  testthat::expect_false(grepl("token", message, ignore.case = TRUE))

  finalized_message <- finalized_poll_contact_message(poll)
  testthat::expect_match(finalized_message, "finalized")
  testthat::expect_match(finalized_message, "organizer@example.org")
})
