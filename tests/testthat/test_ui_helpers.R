testthat::test_that("creation route respects optional creation secret", {
  testthat::expect_true(can_create_poll(list(), creation_secret = ""))
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
