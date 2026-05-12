source(file.path(project_root, "R/modules/mod_finalize_poll.R"), local = TRUE)
source(file.path(project_root, "R/modules/mod_admin_dashboard.R"), local = TRUE)

testthat::test_that("email normalization supports vector comparisons", {
  emails <- normalize_email_vector(c(" First@Example.ORG ", NA, "SECOND@example.org"))

  testthat::expect_equal(emails, c("first@example.org", "", "second@example.org"))
})

testthat::test_that("dashboard data tolerates polls with options and no responses", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)

  data <- get_poll_dashboard_data(conn, result$poll_id)

  testthat::expect_equal(nrow(data$options), 2)
  testthat::expect_equal(nrow(data$participants), 0)
  testthat::expect_equal(nrow(data$ranked), 2)
  testthat::expect_true(all(data$ranked$availability_score == 0L))
  testthat::expect_equal(nrow(data$heatmap), 0)
})

testthat::test_that("dashboard data tolerates blank participant emails", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  options <- get_poll_options(conn, result$poll_id)

  submit_poll_response(
    conn,
    result$poll_id,
    list(name = "Participant Without Email", email = "", organization = ""),
    data.frame(option_id = options$option_id, availability = c("preferred", "available"), stringsAsFactors = FALSE),
    ""
  )

  data <- get_poll_dashboard_data(conn, result$poll_id)
  response_table <- format_responses_table(data$responses, data$poll$timezone[[1]])

  testthat::expect_equal(nrow(data$participants), 1)
  testthat::expect_equal(data$ranked$availability_score[[1]], 2L)
  testthat::expect_true("Not provided" %in% response_table$Email)
})

testthat::test_that("expected participant matching handles multiple emails independently", {
  timezone <- "America/Toronto"
  start <- as.POSIXct("2026-05-15 09:00:00", tz = timezone)
  options <- data.frame(
    option_id = 1L,
    start_datetime = as_utc_string(start),
    end_datetime = as_utc_string(add_minutes(start, 60)),
    display_label = format_option_label(as_utc_string(start), as_utc_string(add_minutes(start, 60)), timezone),
    stringsAsFactors = FALSE
  )
  participants <- data.frame(
    participant_id = 1L,
    name = "First Person",
    email = "first@example.org",
    stringsAsFactors = FALSE
  )
  responses <- data.frame(
    participant_id = 1L,
    option_id = 1L,
    availability = "available",
    stringsAsFactors = FALSE
  )
  expected <- data.frame(
    name = c("First Person", "Second Person"),
    email = c("first@example.org", "second@example.org"),
    organization = c("", ""),
    is_required = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  ranked <- rank_time_options(options, responses, participants, expected, timezone)
  missing <- find_missing_expected_participants(expected, participants)

  testthat::expect_equal(ranked$required_attendee_conflicts[[1]], 1L)
  testthat::expect_equal(missing$email, "second@example.org")
})

testthat::test_that("dashboard formatters return stable empty tables", {
  ranked_table <- format_ranked_table(empty_ranked_options())
  response_table <- format_responses_table(data.frame(), "America/Toronto")

  testthat::expect_named(
    ranked_table,
    c("Time option", "Time zone", "UTC start", "UTC end", "Preferred", "Available", "Unavailable", "Missing", "Score", "Required conflicts")
  )
  testthat::expect_named(
    response_table,
    c("Name", "Email", "Time option", "Time zone", "UTC start", "UTC end", "Availability", "Comment")
  )
  testthat::expect_equal(nrow(ranked_table), 0)
  testthat::expect_equal(nrow(response_table), 0)
})

testthat::test_that("dashboard data tolerates legacy polls without options", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  timezone <- "America/Toronto"
  result <- create_poll_record(
    conn,
    list(
      title = "Legacy empty poll",
      description = "",
      organizer_name = "Organizer",
      organizer_email = "organizer@example.org",
      duration_minutes = 60L,
      timezone = timezone,
      location_type = "To be determined",
      location_details = "",
      response_deadline = ""
    ),
    data.frame(),
    data.frame()
  )

  data <- get_poll_dashboard_data(conn, result$poll_id)

  testthat::expect_equal(nrow(data$options), 0)
  testthat::expect_equal(nrow(data$ranked), 0)
  testthat::expect_equal(nrow(data$heatmap), 0)
  testthat::expect_named(data$ranked, names(empty_ranked_options()))
})

