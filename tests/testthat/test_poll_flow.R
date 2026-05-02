testthat::test_that("poll creation stores hash and response token lookup works", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  testthat::expect_match(result$admin_token, "^[a-f0-9]{64}$")
  testthat::expect_match(result$response_token, "^[a-f0-9]{64}$")

  response_poll <- get_poll_by_response_token(conn, result$response_token)
  admin_poll <- get_poll_by_admin_token(conn, result$admin_token)
  testthat::expect_equal(response_poll$poll_id, result$poll_id)
  testthat::expect_equal(admin_poll$poll_id, result$poll_id)
  testthat::expect_false("admin_token" %in% names(response_poll))
})

testthat::test_that("participant response can be submitted and updated", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  poll <- get_poll_by_response_token(conn, result$response_token)
  options <- get_poll_options(conn, poll$poll_id[[1]])

  participant <- list(name = "Required Person", email = "required@example.org", organization = "Org")
  response_values <- data.frame(
    option_id = options$option_id,
    availability = c("preferred", "unavailable"),
    stringsAsFactors = FALSE
  )
  submit_poll_response(conn, poll$poll_id[[1]], participant, response_values, "Initial")
  response_values$availability <- c("available", "available")
  submit_poll_response(conn, poll$poll_id[[1]], participant, response_values, "Updated")

  participants <- get_participants(conn, poll$poll_id[[1]])
  responses <- get_responses_for_poll(conn, poll$poll_id[[1]])
  testthat::expect_equal(nrow(participants), 1)
  testthat::expect_equal(nrow(responses), 2)
  testthat::expect_true(all(responses$availability == "available"))
})

testthat::test_that("ranking and finalization work", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  poll <- get_poll_by_response_token(conn, result$response_token)
  options <- get_poll_options(conn, poll$poll_id[[1]])
  submit_poll_response(
    conn,
    poll$poll_id[[1]],
    list(name = "Required Person", email = "required@example.org", organization = "Org"),
    data.frame(option_id = options$option_id, availability = c("preferred", "available"), stringsAsFactors = FALSE),
    ""
  )

  data <- get_poll_dashboard_data(conn, poll$poll_id[[1]])
  testthat::expect_equal(data$ranked$availability_score[[1]], 2L)
  finalize_meeting(conn, poll$poll_id[[1]], data$ranked$option_id[[1]], "Final notes")
  finalized <- get_finalized_meeting(conn, poll$poll_id[[1]])
  refreshed <- get_poll_dashboard_data(conn, poll$poll_id[[1]])
  testthat::expect_equal(finalized$selected_option_id[[1]], data$ranked$option_id[[1]])
  testthat::expect_equal(refreshed$poll$status[[1]], "finalized")
})
