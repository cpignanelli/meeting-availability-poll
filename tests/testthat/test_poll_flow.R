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
  testthat::expect_equal(admin_poll$organizer_email_normalized[[1]], "organizer@example.org")
  testthat::expect_false("admin_token" %in% names(response_poll))
})

testthat::test_that("organizer portal queries are constrained by organizer email", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  visible_polls <- list_polls_for_organizer(conn, "Organizer@Example.ORG")
  hidden_polls <- list_polls_for_organizer(conn, "other@example.org")
  visible_poll <- get_poll_for_organizer(conn, result$poll_id, "organizer@example.org")
  hidden_poll <- get_poll_for_organizer(conn, result$poll_id, "other@example.org")

  testthat::expect_equal(nrow(visible_polls), 1)
  testthat::expect_equal(nrow(hidden_polls), 0)
  testthat::expect_equal(visible_poll$poll_id[[1]], result$poll_id)
  testthat::expect_null(hidden_poll)
})

testthat::test_that("organizer magic code verification works and prevents reuse", {
  old_secret <- Sys.getenv("ORGANIZER_AUTH_SECRET", unset = NA_character_)
  Sys.setenv(ORGANIZER_AUTH_SECRET = "test-secret")
  on.exit({
    if (is.na(old_secret)) {
      Sys.unsetenv("ORGANIZER_AUTH_SECRET")
    } else {
      Sys.setenv(ORGANIZER_AUTH_SECRET = old_secret)
    }
  }, add = TRUE)

  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  login <- create_organizer_login_code(conn, "Organizer@Example.ORG", code = "123456")

  testthat::expect_equal(login$email, "organizer@example.org")
  testthat::expect_true(verify_organizer_login_code(conn, "organizer@example.org", "123456"))
  testthat::expect_false(verify_organizer_login_code(conn, "organizer@example.org", "123456"))
})

testthat::test_that("organizer magic code rejects expired and over-attempted codes", {
  old_secret <- Sys.getenv("ORGANIZER_AUTH_SECRET", unset = NA_character_)
  Sys.setenv(ORGANIZER_AUTH_SECRET = "test-secret")
  on.exit({
    if (is.na(old_secret)) {
      Sys.unsetenv("ORGANIZER_AUTH_SECRET")
    } else {
      Sys.setenv(ORGANIZER_AUTH_SECRET = old_secret)
    }
  }, add = TRUE)

  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  create_organizer_login_code(conn, "organizer@example.org", code = "222222")
  DBI::dbExecute(
    conn,
    "UPDATE organizer_login_codes SET expires_at = ?",
    params = list(as_utc_string(Sys.time() - 60))
  )
  testthat::expect_false(verify_organizer_login_code(conn, "organizer@example.org", "222222"))

  create_organizer_login_code(conn, "organizer@example.org", code = "333333")
  for (i in seq_len(magic_code_max_attempts())) {
    testthat::expect_false(verify_organizer_login_code(conn, "organizer@example.org", "000000"))
  }
  testthat::expect_false(verify_organizer_login_code(conn, "organizer@example.org", "333333"))
})

testthat::test_that("trusted session tokens are scoped and expire", {
  old_secret <- Sys.getenv("APP_AUTH_SECRET", unset = NA_character_)
  Sys.setenv(APP_AUTH_SECRET = "test-session-secret")
  on.exit({
    if (is.na(old_secret)) {
      Sys.unsetenv("APP_AUTH_SECRET")
    } else {
      Sys.setenv(APP_AUTH_SECRET = old_secret)
    }
  }, add = TRUE)

  token <- issue_trusted_session_token("participant", "Person@Example.ORG", poll_id = 12L, minutes = 10L)
  verified <- verify_trusted_session_token(token, expected_scope = "participant", poll_id = 12L)

  testthat::expect_true(verified$valid)
  testthat::expect_equal(verified$email, "person@example.org")
  testthat::expect_false(verify_trusted_session_token(token, expected_scope = "participant", poll_id = 13L)$valid)
  testthat::expect_false(verify_trusted_session_token(sub(".$", "0", token), expected_scope = "participant", poll_id = 12L)$valid)
})

testthat::test_that("magic code email can be mocked and dev fallback is explicit", {
  sent <- list(email = NULL, code = NULL)
  old_sender <- getOption("meeting_poll.magic_code_sender", NULL)
  options(meeting_poll.magic_code_sender = function(email, code) {
    sent$email <<- email
    sent$code <<- code
  })
  on.exit(options(meeting_poll.magic_code_sender = old_sender), add = TRUE)

  result <- send_organizer_magic_code_email("Organizer@Example.ORG", "123456")
  testthat::expect_true(result$sent)
  testthat::expect_equal(sent$email, "organizer@example.org")
  testthat::expect_equal(sent$code, "123456")
})

testthat::test_that("participant magic code verification works and prevents reuse", {
  old_secret <- Sys.getenv("APP_AUTH_SECRET", unset = NA_character_)
  Sys.setenv(APP_AUTH_SECRET = "test-participant-secret")
  on.exit({
    if (is.na(old_secret)) {
      Sys.unsetenv("APP_AUTH_SECRET")
    } else {
      Sys.setenv(APP_AUTH_SECRET = old_secret)
    }
  }, add = TRUE)

  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  login <- create_participant_login_code(conn, result$poll_id, "Participant@Example.ORG", code = "456789")

  testthat::expect_equal(login$email, "participant@example.org")
  testthat::expect_true(verify_participant_login_code(conn, result$poll_id, "participant@example.org", "456789"))
  testthat::expect_false(verify_participant_login_code(conn, result$poll_id, "participant@example.org", "456789"))
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

testthat::test_that("participant-visible poll data excludes emails comments and anonymous legacy rows", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  poll <- get_poll_by_response_token(conn, result$response_token)
  options <- get_poll_options(conn, poll$poll_id[[1]])
  response_values <- data.frame(
    option_id = options$option_id,
    availability = c("preferred", "available"),
    stringsAsFactors = FALSE
  )

  submit_poll_response(
    conn,
    poll$poll_id[[1]],
    list(name = "Verified Person", email = "verified@example.org", organization = ""),
    response_values,
    "Organizer-only comment"
  )
  submit_poll_response(
    conn,
    poll$poll_id[[1]],
    list(name = "Anonymous Legacy", email = "", organization = ""),
    response_values,
    ""
  )

  visible <- get_participant_visible_poll_data(conn, poll$poll_id[[1]])

  testthat::expect_equal(nrow(visible$participants), 1)
  testthat::expect_equal(visible$participants$name[[1]], "Verified Person")
  testthat::expect_false("email" %in% names(visible$participants))
  testthat::expect_false("comment" %in% names(visible$responses))
  testthat::expect_equal(nrow(visible$responses), 2)
})

testthat::test_that("participant response can be submitted without email", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  poll <- get_poll_by_response_token(conn, result$response_token)
  options <- get_poll_options(conn, poll$poll_id[[1]])
  response_values <- data.frame(
    option_id = options$option_id,
    availability = c("preferred", "available"),
    stringsAsFactors = FALSE
  )

  submit_poll_response(
    conn,
    poll$poll_id[[1]],
    list(name = "No Email Participant", email = NA_character_, organization = ""),
    response_values,
    ""
  )
  submit_poll_response(
    conn,
    poll$poll_id[[1]],
    list(name = "No Email Participant", email = "", organization = ""),
    response_values,
    ""
  )

  participants <- get_participants(conn, poll$poll_id[[1]])
  responses <- get_responses_for_poll(conn, poll$poll_id[[1]])
  testthat::expect_equal(nrow(participants), 2)
  testthat::expect_true(all(is.na(participants$email)))
  testthat::expect_equal(nrow(responses), 4)
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

testthat::test_that("closed and expired polls can be reopened unless finalized", {
  conn <- make_test_connection()
  on.exit(close_db_connection(conn), add = TRUE)
  result <- make_sample_poll(conn)
  poll <- get_poll_by_response_token(conn, result$response_token)
  today <- as.Date(format(Sys.time(), tz = "America/Toronto", usetz = FALSE))

  close_poll(conn, poll$poll_id[[1]])
  reopened_deadline <- as.character(today + 7L)
  reopen_poll(conn, poll$poll_id[[1]], reopened_deadline)
  reopened <- get_poll_dashboard_data(conn, poll$poll_id[[1]])
  testthat::expect_equal(reopened$poll$status[[1]], "open")
  testthat::expect_equal(reopened$poll$response_deadline[[1]], reopened_deadline)

  DBI::dbExecute(
    conn,
    "UPDATE polls SET response_deadline = ? WHERE poll_id = ?",
    params = list(as.character(today - 1L), poll$poll_id[[1]])
  )
  expired <- get_poll_dashboard_data(conn, poll$poll_id[[1]])
  testthat::expect_equal(poll_display_status(expired$poll), "expired")

  reopen_poll(conn, poll$poll_id[[1]], "")
  no_deadline <- get_poll_dashboard_data(conn, poll$poll_id[[1]])
  testthat::expect_equal(no_deadline$poll$status[[1]], "open")
  testthat::expect_equal(no_deadline$poll$response_deadline[[1]], "")
  testthat::expect_equal(poll_display_status(no_deadline$poll), "open")

  finalize_meeting(conn, poll$poll_id[[1]], no_deadline$options$option_id[[1]], "")
  testthat::expect_error(reopen_poll(conn, poll$poll_id[[1]], ""), "Finalized")
})
