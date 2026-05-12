with_owner_env <- function(code) {
  old_owner <- Sys.getenv("APP_MAIN_OWNER_EMAIL", unset = NA_character_)
  old_secret <- Sys.getenv("ORGANIZER_AUTH_SECRET", unset = NA_character_)
  Sys.setenv(
    APP_MAIN_OWNER_EMAIL = "main.owner@example.org",
    ORGANIZER_AUTH_SECRET = "test-secret"
  )
  on.exit({
    if (is.na(old_owner)) {
      Sys.unsetenv("APP_MAIN_OWNER_EMAIL")
    } else {
      Sys.setenv(APP_MAIN_OWNER_EMAIL = old_owner)
    }
    if (is.na(old_secret)) {
      Sys.unsetenv("ORGANIZER_AUTH_SECRET")
    } else {
      Sys.setenv(ORGANIZER_AUTH_SECRET = old_secret)
    }
  }, add = TRUE)
  force(code)
}

testthat::test_that("main owner email grants main owner role", {
  with_owner_env({
    conn <- make_test_connection()
    on.exit(close_db_connection(conn), add = TRUE)

    testthat::expect_equal(get_owner_role(conn, "MAIN.OWNER@EXAMPLE.ORG"), "main_owner")
    testthat::expect_true(owner_has_workspace_access(conn, "main.owner@example.org"))
  })
})

testthat::test_that("owner access request creation sanitizes and de-duplicates", {
  with_owner_env({
    conn <- make_test_connection()
    on.exit(close_db_connection(conn), add = TRUE)

    first <- create_or_update_owner_access_request(
      conn,
      first_name = "  Secondary  ",
      last_name = " Owner ",
      email = " Secondary.Owner@Example.ORG "
    )
    second <- create_or_update_owner_access_request(
      conn,
      first_name = "Updated",
      last_name = "Person",
      email = "secondary.owner@example.org"
    )
    requests <- DBI::dbGetQuery(conn, "SELECT * FROM owner_access_requests")

    testthat::expect_equal(first$request_id[[1]], second$request_id[[1]])
    testthat::expect_equal(nrow(requests), 1)
    testthat::expect_equal(requests$first_name[[1]], "Updated")
    testthat::expect_equal(requests$email_normalized[[1]], "secondary.owner@example.org")
    testthat::expect_equal(requests$status[[1]], "pending")
    testthat::expect_equal(get_owner_role(conn, "secondary.owner@example.org"), "pending")
  })
})

testthat::test_that("verified owner request sends mocked main-owner notification", {
  with_owner_env({
    conn <- make_test_connection()
    on.exit(close_db_connection(conn), add = TRUE)
    sent <- list(to = NULL, request = NULL)
    old_sender <- getOption("meeting_poll.owner_request_sender", NULL)
    options(meeting_poll.owner_request_sender = function(to, request) {
      sent$to <<- to
      sent$request <<- request
    })
    on.exit(options(meeting_poll.owner_request_sender = old_sender), add = TRUE)

    login <- create_organizer_login_code(conn, "requester@example.org", code = "445566")
    testthat::expect_true(verify_organizer_login_code(conn, login$email, "445566"))
    request <- create_or_update_owner_access_request(conn, "Request", "Person", login$email)
    result <- send_owner_access_request_email(request)

    testthat::expect_true(result$sent)
    testthat::expect_equal(sent$to, "main.owner@example.org")
    testthat::expect_equal(sent$request$email, "requester@example.org")
  })
})

testthat::test_that("main owner can approve deny and revoke secondary owners", {
  with_owner_env({
    conn <- make_test_connection()
    on.exit(close_db_connection(conn), add = TRUE)

    approved_request <- create_or_update_owner_access_request(conn, "Approved", "Owner", "approved@example.org")
    approve_owner_request(conn, approved_request$request_id[[1]], "main.owner@example.org")

    testthat::expect_equal(get_owner_role(conn, "approved@example.org"), "owner")
    testthat::expect_true(owner_has_workspace_access(conn, "approved@example.org"))
    approved <- list_approved_owners(conn, "main.owner@example.org")
    testthat::expect_equal(nrow(approved), 1)

    denied_request <- create_or_update_owner_access_request(conn, "Denied", "Owner", "denied@example.org")
    deny_owner_request(conn, denied_request$request_id[[1]], "main.owner@example.org")
    testthat::expect_equal(get_owner_role(conn, "denied@example.org"), "denied")
    testthat::expect_false(owner_has_workspace_access(conn, "denied@example.org"))

    revoke_approved_owner(conn, approved$owner_id[[1]], "main.owner@example.org")
    testthat::expect_equal(get_owner_role(conn, "approved@example.org"), "revoked")
    testthat::expect_false(owner_has_workspace_access(conn, "approved@example.org"))
  })
})

testthat::test_that("non-main organizers cannot review owner access", {
  with_owner_env({
    conn <- make_test_connection()
    on.exit(close_db_connection(conn), add = TRUE)
    request <- create_or_update_owner_access_request(conn, "Secondary", "Owner", "secondary@example.org")

    testthat::expect_error(
      list_owner_access_requests(conn, "secondary@example.org"),
      "Only the main owner"
    )
    testthat::expect_error(
      approve_owner_request(conn, request$request_id[[1]], "secondary@example.org"),
      "Only the main owner"
    )
  })
})

testthat::test_that("approved owners cannot see other organizer polls", {
  with_owner_env({
    conn <- make_test_connection()
    on.exit(close_db_connection(conn), add = TRUE)
    request <- create_or_update_owner_access_request(conn, "Secondary", "Owner", "secondary@example.org")
    approve_owner_request(conn, request$request_id[[1]], "main.owner@example.org")
    make_sample_poll(conn)

    hidden_polls <- list_polls_for_organizer(conn, "secondary@example.org")
    owner_polls <- list_polls_for_organizer(conn, "organizer@example.org")

    testthat::expect_equal(nrow(hidden_polls), 0)
    testthat::expect_equal(nrow(owner_polls), 1)
  })
})
