testthat::test_that("database initialization creates required tables idempotently", {
  db_path <- tempfile(fileext = ".sqlite")
  initialize_database(db_path)
  initialize_database(db_path)
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  tables <- DBI::dbListTables(conn)
  testthat::expect_true(all(c(
    "polls",
    "poll_options",
    "expected_participants",
    "participants",
    "responses",
    "finalized_meetings",
    "audit_log"
  ) %in% tables))
})

testthat::test_that("local connection can be opened and closed", {
  conn <- make_test_connection()
  testthat::expect_true(DBI::dbIsValid(conn))
  close_db_connection(conn)
})
