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
    "audit_log",
    "organizer_login_codes",
    "participant_login_codes",
    "owner_access_requests",
    "approved_owners"
  ) %in% tables))
  poll_columns <- DBI::dbGetQuery(conn, "PRAGMA table_info(polls)")
  testthat::expect_true("organizer_email_normalized" %in% poll_columns$name)
  participant_columns <- DBI::dbGetQuery(conn, "PRAGMA table_info(participants)")
  email_column <- participant_columns[participant_columns$name == "email", , drop = FALSE]
  testthat::expect_equal(as.integer(email_column$notnull[[1]]), 0L)
})

testthat::test_that("database initialization migrates organizer normalized email", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(conn, "CREATE TABLE polls (
    poll_id INTEGER PRIMARY KEY AUTOINCREMENT,
    admin_token_hash TEXT NOT NULL UNIQUE,
    response_token TEXT NOT NULL UNIQUE,
    title TEXT NOT NULL,
    organizer_name TEXT NOT NULL,
    organizer_email TEXT NOT NULL,
    duration_minutes INTEGER NOT NULL,
    timezone TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'open',
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL
  )")
  DBI::dbExecute(
    conn,
    "INSERT INTO polls (
      admin_token_hash, response_token, title, organizer_name, organizer_email,
      duration_minutes, timezone, status, created_at, updated_at
    ) VALUES ('hash', 'response', 'Title', 'Organizer', ' Organizer@Example.ORG ', 60, 'America/Toronto', 'open', '2026-01-01T00:00:00Z', '2026-01-01T00:00:00Z')"
  )
  DBI::dbDisconnect(conn)

  initialize_database(db_path)
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  columns <- DBI::dbGetQuery(conn, "PRAGMA table_info(polls)")
  poll <- DBI::dbGetQuery(conn, "SELECT organizer_email_normalized FROM polls")

  testthat::expect_true("organizer_email_normalized" %in% columns$name)
  testthat::expect_equal(poll$organizer_email_normalized[[1]], "organizer@example.org")
})

testthat::test_that("local connection can be opened and closed", {
  conn <- make_test_connection()
  testthat::expect_true(DBI::dbIsValid(conn))
  close_db_connection(conn)
})

testthat::test_that("database initialization migrates participant email to nullable", {
  db_path <- tempfile(fileext = ".sqlite")
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "CREATE TABLE participants (
    participant_id INTEGER PRIMARY KEY AUTOINCREMENT,
    poll_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    organization TEXT,
    submitted_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    UNIQUE (poll_id, email)
  )")
  DBI::dbExecute(conn, "CREATE TABLE responses (
    response_id INTEGER PRIMARY KEY AUTOINCREMENT,
    participant_id INTEGER NOT NULL,
    option_id INTEGER NOT NULL,
    availability TEXT NOT NULL CHECK (availability IN ('preferred', 'available', 'unavailable')),
    comment TEXT,
    UNIQUE (participant_id, option_id)
  )")
  DBI::dbExecute(
    conn,
    "INSERT INTO participants (poll_id, name, email, organization, submitted_at, updated_at)
     VALUES (1, 'Existing Person', 'existing@example.org', 'Org', '2026-01-01T00:00:00Z', '2026-01-01T00:00:00Z')"
  )
  DBI::dbExecute(
    conn,
    "INSERT INTO responses (participant_id, option_id, availability, comment)
     VALUES (1, 1, 'preferred', 'Keep this response')"
  )
  DBI::dbDisconnect(conn)

  initialize_database(db_path)
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  participant_columns <- DBI::dbGetQuery(conn, "PRAGMA table_info(participants)")
  email_column <- participant_columns[participant_columns$name == "email", , drop = FALSE]
  participants <- DBI::dbGetQuery(conn, "SELECT * FROM participants")
  responses <- DBI::dbGetQuery(conn, "SELECT * FROM responses")

  testthat::expect_equal(as.integer(email_column$notnull[[1]]), 0L)
  testthat::expect_equal(nrow(participants), 1)
  testthat::expect_equal(nrow(responses), 1)
  testthat::expect_equal(responses$comment[[1]], "Keep this response")
})
