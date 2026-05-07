schema_statements <- function() {
  c(
    "PRAGMA foreign_keys = ON",
    "CREATE TABLE IF NOT EXISTS polls (
      poll_id INTEGER PRIMARY KEY AUTOINCREMENT,
      admin_token_hash TEXT NOT NULL UNIQUE,
      response_token TEXT NOT NULL UNIQUE,
      title TEXT NOT NULL,
      description TEXT,
      organizer_name TEXT NOT NULL,
      organizer_email TEXT NOT NULL,
      organizer_email_normalized TEXT,
      duration_minutes INTEGER NOT NULL,
      timezone TEXT NOT NULL,
      location_type TEXT,
      location_details TEXT,
      response_deadline TEXT,
      status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'closed', 'finalized')),
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      closed_at TEXT
    )",
    "CREATE TABLE IF NOT EXISTS poll_options (
      option_id INTEGER PRIMARY KEY AUTOINCREMENT,
      poll_id INTEGER NOT NULL,
      start_datetime TEXT NOT NULL,
      end_datetime TEXT NOT NULL,
      display_label TEXT NOT NULL,
      option_order INTEGER NOT NULL,
      FOREIGN KEY (poll_id) REFERENCES polls(poll_id) ON DELETE CASCADE
    )",
    "CREATE TABLE IF NOT EXISTS expected_participants (
      expected_participant_id INTEGER PRIMARY KEY AUTOINCREMENT,
      poll_id INTEGER NOT NULL,
      name TEXT NOT NULL,
      email TEXT NOT NULL,
      organization TEXT,
      is_required INTEGER NOT NULL DEFAULT 0 CHECK (is_required IN (0, 1)),
      FOREIGN KEY (poll_id) REFERENCES polls(poll_id) ON DELETE CASCADE,
      UNIQUE (poll_id, email)
    )",
    "CREATE TABLE IF NOT EXISTS participants (
      participant_id INTEGER PRIMARY KEY AUTOINCREMENT,
      poll_id INTEGER NOT NULL,
      name TEXT NOT NULL,
      email TEXT,
      organization TEXT,
      submitted_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (poll_id) REFERENCES polls(poll_id) ON DELETE CASCADE,
      UNIQUE (poll_id, email)
    )",
    "CREATE TABLE IF NOT EXISTS responses (
      response_id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id INTEGER NOT NULL,
      option_id INTEGER NOT NULL,
      availability TEXT NOT NULL CHECK (availability IN ('preferred', 'available', 'unavailable')),
      comment TEXT,
      FOREIGN KEY (participant_id) REFERENCES participants(participant_id) ON DELETE CASCADE,
      FOREIGN KEY (option_id) REFERENCES poll_options(option_id) ON DELETE CASCADE,
      UNIQUE (participant_id, option_id)
    )",
    "CREATE TABLE IF NOT EXISTS finalized_meetings (
      final_id INTEGER PRIMARY KEY AUTOINCREMENT,
      poll_id INTEGER NOT NULL UNIQUE,
      selected_option_id INTEGER NOT NULL,
      final_notes TEXT,
      finalized_at TEXT NOT NULL,
      FOREIGN KEY (poll_id) REFERENCES polls(poll_id) ON DELETE CASCADE,
      FOREIGN KEY (selected_option_id) REFERENCES poll_options(option_id) ON DELETE RESTRICT
    )",
    "CREATE TABLE IF NOT EXISTS audit_log (
      audit_id INTEGER PRIMARY KEY AUTOINCREMENT,
      poll_id INTEGER,
      event_type TEXT NOT NULL,
      event_detail TEXT,
      created_at TEXT NOT NULL,
      FOREIGN KEY (poll_id) REFERENCES polls(poll_id) ON DELETE SET NULL
    )",
    "CREATE TABLE IF NOT EXISTS organizer_login_codes (
      login_code_id INTEGER PRIMARY KEY AUTOINCREMENT,
      organizer_email_normalized TEXT NOT NULL,
      code_hash TEXT NOT NULL,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      used_at TEXT,
      attempts INTEGER NOT NULL DEFAULT 0
    )",
    "CREATE INDEX IF NOT EXISTS idx_polls_response_token ON polls(response_token)",
    "CREATE INDEX IF NOT EXISTS idx_polls_admin_token_hash ON polls(admin_token_hash)",
    "CREATE INDEX IF NOT EXISTS idx_polls_organizer_email_normalized ON polls(organizer_email_normalized)",
    "CREATE INDEX IF NOT EXISTS idx_poll_options_poll_id ON poll_options(poll_id)",
    "CREATE INDEX IF NOT EXISTS idx_expected_poll_id ON expected_participants(poll_id)",
    "CREATE INDEX IF NOT EXISTS idx_participants_poll_id ON participants(poll_id)",
    "CREATE INDEX IF NOT EXISTS idx_responses_participant_id ON responses(participant_id)",
    "CREATE INDEX IF NOT EXISTS idx_responses_option_id ON responses(option_id)",
    "CREATE INDEX IF NOT EXISTS idx_audit_poll_id ON audit_log(poll_id)",
    "CREATE INDEX IF NOT EXISTS idx_login_codes_email_created ON organizer_login_codes(organizer_email_normalized, created_at)"
  )
}

run_schema_migrations <- function(conn) {
  migrate_participants_email_nullable(conn)
  migrate_poll_organizer_email_normalized(conn)
  invisible(TRUE)
}

migrate_poll_organizer_email_normalized <- function(conn) {
  table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(polls)")
  if (!"organizer_email_normalized" %in% table_info$name) {
    DBI::dbExecute(conn, "ALTER TABLE polls ADD COLUMN organizer_email_normalized TEXT")
  }
  DBI::dbExecute(
    conn,
    "UPDATE polls
     SET organizer_email_normalized = lower(trim(organizer_email))
     WHERE organizer_email_normalized IS NULL OR organizer_email_normalized = ''"
  )
  invisible(TRUE)
}

migrate_participants_email_nullable <- function(conn) {
  table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(participants)")
  if (!"email" %in% table_info$name) {
    return(invisible(FALSE))
  }
  email_info <- table_info[table_info$name == "email", , drop = FALSE]
  if (nrow(email_info) == 0 || !identical(as.integer(email_info$notnull[[1]]), 1L)) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "PRAGMA foreign_keys = OFF")
  on.exit(DBI::dbExecute(conn, "PRAGMA foreign_keys = ON"), add = TRUE)
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, "ALTER TABLE participants RENAME TO participants_old")
    DBI::dbExecute(conn, "ALTER TABLE responses RENAME TO responses_old")
    DBI::dbExecute(conn, "CREATE TABLE participants (
      participant_id INTEGER PRIMARY KEY AUTOINCREMENT,
      poll_id INTEGER NOT NULL,
      name TEXT NOT NULL,
      email TEXT,
      organization TEXT,
      submitted_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (poll_id) REFERENCES polls(poll_id) ON DELETE CASCADE,
      UNIQUE (poll_id, email)
    )")
    DBI::dbExecute(conn, "CREATE TABLE responses (
      response_id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id INTEGER NOT NULL,
      option_id INTEGER NOT NULL,
      availability TEXT NOT NULL CHECK (availability IN ('preferred', 'available', 'unavailable')),
      comment TEXT,
      FOREIGN KEY (participant_id) REFERENCES participants(participant_id) ON DELETE CASCADE,
      FOREIGN KEY (option_id) REFERENCES poll_options(option_id) ON DELETE CASCADE,
      UNIQUE (participant_id, option_id)
    )")
    DBI::dbExecute(conn, "INSERT INTO participants (
      participant_id, poll_id, name, email, organization, submitted_at, updated_at
    )
    SELECT participant_id, poll_id, name, email, organization, submitted_at, updated_at
    FROM participants_old")
    DBI::dbExecute(conn, "INSERT INTO responses (
      response_id, participant_id, option_id, availability, comment
    )
    SELECT response_id, participant_id, option_id, availability, comment
    FROM responses_old")
    DBI::dbExecute(conn, "DROP TABLE responses_old")
    DBI::dbExecute(conn, "DROP TABLE participants_old")
  })
  invisible(TRUE)
}

initialize_database <- function(db_path = Sys.getenv("SQLITE_DB_PATH", unset = "data/app.sqlite")) {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Packages DBI and RSQLite are required to initialize the local database.", call. = FALSE)
  }
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  for (statement in schema_statements()) {
    if (!grepl("^CREATE INDEX", trimws(statement), ignore.case = TRUE)) {
      DBI::dbExecute(conn, statement)
    }
  }
  run_schema_migrations(conn)
  for (statement in schema_statements()) {
    if (grepl("^CREATE INDEX", trimws(statement), ignore.case = TRUE)) {
      DBI::dbExecute(conn, statement)
    }
  }
  invisible(db_path)
}
