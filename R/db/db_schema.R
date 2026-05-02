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
      email TEXT NOT NULL,
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
    "CREATE INDEX IF NOT EXISTS idx_polls_response_token ON polls(response_token)",
    "CREATE INDEX IF NOT EXISTS idx_polls_admin_token_hash ON polls(admin_token_hash)",
    "CREATE INDEX IF NOT EXISTS idx_poll_options_poll_id ON poll_options(poll_id)",
    "CREATE INDEX IF NOT EXISTS idx_expected_poll_id ON expected_participants(poll_id)",
    "CREATE INDEX IF NOT EXISTS idx_participants_poll_id ON participants(poll_id)",
    "CREATE INDEX IF NOT EXISTS idx_responses_participant_id ON responses(participant_id)",
    "CREATE INDEX IF NOT EXISTS idx_responses_option_id ON responses(option_id)",
    "CREATE INDEX IF NOT EXISTS idx_audit_poll_id ON audit_log(poll_id)"
  )
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
    DBI::dbExecute(conn, statement)
  }
  invisible(db_path)
}
