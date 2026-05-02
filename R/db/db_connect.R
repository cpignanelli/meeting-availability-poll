get_db_connection <- function(db_path = Sys.getenv("SQLITE_DB_PATH", unset = "data/app.sqlite"), use_pool = TRUE) {
  if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
    stop("Hosted database connections are documented for production but not enabled in this proof of concept.", call. = FALSE)
  }
  initialize_database(db_path)
  if (isTRUE(use_pool) && requireNamespace("pool", quietly = TRUE)) {
    pool::dbPool(RSQLite::SQLite(), dbname = db_path)
  } else {
    DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  }
}

close_db_connection <- function(conn) {
  if (inherits(conn, "Pool")) {
    pool::poolClose(conn)
  } else if (DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn)
  }
  invisible(TRUE)
}

with_db_transaction <- function(conn, callback) {
  if (inherits(conn, "Pool")) {
    pool::poolWithTransaction(conn, callback)
  } else {
    DBI::dbWithTransaction(conn, callback(conn))
  }
}
