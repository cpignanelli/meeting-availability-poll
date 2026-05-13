database_backend <- function() {
  backend <- tolower(trimws(Sys.getenv("DATABASE_BACKEND", unset = "sqlite")))
  if (!nzchar(backend)) {
    backend <- "sqlite"
  }
  if (!backend %in% c("sqlite", "mongodb")) {
    stop("DATABASE_BACKEND must be either 'sqlite' or 'mongodb'.", call. = FALSE)
  }
  backend
}

get_db_connection <- function(db_path = Sys.getenv("SQLITE_DB_PATH", unset = "data/app.sqlite"), use_pool = TRUE) {
  if (identical(database_backend(), "mongodb")) {
    return(get_mongo_connection())
  }
  if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
    stop("DATABASE_URL is not used by this app. Set DATABASE_BACKEND=mongodb with MONGODB_URI, or leave DATABASE_BACKEND unset for SQLite.", call. = FALSE)
  }
  initialize_database(db_path)
  if (isTRUE(use_pool) && requireNamespace("pool", quietly = TRUE)) {
    pool::dbPool(RSQLite::SQLite(), dbname = db_path)
  } else {
    DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  }
}

close_db_connection <- function(conn) {
  if (is_mongo_connection(conn)) {
    close_mongo_connection(conn)
  } else if (inherits(conn, "Pool")) {
    pool::poolClose(conn)
  } else if (DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn)
  }
  invisible(TRUE)
}

with_db_transaction <- function(conn, callback) {
  if (is_mongo_connection(conn)) {
    callback(conn)
  } else if (inherits(conn, "Pool")) {
    pool::poolWithTransaction(conn, callback)
  } else {
    DBI::dbWithTransaction(conn, callback(conn))
  }
}
