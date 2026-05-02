CONFIRM_RESET <- FALSE

if (!isTRUE(CONFIRM_RESET)) {
  stop(
    "Reset cancelled. To delete local test data, close the app, edit this script, and set CONFIRM_RESET <- TRUE.",
    call. = FALSE
  )
}

source("R/utils/time_helpers.R", local = TRUE)
source("R/utils/validation.R", local = TRUE)
source("R/db/db_schema.R", local = TRUE)

db_path <- Sys.getenv("SQLITE_DB_PATH", unset = "data/app.sqlite")
if (normalizePath(dirname(db_path), mustWork = FALSE) != normalizePath("data", mustWork = FALSE)) {
  stop("Refusing to reset a database outside the local data/ folder.", call. = FALSE)
}

if (file.exists(db_path)) {
  unlink(db_path)
  message("Deleted local proof-of-concept database at ", db_path, ".")
}

initialize_database(db_path)
message("Recreated empty local SQLite database schema at ", db_path, ".")
