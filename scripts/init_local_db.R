source("R/utils/time_helpers.R", local = TRUE)
source("R/utils/validation.R", local = TRUE)
source("R/db/db_schema.R", local = TRUE)

db_path <- Sys.getenv("SQLITE_DB_PATH", unset = "data/app.sqlite")
initialize_database(db_path)
message("Local SQLite database is ready at ", db_path, ". Existing data was preserved.")
