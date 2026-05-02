source("R/utils/time_helpers.R", local = TRUE)
source("R/utils/validation.R", local = TRUE)
source("R/db/db_schema.R", local = TRUE)

host <- Sys.getenv("APP_HOST", unset = "127.0.0.1")
port <- as.integer(Sys.getenv("APP_PORT", unset = "3838"))
base_url <- Sys.getenv("APP_BASE_URL", unset = "")

if (is.na(port) || port < 1 || port > 65535) {
  stop("APP_PORT must be a valid TCP port number.", call. = FALSE)
}

initialize_database(Sys.getenv("SQLITE_DB_PATH", unset = "data/app.sqlite"))

if (!nzchar(base_url)) {
  message(
    "APP_BASE_URL is not set. Generated links will use the browser URL seen by Shiny.\n",
    "For a live test, set APP_BASE_URL to the URL your colleague will open."
  )
}

message("Starting Meeting Availability Poll on http://", host, ":", port)
if (nzchar(base_url)) {
  message("Generated poll links will use APP_BASE_URL=", base_url)
}

shiny::runApp(".", host = host, port = port, launch.browser = FALSE)
