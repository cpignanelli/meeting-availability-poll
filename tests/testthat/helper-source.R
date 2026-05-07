project_root <- if (dir.exists(file.path(getwd(), "R"))) {
  getwd()
} else {
  normalizePath(file.path(getwd(), "../.."), mustWork = TRUE)
}

source(file.path(project_root, "R/utils/time_helpers.R"), local = TRUE)
source(file.path(project_root, "R/utils/validation.R"), local = TRUE)
source(file.path(project_root, "R/utils/auth_helpers.R"), local = TRUE)
source(file.path(project_root, "R/utils/scoring.R"), local = TRUE)
source(file.path(project_root, "R/utils/ics_helpers.R"), local = TRUE)
source(file.path(project_root, "R/utils/email_text_helpers.R"), local = TRUE)
source(file.path(project_root, "R/utils/email_helpers.R"), local = TRUE)
source(file.path(project_root, "R/utils/ui_helpers.R"), local = TRUE)
source(file.path(project_root, "R/db/db_schema.R"), local = TRUE)
source(file.path(project_root, "R/db/db_connect.R"), local = TRUE)
source(file.path(project_root, "R/db/db_queries.R"), local = TRUE)

make_test_connection <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  initialize_database(db_path)
  get_db_connection(db_path = db_path, use_pool = FALSE)
}

make_sample_poll <- function(conn) {
  timezone <- "America/Toronto"
  start_one <- as.POSIXct(Sys.time() + 86400 * 3, tz = timezone)
  start_two <- as.POSIXct(Sys.time() + 86400 * 4, tz = timezone)
  options <- data.frame(
    start_datetime = c(as_utc_string(start_one), as_utc_string(start_two)),
    end_datetime = c(as_utc_string(add_minutes(start_one, 60)), as_utc_string(add_minutes(start_two, 60))),
    display_label = c(
      format_option_label(as_utc_string(start_one), as_utc_string(add_minutes(start_one, 60)), timezone),
      format_option_label(as_utc_string(start_two), as_utc_string(add_minutes(start_two, 60)), timezone)
    ),
    option_order = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  poll <- list(
    title = "Test meeting",
    description = "Test description",
    organizer_name = "Organizer",
    organizer_email = "organizer@example.org",
    duration_minutes = 60L,
    timezone = timezone,
    location_type = "Virtual",
    location_details = "Video link to follow",
    response_deadline = ""
  )
  expected <- data.frame(
    name = "Required Person",
    email = "required@example.org",
    organization = "Org",
    is_required = 1L,
    stringsAsFactors = FALSE
  )
  create_poll_record(conn, poll, options, expected)
}
