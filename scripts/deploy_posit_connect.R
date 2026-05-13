if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install the rsconnect package before deploying: install.packages('rsconnect')", call. = FALSE)
}

deployment_files <- c(
  "app.R",
  "renv.lock",
  "data/.gitkeep",
  list.files("R", recursive = TRUE, full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE)
)
deployment_files <- deployment_files[file.exists(deployment_files)]

app_name <- Sys.getenv("CONNECT_APP_NAME", unset = "meeting-availability-poll")
app_title <- Sys.getenv("CONNECT_APP_TITLE", unset = "Meeting Availability Poll")
account <- Sys.getenv("CONNECT_ACCOUNT", unset = "")
server <- Sys.getenv("CONNECT_SERVER_NAME", unset = "")

env_var_candidates <- c(
  "APP_BASE_URL",
  "APP_MAIN_OWNER_EMAIL",
  "ORGANIZER_AUTH_SECRET",
  "DATABASE_BACKEND",
  "SQLITE_DB_PATH",
  "MONGODB_URI",
  "MONGODB_DATABASE",
  "SMTP_HOST",
  "SMTP_PORT",
  "SMTP_USERNAME",
  "SMTP_PASSWORD",
  "SMTP_FROM",
  "SMTP_USE_SSL",
  "ALLOW_DEV_AUTH_CODE_DISPLAY",
  "POLL_CREATION_SECRET"
)
env_vars <- env_var_candidates[nzchar(Sys.getenv(env_var_candidates, unset = ""))]
if (length(env_vars) == 0) {
  env_vars <- NULL
}

message("Deploying ", app_title, " to Posit Connect.")
message("Bundling ", length(deployment_files), " app files. Local SQLite data is excluded.")
if (!is.null(env_vars)) {
  message("Syncing environment variable names: ", paste(env_vars, collapse = ", "))
}

rsconnect::deployApp(
  appDir = ".",
  appFiles = deployment_files,
  appName = app_name,
  appTitle = app_title,
  appMode = "shiny",
  account = if (nzchar(account)) account else NULL,
  server = if (nzchar(server)) server else NULL,
  envVars = env_vars,
  launch.browser = TRUE,
  lint = FALSE
)
