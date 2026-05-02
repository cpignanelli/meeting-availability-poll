if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install rsconnect before writing a manifest: install.packages('rsconnect')", call. = FALSE)
}

deployment_files <- c(
  "app.R",
  "data/.gitkeep",
  list.files("R", recursive = TRUE, full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE)
)
deployment_files <- deployment_files[file.exists(deployment_files)]

rsconnect::writeManifest(
  appDir = ".",
  appFiles = deployment_files,
  appPrimaryDoc = "app.R",
  appMode = "shiny",
  quiet = FALSE
)

message("Wrote manifest.json for Posit Connect Cloud GitHub deployment.")
message("Commit manifest.json along with the app source files before publishing from GitHub.")
