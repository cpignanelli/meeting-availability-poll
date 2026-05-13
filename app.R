source("R/utils/time_helpers.R", local = TRUE)
source("R/utils/validation.R", local = TRUE)
source("R/utils/auth_helpers.R", local = TRUE)
source("R/utils/scoring.R", local = TRUE)
source("R/utils/ics_helpers.R", local = TRUE)
source("R/utils/email_text_helpers.R", local = TRUE)
source("R/utils/email_helpers.R", local = TRUE)
source("R/utils/ui_helpers.R", local = TRUE)
source("R/db/db_schema.R", local = TRUE)
source("R/db/db_mongo.R", local = TRUE)
source("R/db/db_connect.R", local = TRUE)
source("R/db/db_queries.R", local = TRUE)
source("R/styles/app_theme.R", local = TRUE)
source("R/modules/mod_create_poll.R", local = TRUE)
source("R/modules/mod_respond_poll.R", local = TRUE)
source("R/modules/mod_finalize_poll.R", local = TRUE)
source("R/modules/mod_admin_dashboard.R", local = TRUE)
source("R/modules/mod_organizer_portal.R", local = TRUE)

required_packages <- c("shiny", "bslib", "DBI", "RSQLite", "pool", "DT", "openssl", "digest", "htmltools", "jsonlite", "mongolite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running the app: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

db <- get_db_connection()
shiny::onStop(function() {
  close_db_connection(db)
})

ui <- bslib::page_fluid(
  theme = app_theme(),
  shiny::tags$head(
    shiny::includeCSS("www/custom.css"),
    shiny::includeScript("www/app.js"),
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  shiny::uiOutput("route_ui")
)

server <- function(input, output, session) {
  query <- shiny::reactive({
    shiny::parseQueryString(session$clientData$url_search %||% "")
  })

  output$route_ui <- shiny::renderUI({
    params <- query()
    if (!is.null(params$respond) && nzchar(params$respond)) {
      respond_poll_ui("respond")
    } else if (!is.null(params$admin) && nzchar(params$admin)) {
      admin_dashboard_ui("admin")
    } else if (!is.null(params$organizer) && identical(params$organizer, "login")) {
      organizer_portal_ui("organizer")
    } else if (!is.null(params$create) && can_create_poll(params)) {
      create_poll_ui("create")
    } else if (!is.null(params$create)) {
      private_creation_page_ui()
    } else {
      organizer_portal_ui("organizer")
    }
  })

  create_poll_server("create", db)
  respond_poll_server("respond", db, token = shiny::reactive(query()[["respond"]] %||% ""))
  admin_dashboard_server("admin", db, token = shiny::reactive(query()[["admin"]] %||% ""))
  organizer_portal_server("organizer", db)
}

shiny::shinyApp(ui, server)
