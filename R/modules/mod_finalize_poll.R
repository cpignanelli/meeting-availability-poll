finalize_poll_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "section-card",
    shiny::h2("Finalize meeting"),
    shiny::uiOutput(ns("final_option_ui")),
    shiny::textAreaInput(ns("final_notes"), "Optional final notes", rows = 3),
    shiny::uiOutput(ns("email_preview_ui")),
    shiny::div(
      class = "d-flex gap-2 flex-wrap",
      shiny::downloadButton(ns("download_ics"), "Download .ics file", class = "btn-outline-secondary"),
      shiny::actionButton(ns("finalize"), "Finalize and close poll", class = "btn-primary"),
      shiny::actionButton(ns("close_poll"), "Close without finalizing", class = "btn-outline-secondary")
    )
  )
}

finalize_poll_server <- function(id, conn, dashboard_data, refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_option <- shiny::reactive({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$options) == 0 || is.null(input$selected_option_id)) {
        return(NULL)
      }
      option <- data$options[data$options$option_id == as.integer(input$selected_option_id), , drop = FALSE]
      if (nrow(option) == 0) NULL else option
    })

    output$final_option_ui <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$options) == 0) {
        return(shiny::p(class = "helper-text", "No proposed time slots are available."))
      }
      choices <- stats::setNames(data$options$option_id, data$options$display_label)
      selected <- if (!is.null(data$finalized)) data$finalized$selected_option_id[[1]] else data$options$option_id[[1]]
      shiny::selectInput(session$ns("selected_option_id"), "Selected meeting time", choices = choices, selected = selected)
    })

    output$email_preview_ui <- shiny::renderUI({
      data <- dashboard_data()
      option <- selected_option()
      if (is.null(data) || is.null(option)) {
        return(NULL)
      }
      text <- generate_final_email_text(data$poll, option, input$final_notes %||% "")
      shiny::tagList(
        shiny::label("Copy-ready final meeting email text"),
        shiny::tags$textarea(class = "form-control copy-text", rows = 10, readonly = "readonly", text)
      )
    })

    output$download_ics <- shiny::downloadHandler(
      filename = function() {
        "meeting-invite.ics"
      },
      content = function(file) {
        data <- dashboard_data()
        option <- selected_option()
        if (is.null(data) || is.null(option)) {
          stop("Select a meeting time before downloading the calendar file.", call. = FALSE)
        }
        writeLines(generate_ics(data$poll, option, input$final_notes %||% ""), file, useBytes = TRUE)
      },
      contentType = "text/calendar"
    )

    shiny::observeEvent(input$finalize, {
      data <- dashboard_data()
      option <- selected_option()
      tryCatch({
        if (is.null(data) || is.null(option)) {
          stop("Select a meeting time before finalizing.", call. = FALSE)
        }
        finalize_meeting(conn, data$poll$poll_id[[1]], option$option_id[[1]], input$final_notes %||% "")
        refresh()
        shiny::showNotification("Meeting finalized and poll closed.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$close_poll, {
      data <- dashboard_data()
      tryCatch({
        if (is.null(data)) {
          stop("Poll not found.", call. = FALSE)
        }
        close_poll(conn, data$poll$poll_id[[1]])
        refresh()
        shiny::showNotification("Poll closed to new responses.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })
  })
}
