finalize_poll_ui <- function(id) {
  ns <- shiny::NS(id)
  section_panel_ui(
    "Finalize and close poll",
    "Choose the final time, prepare the email text, and close the poll when the decision is made.",
    shiny::uiOutput(ns("final_status")),
    shiny::div(
      class = "finalize-layout",
      shiny::div(
        class = "finalize-controls",
        shiny::uiOutput(ns("final_option_ui")),
        shiny::textAreaInput(ns("final_notes"), "Optional final notes", rows = 3, placeholder = "Add agenda, joining instructions, or follow-up context."),
        shiny::uiOutput(ns("final_actions"))
      ),
      shiny::div(
        class = "finalize-preview",
        shiny::uiOutput(ns("email_preview_ui"))
      )
    )
  )
}

finalize_poll_server <- function(id, conn, dashboard_data, refresh, display_timezone = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    display_timezone <- display_timezone %||% shiny::reactive({
      data <- dashboard_data()
      if (is.null(data) || is.null(data$poll)) {
        return("UTC")
      }
      data$poll$timezone[[1]]
    })

    selected_option <- shiny::reactive({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$options) == 0 || is.null(input$selected_option_id)) {
        return(NULL)
      }
      option <- data$options[data$options$option_id == as.integer(input$selected_option_id), , drop = FALSE]
      if (nrow(option) == 0) NULL else option
    })

    output$final_status <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data) || is.null(data$finalized)) {
        return(NULL)
      }
      shiny::div(
        class = "ready-box",
        shiny::strong("This poll has been finalized."),
        option_time_ui(data$finalized, display_timezone(), heading = "div")
      )
    })

    output$final_option_ui <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$options) == 0) {
        return(shiny::p(class = "helper-text", "No proposed time slots are available."))
      }
      choices <- stats::setNames(
        data$options$option_id,
        vapply(seq_len(nrow(data$options)), function(i) {
          format_readable_option_for_option(data$options[i, , drop = FALSE], display_timezone())
        }, character(1))
      )
      selected <- if (!is.null(data$finalized)) data$finalized$selected_option_id[[1]] else data$options$option_id[[1]]
      shiny::selectInput(session$ns("selected_option_id"), "Selected meeting time", choices = choices, selected = selected)
    })

    output$final_actions <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data)) {
        return(NULL)
      }
      if (identical(poll_display_status(data$poll, data$options), "finalized")) {
        return(shiny::div(
          class = "button-row",
          shiny::downloadButton(session$ns("download_ics"), "Download .ics file", class = "btn-outline-secondary")
        ))
      }
      shiny::div(
        class = "button-row",
        shiny::downloadButton(session$ns("download_ics"), "Download .ics file", class = "btn-outline-secondary"),
        shiny::actionButton(session$ns("finalize"), "Finalize selected time", class = "btn-primary"),
        shiny::actionButton(session$ns("close_poll"), "Close without finalizing", class = "btn-outline-secondary")
      )
    })

    output$email_preview_ui <- shiny::renderUI({
      data <- dashboard_data()
      option <- selected_option()
      if (is.null(data) || is.null(option)) {
        return(NULL)
      }
      text <- generate_final_email_text(data$poll, option, input$final_notes %||% "", display_timezone = display_timezone())
      shiny::tagList(
        shiny::div(
          class = "email-preview-heading",
          shiny::div(
            shiny::span(class = "option-kicker", "Copy-ready email"),
            shiny::h3("Final meeting message")
          ),
          shiny::tags$button(
            type = "button",
            class = "btn btn-outline-secondary copy-button",
            `data-copy-target` = session$ns("email_preview"),
            "Copy"
          )
        ),
        shiny::tags$textarea(
          id = session$ns("email_preview"),
          class = "form-control copy-text",
          rows = 12,
          readonly = "readonly",
          text
        )
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
