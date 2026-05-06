respond_poll_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-shell respondent-shell",
    app_topbar_ui("Respond"),
    shiny::uiOutput(ns("respond_page"))
  )
}

respond_poll_server <- function(id, conn, token) {
  shiny::moduleServer(id, function(input, output, session) {
    submitted <- shiny::reactiveVal(FALSE)

    poll_bundle <- shiny::reactive({
      token_value <- token()
      if (is.null(token_value) || !nzchar(token_value)) {
        return(list(error = "This response link is missing a token."))
      }
      tryCatch({
        poll <- get_poll_by_response_token(conn, token_value)
        if (is.null(poll)) {
          return(list(error = "This response link is not valid."))
        }
        list(
          poll = poll,
          options = get_poll_options(conn, poll$poll_id[[1]])
        )
      }, error = function(e) {
        list(error = "This response link is not valid.")
      })
    })

    output$completion_status <- shiny::renderUI({
      bundle <- poll_bundle()
      if (!is.null(bundle$error) || is.null(bundle$options)) {
        return(NULL)
      }
      options <- bundle$options
      total <- nrow(options)
      answered <- sum(vapply(options$option_id, function(option_id) {
        nzchar(input[[paste0("availability_", option_id)]] %||% "")
      }, logical(1)))
      percent <- if (total == 0) 0 else round(answered / total * 100)

      shiny::div(
        class = "completion-meter",
        shiny::div(
          class = "completion-meter-label",
          shiny::span(paste(answered, "of", total, "times answered")),
          shiny::span(paste0(percent, "%"))
        ),
        shiny::div(
          class = "completion-track",
          shiny::div(class = "completion-fill", style = paste0("width:", percent, "%;"))
        )
      )
    })

    output$respond_page <- shiny::renderUI({
      bundle <- poll_bundle()
      if (!is.null(bundle$error)) {
        return(empty_state_ui("Invalid response link", bundle$error))
      }
      poll <- bundle$poll
      options <- bundle$options
      if (submitted()) {
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Availability saved",
            title = "Response submitted",
            subtitle = "Your availability has been saved. Final meeting confirmation will follow from the organizer."
          ),
          section_panel_ui(
            "What happens next",
            NULL,
            shiny::p("The organizer will review all responses through their private dashboard and follow up with the final meeting time.")
          )
        ))
      }
      display_status <- poll_display_status(poll)
      if (identical(display_status, "finalized")) {
        return(empty_state_ui("This poll has been finalized", finalized_poll_contact_message(poll)))
      }
      if (identical(display_status, "expired")) {
        return(closed_poll_contact_ui(poll, "This booking link has expired"))
      }
      if (!identical(display_status, "open")) {
        return(closed_poll_contact_ui(poll, "This booking link is closed"))
      }

      shiny::tagList(
        page_header_ui(
          eyebrow = "Meeting availability poll",
          title = poll$title[[1]],
          subtitle = poll$description[[1]],
          meta = detail_grid_ui(poll_detail_items(poll))
        ),
        privacy_notice_ui(compact = TRUE),
        section_panel_ui(
          "Your details",
          "Use the email address the organizer knows. If you respond again with the same email, your latest response replaces the earlier one.",
          shiny::fluidRow(
            shiny::column(4, shiny::textInput(session$ns("name"), "Name")),
            shiny::column(4, shiny::textInput(session$ns("email"), "Email")),
            shiny::column(4, shiny::textInput(session$ns("organization"), "Organization"))
          )
        ),
        section_panel_ui(
          "Choose your availability",
          "Available and preferred means you can attend and this time works especially well for you. Final meeting confirmation will follow from the organizer.",
          availability_legend_ui(),
          build_response_matrix_ui(session$ns, options)
        ),
        shiny::div(
          class = "response-submit-bar",
          shiny::uiOutput(session$ns("completion_status")),
          shiny::div(
            class = "response-submit-actions",
            shiny::textAreaInput(session$ns("comments"), "Optional comments", rows = 2, placeholder = "Anything the organizer should know?"),
            shiny::actionButton(session$ns("submit_response"), "Submit my availability", class = "btn-primary btn-lg")
          )
        )
      )
    })

    shiny::observeEvent(input$submit_response, {
      bundle <- poll_bundle()
      if (!is.null(bundle$error)) {
        shiny::showNotification(bundle$error, type = "error")
        return()
      }
      poll <- bundle$poll
      options <- bundle$options
      tryCatch({
        if (!poll_accepts_responses(poll)) {
          stop("This poll is no longer accepting responses.", call. = FALSE)
        }
        participant <- list(
          name = sanitize_text(input$name, max_chars = 160, required = TRUE, field = "Name"),
          email = validate_email(input$email, field = "Email"),
          organization = sanitize_text(input$organization, max_chars = 160, required = TRUE, field = "Organization")
        )
        response_values <- data.frame(
          option_id = options$option_id,
          availability = vapply(options$option_id, function(option_id) {
            validate_availability(input[[paste0("availability_", option_id)]])
          }, character(1)),
          stringsAsFactors = FALSE
        )
        comment <- sanitize_text(input$comments, max_chars = 2000)
        submit_poll_response(conn, poll$poll_id[[1]], participant, response_values, comment)
        submitted(TRUE)
        shiny::showNotification("Your response has been saved.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })
  })
}

build_response_option_ui <- function(ns, option, index, total) {
  shiny::div(
    class = "response-matrix-row",
    shiny::div(
      class = "response-time-cell",
      shiny::span(class = "option-kicker", paste("Option", index, "of", total)),
      shiny::h3(option$display_label[[1]])
    ),
    shiny::div(
      class = "availability-vote",
      shiny::radioButtons(
        ns(paste0("availability_", option$option_id[[1]])),
        "Choose one",
        choices = availability_choices(),
        selected = character(0),
        inline = TRUE
      )
    )
  )
}

build_response_matrix_ui <- function(ns, options) {
  rows <- lapply(seq_len(nrow(options)), function(i) {
    build_response_option_ui(ns, options[i, , drop = FALSE], i, nrow(options))
  })

  shiny::div(
    class = "response-matrix",
    shiny::div(
      class = "response-matrix-header",
      shiny::span("Proposed time"),
      shiny::span("Preferred"),
      shiny::span("Available"),
      shiny::span("Unavailable")
    ),
    rows
  )
}
