respond_poll_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-shell",
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

    output$respond_page <- shiny::renderUI({
      bundle <- poll_bundle()
      if (!is.null(bundle$error)) {
        return(empty_state_ui("Invalid response link", bundle$error))
      }
      poll <- bundle$poll
      options <- bundle$options
      if (submitted()) {
        return(shiny::div(
          class = "section-card",
          shiny::h1("Response submitted"),
          shiny::p("Thank you. Your availability has been saved. You may close this page.")
        ))
      }
      if (!identical(poll$status[[1]], "open")) {
        return(empty_state_ui("This poll is closed", "The organizer is no longer accepting responses for this poll."))
      }
      if (deadline_has_passed(poll$response_deadline[[1]], poll$timezone[[1]])) {
        return(empty_state_ui("The response deadline has passed", "The organizer's response deadline has passed."))
      }

      availability_inputs <- lapply(seq_len(nrow(options)), function(i) {
        option <- options[i, , drop = FALSE]
        shiny::div(
          class = "section-card",
          shiny::h3(option$display_label[[1]]),
          shiny::radioButtons(
            session$ns(paste0("availability_", option$option_id[[1]])),
            "Your availability",
            choices = availability_choices(),
            selected = character(0)
          )
        )
      })

      shiny::tagList(
        shiny::div(
          class = "app-header",
          shiny::h1(poll$title[[1]]),
          shiny::p(poll$description[[1]])
        ),
        shiny::div(
          class = "privacy-notice",
          shiny::strong("Privacy notice"),
          shiny::p("This form collects your name, email, organization, availability, and optional comments so the organizer can choose a meeting time. Results are visible only through the private organizer link.")
        ),
        shiny::div(
          class = "section-card",
          shiny::h2("Your details"),
          shiny::fluidRow(
            shiny::column(4, shiny::textInput(session$ns("name"), "Name")),
            shiny::column(4, shiny::textInput(session$ns("email"), "Email")),
            shiny::column(4, shiny::textInput(session$ns("organization"), "Organization"))
          ),
          shiny::p(class = "helper-text", "Available and preferred means you can attend and this time works especially well for you.")
        ),
        availability_inputs,
        shiny::div(
          class = "section-card",
          shiny::textAreaInput(session$ns("comments"), "Optional comments", rows = 3),
          shiny::actionButton(session$ns("submit_response"), "Submit availability", class = "btn-primary")
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
        if (!identical(poll$status[[1]], "open") || deadline_has_passed(poll$response_deadline[[1]], poll$timezone[[1]])) {
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

empty_state_ui <- function(title, message) {
  shiny::div(
    class = "empty-state",
    shiny::h2(title),
    shiny::p(message)
  )
}
