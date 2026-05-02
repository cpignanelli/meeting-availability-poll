admin_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-shell",
    shiny::uiOutput(ns("admin_page"))
  )
}

admin_dashboard_server <- function(id, conn, token) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh_counter <- shiny::reactiveVal(0L)
    refresh <- function() refresh_counter(refresh_counter() + 1L)

    poll <- shiny::reactive({
      token_value <- token()
      if (is.null(token_value) || !nzchar(token_value)) {
        return(NULL)
      }
      tryCatch(get_poll_by_admin_token(conn, token_value), error = function(e) NULL)
    })

    dashboard_data <- shiny::reactive({
      refresh_counter()
      current_poll <- poll()
      if (is.null(current_poll)) {
        return(NULL)
      }
      get_poll_dashboard_data(conn, current_poll$poll_id[[1]])
    })

    output$admin_page <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data)) {
        return(empty_state_ui("Invalid organizer link", "This private organizer link is missing or invalid."))
      }
      shiny::tagList(
        shiny::div(
          class = "app-header",
          shiny::h1(data$poll$title[[1]]),
          shiny::p(data$poll$description[[1]]),
          shiny::span(class = paste("status-pill", paste0("status-", data$poll$status[[1]])), paste("Status:", data$poll$status[[1]]))
        ),
        shiny::uiOutput(session$ns("summary_cards")),
        shiny::div(
          class = "section-card",
          shiny::h2("Ranked time slots"),
          DT::DTOutput(session$ns("ranked_table")),
          shiny::downloadButton(session$ns("download_ranked"), "Export ranked slots CSV", class = "btn-outline-secondary")
        ),
        shiny::div(
          class = "section-card",
          shiny::h2("Availability heatmap"),
          shiny::uiOutput(session$ns("heatmap"))
        ),
        shiny::div(
          class = "section-card",
          shiny::h2("Participant responses"),
          DT::DTOutput(session$ns("responses_table")),
          shiny::downloadButton(session$ns("download_responses"), "Export responses CSV", class = "btn-outline-secondary")
        ),
        shiny::div(
          class = "section-card",
          shiny::h2("Missing expected responses"),
          shiny::uiOutput(session$ns("missing_expected"))
        ),
        finalize_poll_ui(session$ns("finalize"))
      )
    })

    output$summary_cards <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data)) return(NULL)
      best <- if (nrow(data$ranked) == 0) "No options" else data$ranked$time_option[[1]]
      shiny::div(
        class = "summary-grid",
        summary_card("Total responses", nrow(data$participants)),
        summary_card("Proposed time slots", nrow(data$options)),
        summary_card("Best-ranked time", best),
        summary_card("Response deadline", format_deadline_label(data$poll$response_deadline[[1]])),
        summary_card("Poll status", data$poll$status[[1]])
      )
    })

    output$ranked_table <- DT::renderDT({
      data <- dashboard_data()
      if (is.null(data)) return(data.frame())
      data$ranked
    }, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))

    output$responses_table <- DT::renderDT({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$responses) == 0) {
        return(data.frame(Message = "No participant responses yet."))
      }
      transform(
        data$responses[c("name", "email", "organization", "display_label", "availability", "comment")],
        availability = vapply(availability, availability_label, character(1))
      )
    }, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))

    output$heatmap <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$participants) == 0) {
        return(empty_state_ui("No responses yet", "The heatmap will appear after participants submit availability."))
      }
      build_heatmap_table(data$participants, data$options, data$heatmap)
    })

    output$missing_expected <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$expected) == 0) {
        return(shiny::p(class = "helper-text", "No expected participant list was entered for this poll."))
      }
      if (nrow(data$missing_expected) == 0) {
        return(shiny::p("All expected participants have submitted responses."))
      }
      DT::DTOutput(session$ns("missing_expected_table"))
    })

    output$missing_expected_table <- DT::renderDT({
      data <- dashboard_data()
      if (is.null(data)) return(data.frame())
      data$missing_expected
    }, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))

    output$download_ranked <- shiny::downloadHandler(
      filename = function() "ranked-time-slots.csv",
      content = function(file) {
        data <- dashboard_data()
        utils::write.csv(data$ranked, file, row.names = FALSE, na = "")
      }
    )

    output$download_responses <- shiny::downloadHandler(
      filename = function() "participant-responses.csv",
      content = function(file) {
        data <- dashboard_data()
        utils::write.csv(data$responses, file, row.names = FALSE, na = "")
      }
    )

    finalize_poll_server("finalize", conn, dashboard_data, refresh)
  })
}

summary_card <- function(label, value) {
  shiny::div(
    class = "summary-card",
    shiny::div(class = "summary-label", label),
    shiny::div(class = "summary-value", as.character(value))
  )
}

build_heatmap_table <- function(participants, options, heatmap) {
  header <- shiny::tags$tr(
    shiny::tags$th("Participant"),
    lapply(options$display_label, shiny::tags$th)
  )
  rows <- lapply(seq_len(nrow(participants)), function(i) {
    participant <- participants[i, , drop = FALSE]
    cells <- lapply(seq_len(nrow(options)), function(j) {
      option <- options[j, , drop = FALSE]
      cell <- heatmap[
        heatmap$email == participant$email[[1]] &
          heatmap$option_id == option$option_id[[1]],
        ,
        drop = FALSE
      ]
      availability <- if (nrow(cell) == 0) "missing" else cell$availability[[1]]
      label <- availability_label(availability)
      shiny::tags$td(
        class = paste("availability-cell", paste0("availability-", availability)),
        shiny::strong(label),
        shiny::span(class = "availability-code", if (availability == "preferred") "Can attend; preferred" else label)
      )
    })
    shiny::tags$tr(
      shiny::tags$td(
        shiny::strong(participant$name[[1]]),
        shiny::span(class = "availability-code", paste(participant$organization[[1]], participant$email[[1]], sep = " | "))
      ),
      cells
    )
  })
  shiny::div(
    class = "heatmap-scroll",
    shiny::tags$table(
      class = "availability-heatmap",
      shiny::tags$thead(header),
      shiny::tags$tbody(rows)
    )
  )
}
