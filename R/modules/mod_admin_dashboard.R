admin_dashboard_ui <- function(id) {
  shiny::div(
    class = "app-shell admin-shell",
    app_topbar_ui("Organizer Dashboard"),
    admin_dashboard_body_ui(id)
  )
}

admin_dashboard_body_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("admin_page"))
}

admin_dashboard_server <- function(id, conn, token = NULL, poll_id = NULL, organizer_email = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    token <- token %||% shiny::reactive("")
    poll_id <- poll_id %||% shiny::reactive(NULL)
    organizer_email <- organizer_email %||% shiny::reactive("")
    refresh_counter <- shiny::reactiveVal(0L)
    refresh <- function() refresh_counter(refresh_counter() + 1L)

    poll <- shiny::reactive({
      token_value <- token()
      if (!is.null(token_value) && nzchar(token_value)) {
        return(tryCatch(get_poll_by_admin_token(conn, token_value), error = function(e) NULL))
      }
      current_poll_id <- poll_id()
      current_email <- organizer_email()
      if (is.null(current_poll_id) || !nzchar(current_email %||% "")) {
        return(NULL)
      }
      tryCatch(get_poll_for_organizer(conn, current_poll_id, current_email), error = function(e) NULL)
    })

    dashboard_result <- shiny::reactive({
      refresh_counter()
      current_poll <- poll()
      if (is.null(current_poll)) {
        return(list(data = NULL, error = FALSE, message = "", poll = NULL))
      }
      poll_id_value <- current_poll$poll_id[[1]]
      tryCatch(
        list(
          data = get_poll_dashboard_data(conn, poll_id_value),
          error = FALSE,
          message = "",
          poll = current_poll
        ),
        error = function(e) {
          warning(
            sprintf("Dashboard load failed for poll_id=%s: %s", poll_id_value, conditionMessage(e)),
            call. = FALSE
          )
          list(
            data = NULL,
            error = TRUE,
            message = "We could not load this poll dashboard. The poll data is still stored; try refreshing, then check the app logs if this continues.",
            poll = current_poll
          )
        }
      )
    })

    dashboard_data <- shiny::reactive({
      result <- dashboard_result()
      if (isTRUE(result$error)) {
        return(NULL)
      }
      result$data
    })

    output$admin_page <- shiny::renderUI({
      result <- dashboard_result()
      if (isTRUE(result$error)) {
        return(dashboard_load_error_ui(result$poll, result$message))
      }
      data <- result$data
      if (is.null(data)) {
        return(empty_state_ui("Invalid organizer link", "This private organizer link is missing or invalid."))
      }

      tryCatch(
        shiny::tagList(
          page_header_ui(
            eyebrow = "Organizer dashboard",
            title = data$poll$title[[1]],
            subtitle = data$poll$description[[1]],
            meta = shiny::tagList(
              status_pill_ui(poll_display_status(data$poll, data$options)),
              detail_grid_ui(poll_detail_items(data$poll, data$options))
            )
          ),
          shiny::div(
            class = "dashboard-tabs",
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Overview",
                shiny::uiOutput(session$ns("summary_cards")),
                shiny::div(
                  class = "dashboard-lead-grid",
                  shiny::uiOutput(session$ns("decision_card")),
                  shiny::uiOutput(session$ns("response_link_panel"))
                ),
                section_panel_ui(
                  "Ranked options",
                  "Scores use: preferred = 2, available = 1, unavailable or missing = 0.",
                  shiny::uiOutput(session$ns("ranked_cards")),
                  shiny::details(
                    class = "details-panel",
                    shiny::tags$summary("Show detailed ranked table"),
                    DT::DTOutput(session$ns("ranked_table"))
                  ),
                  shiny::downloadButton(session$ns("download_ranked"), "Download ranked options", class = "btn-outline-secondary")
                )
              ),
              shiny::tabPanel(
                "Availability",
                section_panel_ui(
                  "Availability heatmap",
                  "Each cell includes text and color so availability is readable without relying on color alone.",
                  availability_legend_ui(),
                  shiny::uiOutput(session$ns("heatmap"))
                ),
                section_panel_ui(
                  "Missing responses",
                  NULL,
                  shiny::uiOutput(session$ns("missing_expected"))
                )
              ),
              shiny::tabPanel(
                "Responses",
                section_panel_ui(
                  "Response details",
                  "Detailed response data is visible only from this private organizer link.",
                  DT::DTOutput(session$ns("responses_table")),
                  shiny::downloadButton(session$ns("download_responses"), "Download response details", class = "btn-outline-secondary")
                )
              ),
              shiny::tabPanel(
                "Finalize",
                finalize_poll_ui(session$ns("finalize"))
              )
            )
          )
        ),
        error = function(e) {
          warning(
            sprintf("Dashboard UI failed for poll_id=%s: %s", data$poll$poll_id[[1]], conditionMessage(e)),
            call. = FALSE
          )
          dashboard_load_error_ui(
            data$poll,
            "We could not prepare the dashboard view. Try refreshing, then check the app logs if this continues."
          )
        }
      )
    })

    output$decision_card <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data)) return(NULL)
      if (nrow(data$options) == 0) {
        return(section_panel_ui(
          "Best current option",
          NULL,
          empty_state_ui("No proposed times", "This poll does not have proposed time slots to rank.")
        ))
      }
      if (nrow(data$participants) == 0) {
        return(section_panel_ui(
          "Best current option",
          NULL,
          empty_state_ui("Waiting for responses", "The recommended time will appear after participants submit availability.")
        ))
      }

      best <- data$ranked[1, , drop = FALSE]
      section_panel_ui(
        "Best current option",
        "Based on the current scoring rules and required-attendee conflicts.",
        shiny::div(
          class = "decision-card",
          shiny::div(class = "decision-kicker", "Top-ranked option"),
          option_time_ui(best, data$poll$timezone[[1]], heading = "h3"),
          shiny::div(
            class = "decision-stats",
            decision_stat_ui("Score", best$availability_score[[1]], emphasis = TRUE),
            decision_stat_ui("Preferred", best$preferred_count[[1]]),
            decision_stat_ui("Available", best$available_count[[1]]),
            decision_stat_ui("Conflicts", best$required_attendee_conflicts[[1]])
          )
        )
      )
    })

    output$response_link_panel <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data)) return(NULL)
      response_link <- build_app_link(session, "respond", data$poll$response_token[[1]])
      display_status <- poll_display_status(data$poll, data$options)
      section_panel_ui(
        "Response link",
        "Send this public link to participants. It allows response submission only.",
        status_banner_ui(
          display_status,
          paste("Link status:", poll_display_status_label(display_status)),
          response_link_status_message(data$poll, data$options, display_status)
        ),
        copy_field_ui(session$ns("response_link_admin_copy"), "Public response link", response_link),
        response_link_controls_ui(session$ns, display_status)
      )
    })

    output$summary_cards <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data)) return(NULL)
      expected_count <- nrow(data$expected)
      response_progress <- if (expected_count > 0) {
        paste0(nrow(data$participants), " of ", expected_count)
      } else {
        as.character(nrow(data$participants))
      }
      shiny::div(
        class = "summary-grid",
        metric_card_ui("Responses", response_progress, if (expected_count > 0) "Expected participant progress" else "Submitted responses"),
        metric_card_ui("Proposed slots", nrow(data$options)),
        metric_card_ui("Link expiry", format_deadline_label(poll_effective_deadline(data$poll, data$options))),
        metric_card_ui("Poll status", poll_display_status_label(poll_display_status(data$poll, data$options)))
      )
    })

    output$ranked_cards <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$ranked) == 0) {
        return(empty_state_ui("No proposed times", "This poll does not have proposed time slots."))
      }
      build_ranked_cards(data$ranked, nrow(data$participants), data$poll$timezone[[1]])
    })

    output$ranked_table <- DT::renderDT({
      data <- dashboard_data()
      if (is.null(data)) return(data.frame())
      format_ranked_table(data$ranked)
    }, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))

    output$responses_table <- DT::renderDT({
      data <- dashboard_data()
      if (is.null(data)) return(empty_responses_table())
      format_responses_table(data$responses, data$poll$timezone[[1]])
    }, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))

    output$heatmap <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data)) return(NULL)
      if (nrow(data$options) == 0) {
        return(empty_state_ui("No proposed times", "This poll does not have proposed time slots to display."))
      }
      if (nrow(data$participants) == 0) {
        return(empty_state_ui("No responses yet", "The heatmap will appear after participants submit availability."))
      }
      build_heatmap_table(data$participants, data$options, data$heatmap, data$poll$timezone[[1]])
    })

    output$missing_expected <- shiny::renderUI({
      data <- dashboard_data()
      if (is.null(data) || nrow(data$expected) == 0) {
        return(shiny::p(class = "helper-text", "No expected participant list was entered for this poll."))
      }
      if (nrow(data$missing_expected) == 0) {
        return(shiny::div(class = "ready-box", shiny::strong("All expected participants have submitted responses.")))
      }
      shiny::tagList(
        shiny::p(class = "helper-text", paste(nrow(data$missing_expected), "expected participant(s) have not responded.")),
        DT::DTOutput(session$ns("missing_expected_table"))
      )
    })

    output$missing_expected_table <- DT::renderDT({
      data <- dashboard_data()
      if (is.null(data) || is.null(data$missing_expected) || nrow(data$missing_expected) == 0) {
        return(empty_missing_expected_table())
      }
      required_columns <- c("name", "email", "organization", "is_required")
      missing_columns <- setdiff(required_columns, names(data$missing_expected))
      if (length(missing_columns) > 0) {
        stop("Expected participant data is missing required columns.", call. = FALSE)
      }
      missing <- data$missing_expected[c("name", "email", "organization", "is_required")]
      missing$is_required <- ifelse(missing$is_required == 1L, "Required", "Optional")
      names(missing) <- c("Name", "Email", "Organization", "Required")
      missing
    }, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))

    output$download_ranked <- shiny::downloadHandler(
      filename = function() "ranked-time-slots.csv",
      content = function(file) {
        data <- dashboard_data()
        if (is.null(data)) {
          stop("Dashboard data is not available.", call. = FALSE)
        }
        utils::write.csv(format_ranked_table(data$ranked), file, row.names = FALSE, na = "")
      }
    )

    output$download_responses <- shiny::downloadHandler(
      filename = function() "participant-responses.csv",
      content = function(file) {
        data <- dashboard_data()
        if (is.null(data)) {
          stop("Dashboard data is not available.", call. = FALSE)
        }
        utils::write.csv(format_responses_table(data$responses, data$poll$timezone[[1]]), file, row.names = FALSE, na = "")
      }
    )

    shiny::observeEvent(input$close_response_link, {
      data <- dashboard_data()
      tryCatch({
        if (is.null(data)) {
          stop("Poll not found.", call. = FALSE)
        }
        close_poll(conn, data$poll$poll_id[[1]])
        refresh()
        shiny::showNotification("Response link closed.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$reopen_response_link, {
      data <- dashboard_data()
      tryCatch({
        if (is.null(data)) {
          stop("Poll not found.", call. = FALSE)
        }
        if (identical(poll_display_status(data$poll, data$options), "finalized")) {
          stop("Finalized polls cannot be reopened.", call. = FALSE)
        }

        use_latest_date <- isTRUE(input$reopen_use_latest_date)
        if (use_latest_date) {
          new_deadline <- resolve_response_deadline(FALSE, "", data$options, data$poll$timezone[[1]])
        } else {
          new_deadline <- input$reopen_deadline
          if (is.null(new_deadline) || is.na(new_deadline)) {
            stop("Choose a new expiry date or use the latest proposed meeting date.", call. = FALSE)
          }
          today_local <- as.Date(format(Sys.time(), tz = data$poll$timezone[[1]], usetz = FALSE))
          if (as.Date(new_deadline) < today_local) {
            stop("Choose today or a future date for the reopened response link.", call. = FALSE)
          }
          new_deadline <- resolve_response_deadline(TRUE, new_deadline, data$options, data$poll$timezone[[1]])
        }

        reopen_poll(conn, data$poll$poll_id[[1]], new_deadline)
        refresh()
        shiny::showNotification("Response link reopened.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    finalize_poll_server("finalize", conn, dashboard_data, refresh)
  })
}

response_link_status_message <- function(poll, options, display_status) {
  deadline <- format_deadline_label(poll_effective_deadline(poll, options))
  if (identical(display_status, "open")) {
    return(paste("Participants can respond. Current expiry:", deadline))
  }
  if (identical(display_status, "expired")) {
    return(paste("The response link expired after", deadline, "and is no longer accepting responses."))
  }
  if (identical(display_status, "closed")) {
    return("The organizer closed this response link. You can reopen it below if needed.")
  }
  if (identical(display_status, "finalized")) {
    return("The poll has been finalized. Reopening is disabled for finalized polls.")
  }
  "Response link status is unavailable."
}

dashboard_load_error_ui <- function(poll, message) {
  title <- if (!is.null(poll) && "title" %in% names(poll)) {
    ui_text(poll$title[[1]], "Poll results")
  } else {
    "Poll results"
  }
  shiny::tagList(
    page_header_ui(
      eyebrow = "Organizer dashboard",
      title = title,
      subtitle = "The dashboard could not be prepared from the current poll data."
    ),
    section_panel_ui(
      "Dashboard unavailable",
      NULL,
      shiny::div(
        class = "dashboard-error-card",
        shiny::strong("We could not load this poll dashboard."),
        shiny::p(ui_text(message, "Try refreshing the page. If the issue continues, check the app logs.")),
        shiny::p(class = "helper-text", "No participant data, tokens, or private links are shown in this error message.")
      )
    )
  )
}

response_link_controls_ui <- function(ns, display_status) {
  if (identical(display_status, "finalized")) {
    return(shiny::p(class = "helper-text", "Finalized polls cannot be reopened in this version."))
  }

  if (identical(display_status, "open")) {
    return(shiny::div(
      class = "response-link-controls",
      shiny::actionButton(ns("close_response_link"), "Close response link", class = "btn-outline-secondary")
    ))
  }

  shiny::div(
    class = "response-link-controls reopen-controls",
    shiny::checkboxInput(ns("reopen_use_latest_date"), "Use the latest proposed meeting date as the expiry", value = TRUE),
    shiny::conditionalPanel(
      condition = sprintf("!input['%s']", ns("reopen_use_latest_date")),
      shiny::dateInput(ns("reopen_deadline"), "New response deadline / link expiry", value = Sys.Date() + 7L, format = "yyyy-mm-dd")
    ),
    shiny::actionButton(ns("reopen_response_link"), "Reopen response link", class = "btn-primary")
  )
}

empty_ranked_table <- function() {
  data.frame(
    "Time option" = character(),
    "Time zone" = character(),
    "UTC start" = character(),
    "UTC end" = character(),
    "Preferred" = integer(),
    "Available" = integer(),
    "Unavailable" = integer(),
    "Missing" = integer(),
    "Score" = integer(),
    "Required conflicts" = integer(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

format_ranked_table <- function(ranked) {
  if (is.null(ranked) || nrow(ranked) == 0) {
    return(empty_ranked_table())
  }
  required_columns <- c(
    "time_option",
    "time_zone",
    "start_datetime",
    "end_datetime",
    "preferred_count",
    "available_count",
    "unavailable_count",
    "missing_count",
    "availability_score",
    "required_attendee_conflicts"
  )
  missing_columns <- setdiff(required_columns, names(ranked))
  if (length(missing_columns) > 0) {
    stop("Ranked option data is missing required columns.", call. = FALSE)
  }
  formatted <- ranked[c(
    "time_option",
    "time_zone",
    "start_datetime",
    "end_datetime",
    "preferred_count",
    "available_count",
    "unavailable_count",
    "missing_count",
    "availability_score",
    "required_attendee_conflicts"
  )]
  names(formatted) <- c(
    "Time option",
    "Time zone",
    "UTC start",
    "UTC end",
    "Preferred",
    "Available",
    "Unavailable",
    "Missing",
    "Score",
    "Required conflicts"
  )
  formatted
}

empty_responses_table <- function() {
  data.frame(
    "Name" = character(),
    "Email" = character(),
    "Time option" = character(),
    "Time zone" = character(),
    "UTC start" = character(),
    "UTC end" = character(),
    "Availability" = character(),
    "Comment" = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

format_responses_table <- function(responses, timezone) {
  if (is.null(responses) || nrow(responses) == 0) {
    return(empty_responses_table())
  }
  required_columns <- c("name", "email", "start_datetime", "end_datetime", "availability", "comment")
  missing_columns <- setdiff(required_columns, names(responses))
  if (length(missing_columns) > 0) {
    stop("Response data is missing required columns.", call. = FALSE)
  }
  formatted <- responses[c("name", "email", "start_datetime", "end_datetime", "availability", "comment")]
  formatted$local_time <- vapply(seq_len(nrow(formatted)), function(i) {
    format_readable_option_for_option(responses[i, , drop = FALSE], timezone)
  }, character(1))
  formatted$email <- vapply(formatted$email, ui_text, character(1), fallback = "Not provided")
  formatted$availability <- vapply(formatted$availability, availability_label, character(1))
  formatted$time_zone <- timezone
  formatted <- formatted[c("name", "email", "local_time", "time_zone", "start_datetime", "end_datetime", "availability", "comment")]
  names(formatted) <- c("Name", "Email", "Time option", "Time zone", "UTC start", "UTC end", "Availability", "Comment")
  formatted
}

empty_missing_expected_table <- function() {
  data.frame(
    "Name" = character(),
    "Email" = character(),
    "Organization" = character(),
    "Required" = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

decision_stat_ui <- function(label, value, emphasis = FALSE) {
  shiny::div(
    class = paste("decision-stat", if (emphasis) "decision-stat-emphasis" else ""),
    shiny::span(class = "summary-label", label),
    shiny::strong(ui_text(value, "0"))
  )
}

build_ranked_cards <- function(ranked, participant_count, timezone) {
  max_possible <- max(1L, participant_count * 2L)
  rows <- lapply(seq_len(min(5L, nrow(ranked))), function(i) {
    option <- ranked[i, , drop = FALSE]
    score_width <- min(100, round(option$availability_score[[1]] / max_possible * 100))
    shiny::div(
      class = "ranked-option-card",
      shiny::div(
        class = "ranked-option-main",
        shiny::span(class = "rank-number", paste0("#", i)),
        shiny::div(
          option_time_ui(option, timezone, heading = "h3"),
          shiny::div(
            class = "ranked-counts",
            availability_badge_ui("preferred", paste(option$preferred_count[[1]], "preferred")),
            availability_badge_ui("available", paste(option$available_count[[1]], "available")),
            availability_badge_ui("unavailable", paste(option$unavailable_count[[1]], "unavailable")),
            availability_badge_ui("missing", paste(option$missing_count[[1]], "missing"))
          )
        )
      ),
      shiny::div(
        class = "score-bar-wrap",
        shiny::div(
          class = "score-bar-label",
          shiny::span(paste("Score", option$availability_score[[1]])),
          shiny::span(paste(option$required_attendee_conflicts[[1]], "required conflict(s)"))
        ),
        shiny::div(class = "score-track", shiny::div(class = "score-fill", style = paste0("width:", score_width, "%;")))
      )
    )
  })
  shiny::div(class = "ranked-card-list", rows)
}

build_heatmap_table <- function(participants, options, heatmap, timezone) {
  if (is.null(participants) || is.null(options) || nrow(participants) == 0 || nrow(options) == 0) {
    return(empty_state_ui("No availability data", "The heatmap will appear after this poll has proposed times and participant responses."))
  }
  header <- shiny::tags$tr(
    shiny::tags$th("Participant"),
    lapply(seq_len(nrow(options)), function(i) {
      shiny::tags$th(
        shiny::span(class = "option-kicker", paste("Option", i)),
        shiny::span(
          class = "heatmap-time-label",
          format_readable_option_for_option(options[i, , drop = FALSE], timezone)
        )
      )
    })
  )
  rows <- lapply(seq_len(nrow(participants)), function(i) {
    participant <- participants[i, , drop = FALSE]
    cells <- lapply(seq_len(nrow(options)), function(j) {
      option <- options[j, , drop = FALSE]
      cell <- heatmap[
        heatmap$participant_id == participant$participant_id[[1]] &
          heatmap$option_id == option$option_id[[1]],
        ,
        drop = FALSE
      ]
      availability <- if (nrow(cell) == 0) "missing" else cell$availability[[1]]
      shiny::tags$td(
        class = paste("availability-cell", paste0("availability-", availability)),
        availability_badge_ui(availability),
        shiny::span(class = "availability-code", availability_hint(availability))
      )
    })
    shiny::tags$tr(
      shiny::tags$td(
        shiny::strong(participant$name[[1]]),
        shiny::span(class = "availability-code", participant_contact_label(participant))
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

participant_contact_label <- function(participant) {
  values <- c(
    ui_text(participant$organization[[1]], ""),
    ui_text(participant$email[[1]], "")
  )
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return("Contact not provided")
  }
  paste(values, collapse = " | ")
}
