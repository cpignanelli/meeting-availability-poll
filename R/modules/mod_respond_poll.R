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
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Response link",
            title = "We could not open this poll",
            subtitle = "Check that the link was copied correctly or ask the organizer to resend it."
          ),
          empty_state_ui("Invalid response link", bundle$error)
        ))
      }
      poll <- bundle$poll
      options <- bundle$options
      if (submitted()) {
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Availability saved",
            title = "Availability sent",
            subtitle = "Your availability has been saved. The organizer will confirm the final meeting time after reviewing responses."
          ),
          response_success_ui(poll)
        ))
      }
      display_status <- poll_display_status(poll, options)
      if (identical(display_status, "finalized")) {
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Meeting availability poll",
            title = poll$title[[1]],
            subtitle = "This poll is no longer accepting responses."
          ),
          response_contact_state_ui("This poll has been finalized", finalized_poll_contact_message(poll), poll)
        ))
      }
      if (identical(display_status, "expired")) {
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Meeting availability poll",
            title = poll$title[[1]],
            subtitle = "This response link is no longer accepting availability."
          ),
          closed_poll_contact_ui(poll, "This booking link has expired")
        ))
      }
      if (!identical(display_status, "open")) {
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Meeting availability poll",
            title = poll$title[[1]],
            subtitle = "This response link is currently closed."
          ),
          closed_poll_contact_ui(poll, "This booking link is closed")
        ))
      }

      shiny::tagList(
        page_header_ui(
          eyebrow = "Meeting availability poll",
          title = poll$title[[1]],
          subtitle = poll$description[[1]]
        ),
        shiny::div(
          class = "respond-flow",
          section_panel_ui(
            "1. Review meeting details",
            NULL,
            shiny::div(class = "respond-detail-card", detail_grid_ui(response_poll_detail_items(poll, options)))
          ),
          section_panel_ui(
            "2. Tell us who you are",
            "Email is optional. If you include it and respond again with the same email, your latest response replaces the earlier one.",
            shiny::div(
              class = "respond-identity-grid",
              shiny::div(class = "form-field", shiny::textInput(session$ns("name"), "Name")),
              shiny::div(class = "form-field", shiny::textInput(session$ns("email"), "Email (optional)"))
            ),
            response_privacy_callout_ui()
          ),
          section_panel_ui(
            "3. Choose your availability",
            NULL,
            shiny::uiOutput(session$ns("completion_status")),
            build_response_calendar_ui(session$ns, options, poll$timezone[[1]])
          ),
          section_panel_ui(
            "Send your response",
            NULL,
            shiny::div(
              class = "response-submit-panel",
              shiny::textAreaInput(session$ns("comments"), "Optional comments", rows = 3, placeholder = "Anything the organizer should know?"),
              response_notice_ui(
                "Before you send",
                "Final meeting confirmation will follow from the organizer.",
                "subtle"
              ),
              shiny::actionButton(session$ns("submit_response"), "Send availability", class = "btn-primary btn-lg response-submit-button")
            )
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
        if (!poll_accepts_responses(poll, options)) {
          stop("This poll is no longer accepting responses.", call. = FALSE)
        }
        participant <- list(
          name = sanitize_text(input$name, max_chars = 160, required = TRUE, field = "Name"),
          email = input$email %||% "",
          organization = ""
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
        shiny::showNotification("Your availability has been saved.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })
  })
}

build_response_calendar_ui <- function(ns, options, timezone) {
  calendar <- response_calendar_data(options, timezone)
  if (nrow(calendar) == 0) {
    return(empty_state_ui("No proposed times", "This poll does not have any proposed meeting times."))
  }

  week_keys <- unique(calendar$week_key)
  default_week <- default_response_week(calendar)
  panels <- lapply(week_keys, function(week_key) {
    week_options <- calendar[calendar$week_key == week_key, , drop = FALSE]
    shiny::tabPanel(
      title = week_options$week_label[[1]],
      value = week_key,
      response_week_calendar_ui(ns, week_options, timezone)
    )
  })

  shiny::div(
    class = "response-calendar",
    shiny::div(
      class = "response-calendar-toolbar",
      shiny::div(
        class = "response-calendar-note",
        shiny::strong("Click each cell to choose your response."),
        shiny::span(paste("Times shown in", unique(calendar$timezone_label)[[1]]))
      ),
      response_board_legend_ui()
    ),
    do.call(
      shiny::tabsetPanel,
      c(list(type = "pills", selected = default_week, id = ns("response_week_tabs")), panels)
    )
  )
}

response_board_legend_ui <- function() {
  states <- response_availability_cycle()
  shiny::div(
    class = "response-board-legend",
    lapply(states, function(state) {
      shiny::span(
        class = paste("response-board-legend-item", paste0("availability-state-", state)),
        shiny::span(class = "availability-cycle-icon", availability_icon(state)),
        shiny::span(availability_short_label(state))
      )
    })
  )
}

response_calendar_data <- function(options, timezone) {
  if (is.null(options) || nrow(options) == 0) {
    return(data.frame())
  }
  starts <- parse_utc_timestamp(options$start_datetime)
  ends <- parse_utc_timestamp(options$end_datetime)
  local_dates <- as.Date(format(starts, "%Y-%m-%d", tz = timezone))
  week_starts <- calendar_week_start(local_dates)
  is_all_day <- as.numeric(difftime(ends, starts, units = "mins")) >= 1440
  local_times <- ifelse(is_all_day, "all_day", format(starts, "%H:%M", tz = timezone))
  local_time_labels <- vapply(seq_along(starts), function(i) {
    if (isTRUE(is_all_day[[i]])) "All day" else format_readable_clock(starts[[i]], timezone)
  }, character(1))
  local_date_labels <- vapply(seq_along(local_dates), function(i) {
    format_readable_date(local_dates[i], include_year = FALSE, ordinal = FALSE)
  }, character(1))
  timezone_labels <- vapply(seq_along(starts), function(i) {
    format_timezone_abbreviation(starts[i], timezone)
  }, character(1))
  week_labels <- vapply(seq_along(week_starts), function(i) {
    calendar_week_label(week_starts[i])
  }, character(1))
  data.frame(
    options,
    local_date = local_dates,
    local_time = local_times,
    local_time_label = local_time_labels,
    local_date_label = local_date_labels,
    week_start = week_starts,
    week_key = paste0("week_", format(week_starts, "%Y_%m_%d")),
    week_label = week_labels,
    timezone_label = timezone_labels,
    stringsAsFactors = FALSE
  )
}

default_response_week <- function(calendar) {
  today <- Sys.Date()
  upcoming <- calendar[calendar$local_date >= today, , drop = FALSE]
  if (nrow(upcoming) > 0) {
    return(upcoming$week_key[[1]])
  }
  calendar$week_key[[1]]
}

response_week_calendar_ui <- function(ns, week_options, timezone) {
  week_options <- week_options[order(week_options$start_datetime), , drop = FALSE]
  header <- shiny::tags$tr(
    shiny::tags$th(class = "response-board-participant-header", "Participant"),
    lapply(seq_len(nrow(week_options)), function(i) {
      response_board_option_header_ui(week_options[i, , drop = FALSE], timezone)
    })
  )
  response_cells <- lapply(seq_len(nrow(week_options)), function(i) {
    option <- week_options[i, , drop = FALSE]
    option_label <- paste(option$local_date_label[[1]], response_board_time_label(option, timezone))
    shiny::tags$td(
      class = "response-board-answer-cell",
      availability_cycle_button_ui(ns(paste0("availability_", option$option_id[[1]])), option_label)
    )
  })

  shiny::div(
    class = "response-calendar-board",
    shiny::div(class = "response-scroll-hint", "Scroll sideways to see all proposed times."),
    shiny::div(
      class = "response-calendar-scroll",
      shiny::tags$table(
        class = "response-calendar-table response-board-table",
        shiny::tags$thead(header),
        shiny::tags$tbody(
          shiny::tags$tr(
            shiny::tags$th(
              class = "response-board-participant-cell",
              shiny::div(
                class = "response-board-participant-content",
                shiny::span(class = "participant-avatar-mini", "Y"),
                shiny::div(
                  shiny::strong("You"),
                  shiny::span("Your response")
                )
              )
            ),
            response_cells
          )
        )
      )
    ),
    response_mobile_date_cards_ui(ns, week_options, timezone)
  )
}

response_board_option_header_ui <- function(option, timezone) {
  date_value <- as.Date(option$local_date[[1]])
  shiny::tags$th(
    class = "response-board-option-header",
    shiny::span(class = "response-board-favorite", "☆"),
    shiny::span(class = "response-board-date", format_readable_date(date_value, include_year = FALSE, ordinal = FALSE)),
    shiny::span(class = "response-board-time", response_board_time_label(option, timezone)),
    shiny::span(class = "response-board-duration", response_board_duration_label(option))
  )
}

response_board_time_label <- function(option, timezone) {
  if (isTRUE(response_board_is_all_day(option))) {
    return("All day")
  }
  format_readable_time_range(option$start_datetime[[1]], option$end_datetime[[1]], timezone)
}

response_board_duration_label <- function(option) {
  format_duration_label(response_board_duration_minutes(option))
}

response_board_duration_minutes <- function(option) {
  start <- parse_utc_timestamp(option$start_datetime[[1]])
  end <- parse_utc_timestamp(option$end_datetime[[1]])
  as.integer(round(as.numeric(difftime(end, start, units = "mins"))))
}

response_board_is_all_day <- function(option) {
  response_board_duration_minutes(option) >= 1440L ||
    is_all_day_option_label(option$display_label[[1]] %||% "")
}

response_mobile_date_cards_ui <- function(ns, week_options, timezone) {
  dates <- unique(week_options$local_date)
  cards <- lapply(dates, function(date_value) {
    day_options <- week_options[week_options$local_date == date_value, , drop = FALSE]
    day_options <- day_options[order(day_options$local_time), , drop = FALSE]
    shiny::div(
      class = "response-date-card",
      shiny::h3(day_options$local_date_label[[1]]),
      lapply(seq_len(nrow(day_options)), function(i) {
        option <- day_options[i, , drop = FALSE]
        shiny::div(
          class = "response-date-option",
          shiny::div(
            class = "response-date-option-main",
            shiny::strong(response_board_time_label(option, timezone)),
            shiny::span(response_board_duration_label(option))
          ),
          availability_cycle_button_ui(
            ns(paste0("availability_", option$option_id[[1]])),
            paste(option$local_date_label[[1]], response_board_time_label(option, timezone))
          )
        )
      })
    )
  })
  shiny::div(class = "response-mobile-date-cards", cards)
}
