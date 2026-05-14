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
    notification_sent <- shiny::reactiveVal(FALSE)
    authenticated_email <- shiny::reactiveVal("")
    pending_email <- shiny::reactiveVal("")
    code_requested <- shiny::reactiveVal(FALSE)
    dev_code <- shiny::reactiveVal("")

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
      existing_values <- current_response_values()
      existing_map <- response_value_map(existing_values)
      total <- nrow(options)
      answered <- sum(vapply(options$option_id, function(option_id) {
        value <- input[[paste0("availability_", option_id)]]
        if (is.null(value) && as.character(option_id) %in% names(existing_map)) {
          value <- existing_map[[as.character(option_id)]]
        }
        nzchar(value %||% "")
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

    display_timezone <- shiny::reactive({
      bundle <- poll_bundle()
      fallback <- if (!is.null(bundle$poll)) bundle$poll$timezone[[1]] else "UTC"
      resolve_display_timezone(
        override = input$timezone_override,
        detected = input$detected_timezone,
        fallback = fallback
      )
    })

    display_timezone_note_text <- shiny::reactive({
      bundle <- poll_bundle()
      if (is.null(bundle$poll)) {
        return("")
      }
      display_timezone_note(display_timezone(), input$detected_timezone, bundle$poll$timezone[[1]])
    })

    current_participant <- shiny::reactive({
      bundle <- poll_bundle()
      email <- authenticated_email()
      if (!nzchar(email) || !is.null(bundle$error)) {
        return(NULL)
      }
      get_participant_by_email(conn, bundle$poll$poll_id[[1]], email)
    })

    current_response_values <- shiny::reactive({
      participant <- current_participant()
      if (is.null(participant)) {
        return(data.frame(option_id = integer(), availability = character(), stringsAsFactors = FALSE))
      }
      get_participant_response_values(conn, participant$participant_id[[1]])
    })

    participant_results <- shiny::reactive({
      bundle <- poll_bundle()
      if (!nzchar(authenticated_email()) || !is.null(bundle$error)) {
        return(list(participants = data.frame(), responses = data.frame()))
      }
      get_participant_visible_poll_data(conn, bundle$poll$poll_id[[1]])
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
          response_success_ui(poll, notification_sent = notification_sent())
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

      if (!nzchar(authenticated_email())) {
        timezone <- display_timezone()
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Meeting availability poll",
            title = poll$title[[1]],
            subtitle = poll$description[[1]]
          ),
          participant_login_flow_ui(
            session$ns,
            poll = poll,
            options = options,
            timezone = timezone,
            code_requested = code_requested(),
            pending_email = pending_email(),
            dev_code = dev_code()
          )
        ))
      }

      participant <- current_participant()
      current_values <- current_response_values()
      visible <- participant_results()
      timezone <- display_timezone()
      participant_name <- if (is.null(participant)) "" else participant$name[[1]] %||% ""
      participant_id <- if (is.null(participant)) NA_integer_ else participant$participant_id[[1]]

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
            shiny::div(class = "respond-detail-card", detail_grid_ui(response_poll_detail_items(poll, options, timezone)))
          ),
          section_panel_ui(
            "2. Tell us who you are",
            "You are signed in for this poll. Your email is used only to let you edit your response.",
            shiny::div(
              class = "respond-identity-grid",
              shiny::div(class = "form-field", shiny::textInput(session$ns("name"), "Name", value = participant_name)),
              shiny::div(class = "form-field", shiny::tags$label("Email"), shiny::div(class = "locked-input", authenticated_email())),
              shiny::div(class = "form-field", timezone_selector_ui(session$ns, selected = input$timezone_override %||% device_timezone_choice()))
            ),
            if (nzchar(display_timezone_note_text())) {
              shiny::p(class = "helper-text", display_timezone_note_text())
            },
            shiny::actionButton(session$ns("use_different_participant_email"), "Use a different email", class = "btn-outline-secondary"),
            response_privacy_callout_ui()
          ),
          section_panel_ui(
            "3. Choose your availability",
            NULL,
            shiny::uiOutput(session$ns("completion_status")),
            build_response_calendar_ui(
              session$ns,
              options,
              timezone,
              poll_timezone = poll$timezone[[1]],
              current_values = current_values,
              visible_data = visible,
              current_participant_id = participant_id
            )
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

    shiny::observeEvent(input$request_participant_code, {
      bundle <- poll_bundle()
      if (!is.null(bundle$error)) {
        shiny::showNotification(bundle$error, type = "error")
        return()
      }
      tryCatch({
        if (!poll_accepts_responses(bundle$poll, bundle$options)) {
          stop("This poll is no longer accepting responses.", call. = FALSE)
        }
        email <- validate_email(input$participant_email, field = "Participant email")
        login <- create_participant_login_code(conn, bundle$poll$poll_id[[1]], email)
        delivery <- send_participant_magic_code_email(email, login$code, poll_title = bundle$poll$title[[1]])
        pending_email(email)
        code_requested(TRUE)
        dev_code(delivery$dev_code %||% "")
        shiny::showNotification("A poll access code has been sent.", type = "message", duration = 6)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$verify_participant_code, {
      bundle <- poll_bundle()
      if (!is.null(bundle$error)) {
        shiny::showNotification(bundle$error, type = "error")
        return()
      }
      tryCatch({
        email <- pending_email()
        if (!nzchar(email)) {
          stop("Request a poll access code first.", call. = FALSE)
        }
        if (!verify_participant_login_code(conn, bundle$poll$poll_id[[1]], email, input$participant_magic_code)) {
          stop("That code is invalid or expired. Request a new code if needed.", call. = FALSE)
        }
        authenticated_email(email)
        code_requested(FALSE)
        dev_code("")
        session$sendCustomMessage("trustedSession", list(
          scope = "participant",
          response_token = token(),
          token = issue_trusted_session_token("participant", email, poll_id = bundle$poll$poll_id[[1]])
        ))
        shiny::showNotification("Signed in to this poll.", type = "message", duration = 5)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$use_different_participant_email, {
      authenticated_email("")
      pending_email("")
      code_requested(FALSE)
      dev_code("")
      submitted(FALSE)
      notification_sent(FALSE)
      session$sendCustomMessage("clearTrustedSession", list(scope = "participant", response_token = token()))
    })

    shiny::observeEvent(input$trusted_session, {
      bundle <- poll_bundle()
      if (!is.null(bundle$error) || is.null(bundle$poll)) {
        session$sendCustomMessage("clearTrustedSession", list(scope = "participant", response_token = token()))
        return()
      }
      tryCatch({
        restored <- verify_trusted_session_token(
          input$trusted_session,
          expected_scope = "participant",
          poll_id = bundle$poll$poll_id[[1]]
        )
        if (!isTRUE(restored$valid)) {
          session$sendCustomMessage("clearTrustedSession", list(scope = "participant", response_token = token()))
          return()
        }
        authenticated_email(restored$email)
        pending_email("")
        code_requested(FALSE)
        dev_code("")
      }, error = function(e) {
        session$sendCustomMessage("clearTrustedSession", list(scope = "participant", response_token = token()))
      })
    }, ignoreInit = TRUE)

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
        if (!nzchar(authenticated_email())) {
          stop("Sign in with your email before sending availability.", call. = FALSE)
        }
        existing_participant <- current_participant()
        existing_values <- current_response_values()
        existing_map <- stats::setNames(as.character(existing_values$availability), as.character(existing_values$option_id))
        participant <- list(
          name = sanitize_text(input$name, max_chars = 160, required = TRUE, field = "Name"),
          email = authenticated_email(),
          organization = ""
        )
        response_values <- data.frame(
          option_id = options$option_id,
          availability = vapply(options$option_id, function(option_id) {
            input_value <- input[[paste0("availability_", option_id)]]
            if (is.null(input_value) && as.character(option_id) %in% names(existing_map)) {
              input_value <- existing_map[[as.character(option_id)]]
            }
            validate_availability(input_value)
          }, character(1)),
          stringsAsFactors = FALSE
        )
        comment <- sanitize_text(input$comments, max_chars = 2000)
        submit_poll_response(conn, poll$poll_id[[1]], participant, response_values, comment)
        response_link <- build_app_link(session, "respond", poll$response_token[[1]])
        organizer_link <- build_organizer_poll_link(session, poll$response_token[[1]])
        email_sent <- tryCatch({
          send_response_submission_notifications(
            poll = poll,
            participant_name = participant$name,
            participant_email = authenticated_email(),
            response_link = response_link,
            organizer_link = organizer_link,
            action = if (is.null(existing_participant)) "submitted" else "updated"
          )
          TRUE
        }, error = function(e) {
          warning(
            paste0("Response notification email failed after response save for poll_id=", poll$poll_id[[1]]),
            call. = FALSE
          )
          FALSE
        })
        notification_sent(email_sent)
        submitted(TRUE)
        if (isTRUE(email_sent)) {
          shiny::showNotification("Your availability has been saved and confirmation emails were sent.", type = "message")
        } else {
          shiny::showNotification("Your availability was saved, but confirmation emails could not be sent.", type = "warning", duration = 8)
        }
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })
  })
}

participant_login_flow_ui <- function(ns, poll, options = NULL, timezone = poll$timezone[[1]], code_requested, pending_email, dev_code) {
  if (isTRUE(code_requested)) {
    return(shiny::div(
      class = "respond-flow",
      section_panel_ui(
        "Sign in to this poll",
        paste("Enter the 6-digit code sent to", pending_email),
        shiny::textInput(ns("participant_magic_code"), "6-digit code", placeholder = "123456"),
        if (nzchar(dev_code %||% "")) {
          shiny::div(
            class = "dev-code-panel",
            shiny::strong("Development poll code"),
            shiny::p(dev_code)
          )
        },
        shiny::div(
          class = "button-row",
          shiny::actionButton(ns("verify_participant_code"), "Open poll", class = "btn-primary"),
          shiny::actionButton(ns("use_different_participant_email"), "Use a different email", class = "btn-outline-secondary")
        )
      )
    ))
  }

  shiny::div(
    class = "respond-flow",
    section_panel_ui(
      "Sign in to respond",
      "Enter your email to receive a short code. This lets you edit your response later and view other participants' availability.",
      detail_grid_ui(response_poll_detail_items(poll, options, timezone)),
      shiny::textInput(ns("participant_email"), "Email", placeholder = "you@example.org"),
      response_notice_ui(
        "Participant visibility",
        "After sign-in, other verified participants can see your name and availability. Emails and comments remain private to the organizer.",
        "privacy"
      ),
      shiny::actionButton(ns("request_participant_code"), "Send poll access code", class = "btn-primary")
    )
  )
}

build_response_calendar_ui <- function(ns, options, timezone, poll_timezone = timezone, current_values = data.frame(), visible_data = NULL, current_participant_id = NA_integer_) {
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
      response_week_calendar_ui(
        ns,
        week_options,
        timezone,
        poll_timezone = poll_timezone,
        current_values = current_values,
        visible_data = visible_data,
        current_participant_id = current_participant_id
      )
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

response_week_calendar_ui <- function(ns, week_options, timezone, poll_timezone = timezone, current_values = data.frame(), visible_data = NULL, current_participant_id = NA_integer_) {
  week_options <- week_options[order(week_options$start_datetime), , drop = FALSE]
  current_map <- response_value_map(current_values)
  header <- shiny::tags$tr(
    shiny::tags$th(class = "response-board-participant-header", "Participant"),
    lapply(seq_len(nrow(week_options)), function(i) {
      response_board_option_header_ui(week_options[i, , drop = FALSE], timezone, visible_data, poll_timezone = poll_timezone)
    })
  )
  response_cells <- lapply(seq_len(nrow(week_options)), function(i) {
    option <- week_options[i, , drop = FALSE]
    option_label <- paste(option$local_date_label[[1]], response_board_time_label(option, timezone, poll_timezone = poll_timezone))
    value <- current_map[[as.character(option$option_id[[1]])]] %||% "pending"
    shiny::tags$td(
      class = "response-board-answer-cell",
      availability_cycle_button_ui(ns(paste0("availability_", option$option_id[[1]])), option_label, value = value)
    )
  })
  other_rows <- response_board_participant_rows_ui(week_options, visible_data, current_participant_id)

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
          ),
          other_rows
        )
      )
    )
  )
}

response_board_option_header_ui <- function(option, timezone, visible_data = NULL, poll_timezone = timezone) {
  date_value <- as.Date(option$local_date[[1]])
  shiny::tags$th(
    class = "response-board-option-header",
    shiny::span(class = "response-board-favorite", "☆"),
    shiny::span(class = "response-board-date", format_readable_date(date_value, include_year = FALSE, ordinal = FALSE)),
    shiny::span(class = "response-board-time", response_board_time_label(option, timezone, poll_timezone = poll_timezone)),
    shiny::span(class = "response-board-duration", response_board_duration_label(option)),
    response_board_counts_ui(option$option_id[[1]], visible_data)
  )
}

response_value_map <- function(values) {
  if (is.null(values) || nrow(values) == 0 || !"option_id" %in% names(values) || !"availability" %in% names(values)) {
    return(list())
  }
  as.list(stats::setNames(as.character(values$availability), as.character(values$option_id)))
}

response_board_counts_ui <- function(option_id, visible_data = NULL) {
  responses <- visible_data$responses %||% data.frame()
  option_responses <- if (nrow(responses) > 0 && "option_id" %in% names(responses)) {
    responses[responses$option_id == option_id, , drop = FALSE]
  } else {
    data.frame()
  }
  count_for <- function(value) {
    if (nrow(option_responses) == 0) 0L else sum(option_responses$availability == value, na.rm = TRUE)
  }
  shiny::span(
    class = "response-board-counts",
    shiny::span(paste0("★ ", count_for("preferred"))),
    shiny::span(paste0("✓ ", count_for("available"))),
    shiny::span(paste0("× ", count_for("unavailable")))
  )
}

response_board_participant_rows_ui <- function(week_options, visible_data = NULL, current_participant_id = NA_integer_) {
  participants <- visible_data$participants %||% data.frame()
  responses <- visible_data$responses %||% data.frame()
  if (nrow(participants) == 0) {
    return(NULL)
  }
  current_participant_id <- suppressWarnings(as.integer(current_participant_id))
  if (!is.na(current_participant_id)) {
    participants <- participants[participants$participant_id != current_participant_id, , drop = FALSE]
  }
  if (nrow(participants) == 0) {
    return(NULL)
  }
  lapply(seq_len(nrow(participants)), function(i) {
    participant <- participants[i, , drop = FALSE]
    shiny::tags$tr(
      shiny::tags$th(
        class = "response-board-participant-cell",
        shiny::div(
          class = "response-board-participant-content",
          shiny::span(class = "participant-avatar-mini muted-avatar", participant_initials(participant$name[[1]])),
          shiny::div(
            shiny::strong(participant$name[[1]]),
            shiny::span("Submitted")
          )
        )
      ),
      lapply(seq_len(nrow(week_options)), function(j) {
        option <- week_options[j, , drop = FALSE]
        availability <- participant_availability_for_option(responses, participant$participant_id[[1]], option$option_id[[1]])
        shiny::tags$td(
          class = paste("response-board-answer-cell", "response-board-answer-readonly"),
          availability_cycle_button_ui(
            input_id = paste0("readonly_", participant$participant_id[[1]], "_", option$option_id[[1]]),
            label = paste(participant$name[[1]], option$local_date_label[[1]]),
            value = availability,
            readonly = TRUE
          )
        )
      })
    )
  })
}

participant_availability_for_option <- function(responses, participant_id, option_id) {
  if (is.null(responses) || nrow(responses) == 0) {
    return("pending")
  }
  match <- responses[responses$participant_id == participant_id & responses$option_id == option_id, , drop = FALSE]
  if (nrow(match) == 0) "pending" else match$availability[[1]]
}

participant_initials <- function(name) {
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name)) {
    return("?")
  }
  parts <- strsplit(name, "\\s+")[[1]]
  initials <- paste(substr(parts, 1, 1), collapse = "")
  toupper(substr(initials, 1, 2))
}

response_board_time_label <- function(option, timezone, poll_timezone = timezone) {
  if (isTRUE(response_board_is_all_day(option)) &&
      is_local_midnight_all_day_interval(option$start_datetime[[1]], option$end_datetime[[1]], timezone)) {
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
