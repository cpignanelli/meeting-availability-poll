create_poll_ui <- function(id, embedded = FALSE, authenticated = FALSE) {
  ns <- shiny::NS(id)
  content <- shiny::tagList(
    page_header_ui(
      eyebrow = "Organizer setup",
      title = "Create a meeting availability poll",
      subtitle = "Create one unique booking invite for this meeting. You can create additional polls for other meetings or groups."
    ),
    shiny::div(
      class = "create-flow",
      section_panel_ui(
        "1. Meeting basics",
        "Keep this short and recognizable. Participants will see these details before choosing availability.",
        shiny::div(
          class = "meeting-basics-grid",
          shiny::div(class = "form-field", shiny::textInput(ns("title"), "Meeting title", placeholder = "Performance working group planning meeting")),
          shiny::div(
            class = "form-field",
            shiny::selectInput(
              ns("timezone"),
              "Time zone",
              choices = OlsonNames(),
              selected = if ("America/Toronto" %in% OlsonNames()) "America/Toronto" else Sys.timezone()
            )
          ),
          shiny::div(class = "form-field form-span-2", shiny::textAreaInput(ns("description"), "Description/context", rows = 4, placeholder = "Briefly describe the purpose of the meeting.")),
          shiny::div(class = "form-field", shiny::textInput(ns("organizer_name"), "Organizer name")),
          shiny::uiOutput(ns("organizer_email_ui")),
          shiny::div(class = "form-field", shiny::selectInput(ns("location_type"), "Location type", choices = c("To be determined", "Virtual", "In person", "Hybrid"))),
          shiny::div(class = "form-field", shiny::textInput(ns("location_details"), "Optional location or virtual meeting details"))
        ),
        if (!isTRUE(authenticated)) shiny::div(
          class = "remember-organizer-card",
          shiny::div(
            shiny::checkboxInput(ns("remember_organizer"), "Remember organizer name and email on this device", value = FALSE),
            shiny::p(class = "helper-text", "Only these two fields are saved in this browser. Poll links, tokens, participant data, and availability are never stored here.")
          ),
          shiny::actionButton(ns("clear_saved_organizer"), "Clear saved details", class = "btn-outline-secondary btn-sm")
        ),
        shiny::div(
          class = "response-settings-card response-settings-card-optional",
          shiny::div(
            class = "response-settings-copy",
            shiny::strong("Response link settings"),
            shiny::p("By default, the response link closes after the last proposed meeting date.")
          ),
          shiny::div(
            class = "expiry-control",
            shiny::div(
              class = "expiry-toggle-row",
              shiny::checkboxInput(ns("use_response_deadline"), "Set an earlier response deadline", value = FALSE)
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']", ns("use_response_deadline")),
              shiny::dateInput(ns("response_deadline"), "Earlier response deadline", value = Sys.Date(), format = "yyyy-mm-dd")
            ),
            shiny::uiOutput(ns("expiry_summary"))
          )
        )
      ),
      section_panel_ui(
        "2. Proposed times",
        "Choose a duration, then select times on the calendar. Past times are disabled. Weekends are available.",
        shiny::div(
          class = "duration-selector",
          shiny::tags$label(class = "control-label", "Duration"),
          shiny::radioButtons(
            ns("duration_choice"),
            label = NULL,
            choices = c(
              "15 min" = "15",
              "30 min" = "30",
              "45 min" = "45",
              "60 min" = "60",
              "90 min" = "90",
              "120 min" = "120",
              "All day" = "all_day",
              "Custom duration" = "custom"
            ),
            selected = "60",
            inline = TRUE
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("duration_choice")),
            shiny::numericInput(ns("custom_duration_minutes"), "Custom duration in minutes", value = 60, min = 5, max = 1440, step = 5)
          ),
          shiny::uiOutput(ns("duration_summary"))
        ),
        shiny::div(
          class = "calendar-toolbar",
          shiny::div(
            class = "calendar-nav",
            shiny::actionButton(ns("previous_week"), "\u2039", class = "btn-outline-secondary calendar-nav-button", title = "Previous week"),
            shiny::actionButton(ns("next_week"), "\u203a", class = "btn-outline-secondary calendar-nav-button", title = "Next week"),
            shiny::actionButton(ns("today_week"), "Today", class = "btn-outline-secondary")
          ),
          shiny::uiOutput(ns("week_label")),
          shiny::uiOutput(ns("timezone_label"))
        ),
        shiny::uiOutput(ns("calendar_grid")),
        shiny::uiOutput(ns("selected_times_summary"))
      ),
      section_panel_ui(
        "3. Review and create",
        "Create the poll when the essentials are ready. The private organizer link is shown once after creation.",
        shiny::uiOutput(ns("create_readiness")),
        privacy_notice_ui(compact = TRUE),
        shiny::actionButton(ns("create_poll"), "Create poll and generate links", class = "btn-primary btn-lg create-submit"),
        shiny::uiOutput(ns("created_links"))
      )
    )
  )

  if (isTRUE(embedded)) {
    return(shiny::div(class = "organizer-shell embedded-create-shell", content))
  }

  shiny::div(
    class = "app-shell organizer-shell",
    app_topbar_ui("Create"),
    content
  )
}

create_poll_server <- function(id, conn, organizer_email = NULL, on_created = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    organizer_email <- organizer_email %||% shiny::reactive("")
    selected_slots <- shiny::reactiveVal(empty_selected_slots())
    week_start <- shiny::reactiveVal(calendar_week_start(Sys.Date()))
    created <- shiny::reactiveVal(NULL)
    last_locked_organizer_email <- shiny::reactiveVal(NULL)

    locked_organizer_email <- shiny::reactive({
      value <- organizer_email()
      if (is.null(value) || !nzchar(value)) {
        return("")
      }
      validate_email(value, field = "Organizer email")
    })

    active_organizer_email <- function() {
      locked <- locked_organizer_email()
      if (nzchar(locked)) {
        return(locked)
      }
      validate_email(input$organizer_email, field = "Organizer email")
    }

    output$organizer_email_ui <- shiny::renderUI({
      locked <- tryCatch(locked_organizer_email(), error = function(e) "")
      if (nzchar(locked)) {
        return(shiny::div(
          class = "form-field locked-email-field",
          shiny::tags$label("Organizer email"),
          shiny::div(class = "locked-input", locked),
          shiny::p(class = "helper-text", "This poll will be saved to your signed-in organizer workspace.")
        ))
      }
      shiny::div(class = "form-field", shiny::textInput(session$ns("organizer_email"), "Organizer email"))
    })

    shiny::observe({
      locked <- tryCatch(locked_organizer_email(), error = function(e) "")
      if (!identical(locked, last_locked_organizer_email())) {
        last_locked_organizer_email(locked)
        created(NULL)
        selected_slots(empty_selected_slots())
      }
    })

    current_duration <- shiny::reactive({
      resolve_duration_minutes(input$duration_choice %||% "60", input$custom_duration_minutes)
    })

    collect_time_options <- function(timezone, duration) {
      selected_slots_to_options(selected_slots(), duration, timezone)
    }

    send_calendar_selection <- function() {
      slots <- shiny::isolate(selected_slots())
      session$sendCustomMessage(
        "calendarSelection",
        list(
          container_id = session$ns("calendar_grid_scroll"),
          selected = if (nrow(slots) == 0) character() else slot_key(slots$date, slots$start_time)
        )
      )
    }

    send_calendar_scroll <- function(all_day_mode = FALSE) {
      if (!isTRUE(all_day_mode)) {
        session$sendCustomMessage(
          "calendarScrollToTime",
          list(container_id = session$ns("calendar_grid_scroll"), time = "08:00")
        )
      }
    }

    clear_selected_slots <- function(message = NULL) {
      if (nrow(selected_slots()) > 0) {
        selected_slots(empty_selected_slots())
        send_calendar_selection()
        created(NULL)
        if (nzchar(message %||% "")) {
          shiny::showNotification(message, type = "message", duration = 4)
        }
      }
    }

    shiny::observeEvent(input$duration_choice, {
      clear_selected_slots("Selected times were cleared because the duration mode changed.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$previous_week, {
      week_start(week_start() - 7L)
    })

    shiny::observeEvent(input$next_week, {
      week_start(week_start() + 7L)
    })

    shiny::observeEvent(input$today_week, {
      week_start(calendar_week_start(Sys.Date()))
    })

    normalize_slot_changes_input <- function(value) {
      changes <- value$changes %||% value
      if (is.null(changes) || length(changes) == 0) {
        return(data.frame(key = character(), selected = logical(), stringsAsFactors = FALSE))
      }
      if (is.data.frame(changes)) {
        return(changes[c("key", "selected")])
      }
      rows <- lapply(changes, function(change) {
        data.frame(
          key = as.character(change$key %||% ""),
          selected = coerce_slot_selected(change$selected),
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    }

    shiny::observeEvent(input$slot_changes, {
      value <- input$slot_changes
      timezone <- tryCatch(validate_timezone(input$timezone), error = function(e) NULL)
      duration <- tryCatch(current_duration(), error = function(e) NULL)
      if (is.null(timezone) || is.null(duration)) {
        return(NULL)
      }
      result <- tryCatch(
        apply_selected_slot_changes(selected_slots(), normalize_slot_changes_input(value), timezone, duration),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        shiny::showNotification(safe_error_message(result), type = "error", duration = 6)
        send_calendar_selection()
        return(NULL)
      }
      if (result$ignored_past > 0) {
        shiny::showNotification("Choose future proposed times.", type = "warning", duration = 5)
        send_calendar_selection()
      }
      if (isTRUE(result$changed)) {
        selected_slots(result$slots)
        created(NULL)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$remove_selected_slot, {
      key <- as.character(input$remove_selected_slot %||% "")
      slots <- selected_slots()
      if (nrow(slots) == 0) {
        return(NULL)
      }
      keys <- slot_key(slots$date, slots$start_time)
      slots <- slots[keys != key, , drop = FALSE]
      rownames(slots) <- NULL
      selected_slots(slots)
      created(NULL)
      send_calendar_selection()
    })

    output$duration_summary <- shiny::renderUI({
      duration <- tryCatch(current_duration(), error = function(e) e)
      if (inherits(duration, "error")) {
        return(validation_summary_ui(safe_error_message(duration)))
      }
      label <- if (duration == 1440L && identical(input$duration_choice, "all_day")) {
        "All-day meeting options will be added from the all-day row."
      } else {
        paste("Each selected start time creates a", duration, "minute option.")
      }
      shiny::p(class = "helper-text duration-summary", label)
    })

    output$week_label <- shiny::renderUI({
      shiny::div(
        class = "calendar-week-label",
        shiny::strong(calendar_week_label(week_start()))
      )
    })

    output$timezone_label <- shiny::renderUI({
      shiny::div(
        class = "calendar-timezone-label",
        validate_timezone(input$timezone),
        shiny::span(class = "helper-text", "local time")
      )
    })

    output$calendar_grid <- shiny::renderUI({
      tryCatch({
        all_day_mode <- identical(input$duration_choice, "all_day")
        grid <- build_calendar_grid_ui(
          ns = session$ns,
          week_start = week_start(),
          timezone = validate_timezone(input$timezone),
          duration_minutes = current_duration(),
          selected_slots = shiny::isolate(selected_slots()),
          all_day_mode = all_day_mode
        )
        session$onFlushed(function() {
          send_calendar_selection()
          send_calendar_scroll(all_day_mode)
        }, once = TRUE)
        grid
      }, error = function(e) {
        validation_summary_ui(safe_error_message(e))
      })
    })

    output$selected_times_summary <- shiny::renderUI({
      slots <- selected_slots()
      if (nrow(slots) == 0) {
        return(empty_state_ui("No times selected", "Choose one or more future time slots from the calendar."))
      }

      timezone <- validate_timezone(input$timezone)
      duration <- current_duration()
      ordered_slots <- order_selected_slots(slots, timezone)
      options <- selected_slots_to_options(ordered_slots, duration, timezone)
      rows <- lapply(seq_len(nrow(options)), function(i) {
        key <- slot_key(ordered_slots$date[[i]], ordered_slots$start_time[[i]])
        shiny::tags$tr(
          shiny::tags$td(i),
          shiny::tags$td(option_time_ui(options[i, , drop = FALSE], timezone, heading = "strong")),
          shiny::tags$td(
            shiny::tags$button(
              type = "button",
              class = "selected-time-remove",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                session$ns("remove_selected_slot"),
                key
              ),
              "Remove"
            )
          )
        )
      })

      shiny::div(
        class = "selected-times-panel",
        shiny::div(
          class = "selected-times-heading",
          shiny::strong("Selected meeting options"),
          shiny::span(class = "helper-text", paste(nrow(options), "option", if (nrow(options) == 1L) "selected" else "selected"))
        ),
        shiny::div(
          class = "preview-table-wrap",
          shiny::tags$table(
            class = "preview-table selected-times-table",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("#"),
                shiny::tags$th("Proposed time"),
                shiny::tags$th("Action")
              )
            ),
            shiny::tags$tbody(rows)
          )
        )
      )
    })

    output$expiry_summary <- shiny::renderUI({
      timezone <- tryCatch(validate_timezone(input$timezone), error = function(e) NULL)
      duration <- tryCatch(current_duration(), error = function(e) NULL)
      options <- tryCatch({
        if (is.null(timezone) || is.null(duration)) {
          NULL
        } else {
          collect_time_options(timezone, duration)
        }
      }, error = function(e) NULL)

      if (is.null(options) || nrow(options) == 0) {
        return(shiny::p(class = "helper-text", "Select proposed times to calculate the automatic link expiry."))
      }

      last_date <- latest_option_date(options, timezone)
      effective_deadline <- tryCatch(
        resolve_response_deadline(isTRUE(input$use_response_deadline), input$response_deadline, options, timezone),
        error = function(e) e
      )
      if (inherits(effective_deadline, "error")) {
        return(validation_summary_ui(safe_error_message(effective_deadline)))
      }

      copy <- if (isTRUE(input$use_response_deadline)) {
        manual <- as.Date(input$response_deadline)
        if (!is.na(manual) && manual > last_date) {
          paste("The chosen deadline is after the final proposed date, so the link will expire on", format_deadline_label(effective_deadline), ".")
        } else {
          paste("The response link will expire on", format_deadline_label(effective_deadline), ".")
        }
      } else {
        paste("Automatic expiry:", format_deadline_label(effective_deadline), "(the final proposed meeting date).")
      }
      shiny::p(class = "helper-text expiry-summary", copy)
    })

    output$create_readiness <- shiny::renderUI({
      missing <- character()
      if (!nzchar(trimws(input$title %||% ""))) missing <- c(missing, "Meeting title")
      if (!nzchar(trimws(input$organizer_name %||% ""))) missing <- c(missing, "Organizer name")
      organizer_email_value <- tryCatch(active_organizer_email(), error = function(e) "")
      if (!nzchar(organizer_email_value)) missing <- c(missing, "Organizer email")

      valid_times <- tryCatch({
        timezone <- validate_timezone(input$timezone)
        duration <- current_duration()
        options <- collect_time_options(timezone, duration)
        nrow(options)
      }, error = function(e) 0L)

      if (valid_times == 0L) {
        missing <- c(missing, "At least one future proposed time")
      }

      if (length(missing) > 0) {
        return(shiny::div(
          class = "readiness-list",
          shiny::p(class = "helper-text", "Before creating the poll:"),
          shiny::tags$ul(lapply(missing, shiny::tags$li))
        ))
      }

      shiny::div(
        class = "ready-box",
        shiny::strong("Ready to create"),
        shiny::p(paste(valid_times, "proposed time", if (valid_times == 1L) "slot" else "slots", "will be included."))
      )
    })

    output$created_links <- shiny::renderUI({
      info <- created()
      if (is.null(info)) {
        return(NULL)
      }
      shiny::div(
        class = "created-links-inline",
        shiny::h3("Poll created"),
        shiny::p(class = "helper-text", "This is one unique booking invite. Share the public response link with participants, and keep the private organizer link restricted to the organizer."),
        shiny::div(
          class = "link-box",
          copy_field_ui(session$ns("response_link_copy"), "Public response link", info$response_link, "Participants can only submit availability from this link."),
          copy_field_ui(session$ns("admin_link_copy"), "Private organizer link", info$admin_link, sensitive = TRUE),
          shiny::p(class = "helper-text", "The private link is shown here as a raw token only once. The app stores only its hash. Create a new poll if you need another live booking invite.")
        )
      )
    })

    shiny::observeEvent(input$create_poll, {
      tryCatch({
        title <- sanitize_text(input$title, max_chars = 180, required = TRUE, field = "Meeting title")
        duration <- current_duration()
        timezone <- validate_timezone(input$timezone)
        description <- sanitize_text(input$description, max_chars = 2000)
        organizer_name <- sanitize_text(input$organizer_name, max_chars = 160, required = TRUE, field = "Organizer name")
        organizer_email <- active_organizer_email()
        location_type <- sanitize_text(input$location_type, max_chars = 80)
        location_details <- sanitize_text(input$location_details, max_chars = 1000)

        options <- collect_time_options(timezone, duration)
        if (nrow(options) == 0) {
          stop("Add at least one proposed meeting time.", call. = FALSE)
        }
        if (any(is_past_datetime(parse_utc_timestamp(options$start_datetime)))) {
          stop("All proposed meeting times must be in the future.", call. = FALSE)
        }

        response_deadline <- resolve_response_deadline(
          isTRUE(input$use_response_deadline),
          input$response_deadline,
          options,
          timezone
        )

        expected <- parse_expected_participants("")
        poll <- list(
          title = title,
          description = description,
          organizer_name = organizer_name,
          organizer_email = organizer_email,
          duration_minutes = duration,
          timezone = timezone,
          location_type = location_type,
          location_details = location_details,
          response_deadline = response_deadline
        )
        result <- create_poll_record(conn, poll, options, expected)
        response_link <- build_app_link(session, "respond", result$response_token)
        admin_link <- build_app_link(session, "admin", result$admin_token)
        created(list(response_link = response_link, admin_link = admin_link))
        if (is.function(on_created)) {
          on_created(result)
        }
        shiny::showNotification("Poll created. Copy the links below before leaving this page.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })
  })
}

slot_sort_time <- function(slots, timezone) {
  timezone <- validate_timezone(timezone)
  vapply(seq_len(nrow(slots)), function(i) {
    as.numeric(parse_local_datetime(
      as.Date(slots$date[[i]]),
      if (identical(slots$start_time[[i]], "all_day")) "00:00" else slots$start_time[[i]],
      timezone
    ))
  }, numeric(1))
}

order_selected_slots <- function(slots, timezone) {
  if (nrow(slots) == 0) {
    return(slots)
  }
  slots[order(slot_sort_time(slots, timezone), slots$start_time), , drop = FALSE]
}

build_calendar_grid_ui <- function(ns, week_start, timezone, duration_minutes, selected_slots, all_day_mode = FALSE) {
  week_start <- as.Date(week_start)
  days <- week_start + 0:6
  selected_keys <- slot_key(selected_slots$date, selected_slots$start_time)
  today_local <- as.Date(format(Sys.time(), tz = timezone, usetz = FALSE))
  render_key <- paste(as.character(week_start), timezone, duration_minutes, all_day_mode, sep = "|")

  header <- c(
    list(shiny::div(class = "calendar-corner", "Time")),
    lapply(days, function(day) {
      shiny::div(
        class = paste("calendar-day-header", if (identical(day, today_local)) "calendar-day-today" else ""),
        shiny::span(class = "calendar-day-name", format(day, "%a")),
        shiny::strong(paste0(format(day, "%b "), as.integer(format(day, "%d"))))
      )
    })
  )

  body <- if (isTRUE(all_day_mode)) {
    c(
      list(shiny::div(class = "calendar-time-label", `data-time-row` = "all_day", "All day")),
      lapply(days, function(day) calendar_slot_button(ns, day, "all_day", timezone, selected_keys))
    )
  } else {
    step <- if (identical(as.integer(duration_minutes), 15L)) 15L else 30L
    start_minutes <- seq(0L, 1440L - as.integer(duration_minutes), by = step)
    rows <- lapply(start_minutes, function(minutes) {
      time_value <- minutes_to_time(minutes)
      c(
        list(shiny::div(class = "calendar-time-label", `data-time-row` = time_value, time_value)),
        lapply(days, function(day) calendar_slot_button(ns, day, time_value, timezone, selected_keys))
      )
    })
    do.call(c, rows)
  }

  shiny::div(
    class = "calendar-grid-scroll",
    id = ns("calendar_grid_scroll"),
    `data-calendar-render-key` = render_key,
    `data-default-scroll-time` = if (isTRUE(all_day_mode)) NULL else "08:00",
    shiny::div(
      class = "poll-calendar-grid",
      header,
      body
    )
  )
}

calendar_slot_button <- function(ns, date_value, start_time, timezone, selected_keys) {
  key <- slot_key(date_value, start_time)
  slot_start <- parse_local_datetime(date_value, if (identical(start_time, "all_day")) "00:00" else start_time, timezone)
  is_past <- is_past_datetime(slot_start)
  is_selected <- key %in% selected_keys
  class <- paste(
    "calendar-slot",
    if (is_past) "calendar-slot-disabled" else "",
    if (is_selected) "calendar-slot-selected" else ""
  )
  label <- if (is_past) {
    "Past"
  } else if (is_selected) {
    "Selected"
  } else {
    "+"
  }

  shiny::tags$button(
    type = "button",
    class = class,
    disabled = if (is_past) "disabled" else NULL,
    `data-slot-key` = key,
    `data-shiny-input` = ns("slot_changes"),
    `data-start-time` = start_time,
    `aria-pressed` = if (is_selected) "true" else "false",
    `aria-label` = paste(
      if (is_selected) "Remove" else "Select",
      if (identical(start_time, "all_day")) "all day" else start_time,
      "on",
      format_readable_date(date_value, include_year = TRUE, ordinal = FALSE)
    ),
    label
  )
}

minutes_to_time <- function(minutes) {
  sprintf("%02d:%02d", minutes %/% 60L, minutes %% 60L)
}
