create_poll_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-shell organizer-shell",
    app_topbar_ui("Create"),
    page_header_ui(
      eyebrow = "Organizer setup",
      title = "Create a meeting availability poll",
      subtitle = "Create one unique booking invite for this meeting. You can create additional polls for other meetings or groups."
    ),
    shiny::uiOutput(ns("created_links")),
    shiny::div(
      class = "create-layout",
      shiny::div(
        class = "create-main",
        section_panel_ui(
          "1. Meeting basics",
          "Keep this short and recognizable. Participants will see these details before choosing availability.",
          shiny::fluidRow(
            shiny::column(7, shiny::textInput(ns("title"), "Meeting title", placeholder = "Performance working group planning meeting")),
            shiny::column(5, shiny::numericInput(ns("duration_minutes"), "Duration in minutes", value = 60, min = 5, max = 1440, step = 5))
          ),
          shiny::textAreaInput(ns("description"), "Description/context", rows = 3, placeholder = "Briefly describe the purpose of the meeting."),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("organizer_name"), "Organizer name")),
            shiny::column(6, shiny::textInput(ns("organizer_email"), "Organizer email"))
          ),
          shiny::selectInput(
            ns("timezone"),
            "Time zone",
            choices = OlsonNames(),
            selected = if ("America/Toronto" %in% OlsonNames()) "America/Toronto" else Sys.timezone()
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(ns("location_type"), "Location type", choices = c("To be determined", "Virtual", "In person", "Hybrid"))),
            shiny::column(8, shiny::textInput(ns("location_details"), "Optional location or virtual meeting details"))
          ),
          shiny::div(
            class = "response-settings-card",
            shiny::div(
              class = "response-settings-copy",
              shiny::strong("Response link settings"),
              shiny::p("The response link will close after this date unless you reopen it from the organizer dashboard.")
            ),
            shiny::dateInput(ns("response_deadline"), "Response deadline / link expiry", value = NULL)
          )
        ),
        section_panel_ui(
          "2. Proposed times",
          "Enter local times in the selected time zone using 24-hour HH:MM format. Add several realistic options so participants can compare them quickly.",
          shiny::uiOutput(ns("time_options_ui")),
          shiny::div(
            class = "button-row",
            shiny::actionButton(ns("add_option"), "Add time", class = "btn-outline-secondary"),
            shiny::actionButton(ns("remove_option"), "Remove last", class = "btn-outline-secondary")
          ),
          shiny::uiOutput(ns("time_preview"))
        ),
        section_panel_ui(
          "3. Participants and instructions",
          "Expected participants are optional and visible only in the private organizer dashboard.",
          shiny::textAreaInput(
            ns("expected_participants"),
            "Expected participant list",
            rows = 5,
            placeholder = "Alex Lee,alex@example.org,Institute A,yes\nSam Patel,sam@example.org,Partner Org,no"
          ),
          shiny::uiOutput(ns("expected_preview")),
          shiny::textAreaInput(ns("notes"), "Optional notes/instructions for participants", rows = 3, placeholder = "Add context participants need before responding.")
        )
      ),
      shiny::tags$aside(
        class = "create-sidebar",
        section_panel_ui(
          "Review and create",
          "Create the poll when the essentials are ready. The private organizer link is shown once after creation.",
          shiny::uiOutput(ns("create_readiness")),
          shiny::actionButton(ns("create_poll"), "Create poll and generate links", class = "btn-primary btn-lg create-submit")
        ),
        privacy_notice_ui(compact = TRUE)
      )
    )
  )
}

create_poll_server <- function(id, conn) {
  shiny::moduleServer(id, function(input, output, session) {
    option_count <- shiny::reactiveVal(3L)
    created <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$add_option, {
      option_count(min(option_count() + 1L, 30L))
    })

    shiny::observeEvent(input$remove_option, {
      option_count(max(option_count() - 1L, 1L))
    })

    collect_time_options <- function(timezone, duration) {
      rows <- lapply(seq_len(option_count()), function(i) {
        start_local <- parse_local_datetime(input[[paste0("option_date_", i)]], input[[paste0("option_time_", i)]], timezone)
        end_local <- add_minutes(start_local, duration)
        data.frame(
          start_datetime = as_utc_string(start_local),
          end_datetime = as_utc_string(end_local),
          display_label = format_option_label(as_utc_string(start_local), as_utc_string(end_local), timezone),
          option_order = i,
          stringsAsFactors = FALSE
        )
      })
      options <- do.call(rbind, rows)
      options <- options[!duplicated(options[c("start_datetime", "end_datetime")]), , drop = FALSE]
      options$option_order <- seq_len(nrow(options))
      rownames(options) <- NULL
      options
    }

    output$time_options_ui <- shiny::renderUI({
      ns <- session$ns
      rows <- lapply(seq_len(option_count()), function(i) {
        shiny::div(
          class = "time-option-row",
          shiny::div(
            class = "time-option-index",
            shiny::span("Option"),
            shiny::strong(i)
          ),
          shiny::dateInput(ns(paste0("option_date_", i)), paste("Date", i), value = Sys.Date() + i),
          shiny::textInput(ns(paste0("option_time_", i)), paste("Start time", i), value = if (i == 1) "09:00" else if (i == 2) "11:00" else "14:00")
        )
      })
      shiny::tagList(rows)
    })

    output$time_preview <- shiny::renderUI({
      if (is.null(input$timezone) || is.null(input$duration_minutes)) {
        return(NULL)
      }

      preview <- tryCatch({
        timezone <- validate_timezone(input$timezone)
        duration <- validate_duration(input$duration_minutes)
        collect_time_options(timezone, duration)
      }, error = function(e) {
        return(NULL)
      })

      if (is.null(preview) || nrow(preview) == 0) {
        return(shiny::p(class = "helper-text", "A preview will appear after valid dates and times are entered."))
      }

      shiny::div(
        class = "time-preview",
        shiny::div(class = "preview-label", "Participant preview"),
        lapply(seq_len(nrow(preview)), function(i) {
          shiny::div(
            class = "time-preview-item",
            shiny::span(class = "time-preview-number", i),
            shiny::span(preview$display_label[[i]])
          )
        })
      )
    })

    output$expected_preview <- shiny::renderUI({
      text <- input$expected_participants %||% ""
      if (!nzchar(trimws(text))) {
        return(shiny::p(class = "helper-text", "No expected participant list added. You can still share the poll with anyone who has the public response link."))
      }

      parsed <- tryCatch(parse_expected_participants(text), error = function(e) e)
      if (inherits(parsed, "error")) {
        return(validation_summary_ui(safe_error_message(parsed)))
      }
      build_expected_preview_table(parsed)
    })

    output$create_readiness <- shiny::renderUI({
      missing <- character()
      if (!nzchar(trimws(input$title %||% ""))) missing <- c(missing, "Meeting title")
      if (!nzchar(trimws(input$organizer_name %||% ""))) missing <- c(missing, "Organizer name")
      if (!nzchar(trimws(input$organizer_email %||% ""))) missing <- c(missing, "Organizer email")

      valid_times <- tryCatch({
        timezone <- validate_timezone(input$timezone)
        duration <- validate_duration(input$duration_minutes)
        options <- collect_time_options(timezone, duration)
        nrow(options)
      }, error = function(e) 0L)

      if (valid_times == 0L) {
        missing <- c(missing, "At least one valid proposed time")
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
      section_panel_ui(
        "Poll created",
        "This is one unique booking invite. Share the public response link with participants, and keep the private organizer link restricted to the organizer.",
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
        duration <- validate_duration(input$duration_minutes)
        timezone <- validate_timezone(input$timezone)
        description <- sanitize_text(input$description, max_chars = 2000)
        notes <- sanitize_text(input$notes, max_chars = 2000)
        description_parts <- c(description, if (nzchar(notes)) paste0("Participant instructions: ", notes) else "")
        description <- paste(description_parts[nzchar(description_parts)], collapse = "\n\n")
        organizer_name <- sanitize_text(input$organizer_name, max_chars = 160, required = TRUE, field = "Organizer name")
        organizer_email <- validate_email(input$organizer_email, field = "Organizer email")
        location_type <- sanitize_text(input$location_type, max_chars = 80)
        location_details <- sanitize_text(input$location_details, max_chars = 1000)
        response_deadline <- input$response_deadline
        response_deadline <- if (is.null(response_deadline) || is.na(response_deadline)) "" else as.character(response_deadline)
        if (nzchar(response_deadline)) {
          today_local <- as.Date(format(Sys.time(), tz = timezone, usetz = FALSE))
          if (as.Date(response_deadline) < today_local) {
            stop("Choose today or a future date for the response deadline / link expiry.", call. = FALSE)
          }
        }

        options <- collect_time_options(timezone, duration)
        if (nrow(options) == 0) {
          stop("Add at least one proposed meeting time.", call. = FALSE)
        }
        if (any(parse_utc_timestamp(options$start_datetime) <= now_utc())) {
          stop("All proposed meeting times must be in the future.", call. = FALSE)
        }

        expected <- parse_expected_participants(input$expected_participants)
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
        shiny::showNotification("Poll created. Copy the links above before leaving this page.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })
  })
}

build_expected_preview_table <- function(expected) {
  if (nrow(expected) == 0) {
    return(shiny::p(class = "helper-text", "No expected participant list added."))
  }

  rows <- lapply(seq_len(nrow(expected)), function(i) {
    shiny::tags$tr(
      shiny::tags$td(expected$name[[i]]),
      shiny::tags$td(expected$email[[i]]),
      shiny::tags$td(expected$organization[[i]]),
      shiny::tags$td(if (expected$is_required[[i]] == 1L) "Required" else "Optional")
    )
  })

  shiny::div(
    class = "preview-table-wrap",
    shiny::tags$table(
      class = "preview-table",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Name"),
          shiny::tags$th("Email"),
          shiny::tags$th("Organization"),
          shiny::tags$th("Required")
        )
      ),
      shiny::tags$tbody(rows)
    )
  )
}
