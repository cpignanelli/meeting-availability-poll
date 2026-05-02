create_poll_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "app-shell",
      shiny::div(
        class = "app-header",
        shiny::h1("Create a meeting availability poll"),
        shiny::p("Manually propose meeting times, share a response link, and review availability through a private organizer dashboard.")
      ),
      shiny::uiOutput(ns("created_links")),
      shiny::div(
        class = "section-card",
        shiny::h2("Meeting details"),
        shiny::fluidRow(
          shiny::column(7, shiny::textInput(ns("title"), "Meeting title", placeholder = "Performance working group planning meeting")),
          shiny::column(5, shiny::numericInput(ns("duration_minutes"), "Duration in minutes", value = 60, min = 5, max = 1440, step = 5))
        ),
        shiny::textAreaInput(ns("description"), "Description/context", rows = 3, placeholder = "Briefly describe the purpose of the meeting."),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("organizer_name"), "Organizer name")),
          shiny::column(6, shiny::textInput(ns("organizer_email"), "Organizer email"))
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns("timezone"),
              "Time zone",
              choices = OlsonNames(),
              selected = if ("America/Toronto" %in% OlsonNames()) "America/Toronto" else Sys.timezone()
            )
          ),
          shiny::column(6, shiny::dateInput(ns("response_deadline"), "Optional response deadline", value = NULL))
        ),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(ns("location_type"), "Location type", choices = c("To be determined", "Virtual", "In person", "Hybrid"))
          ),
          shiny::column(8, shiny::textInput(ns("location_details"), "Optional location or virtual meeting details"))
        )
      ),
      shiny::div(
        class = "section-card",
        shiny::h2("Proposed times"),
        shiny::p(class = "helper-text", "Enter local times in the selected time zone using 24-hour HH:MM format."),
        shiny::uiOutput(ns("time_options_ui")),
        shiny::div(
          class = "d-flex gap-2 flex-wrap",
          shiny::actionButton(ns("add_option"), "Add time", class = "btn-outline-secondary"),
          shiny::actionButton(ns("remove_option"), "Remove last", class = "btn-outline-secondary")
        )
      ),
      shiny::div(
        class = "section-card",
        shiny::h2("Expected participants"),
        shiny::p(class = "helper-text", "Optional. Paste one participant per line as: name,email,organization,required. Use yes/no for required."),
        shiny::textAreaInput(
          ns("expected_participants"),
          "Expected participant list",
          rows = 5,
          placeholder = "Alex Lee,alex@example.org,Institute A,yes\nSam Patel,sam@example.org,Partner Org,no"
        ),
        shiny::textAreaInput(ns("notes"), "Optional notes/instructions for participants", rows = 3),
        shiny::actionButton(ns("create_poll"), "Create poll", class = "btn-primary")
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

    output$time_options_ui <- shiny::renderUI({
      ns <- session$ns
      rows <- lapply(seq_len(option_count()), function(i) {
        shiny::div(
          class = "time-option-row",
          shiny::dateInput(ns(paste0("option_date_", i)), paste("Date", i), value = Sys.Date() + i),
          shiny::textInput(ns(paste0("option_time_", i)), paste("Start time", i), value = if (i == 1) "09:00" else if (i == 2) "11:00" else "14:00")
        )
      })
      shiny::tagList(rows)
    })

    output$created_links <- shiny::renderUI({
      info <- created()
      if (is.null(info)) {
        return(NULL)
      }
      shiny::div(
        class = "section-card link-box",
        shiny::h2("Poll created"),
        shiny::p("Share the public response link with participants. Keep the private organizer link restricted to the organizer."),
        shiny::textAreaInput(session$ns("response_link_display"), "Public response link", value = info$response_link, rows = 2, width = "100%"),
        shiny::textAreaInput(session$ns("admin_link_display"), "Private organizer link", value = info$admin_link, rows = 2, width = "100%"),
        shiny::p(class = "helper-text", "The private link is the only way to view results in this prototype. It is shown once here and stored only as a hash.")
      )
    })

    shiny::observeEvent(input$create_poll, {
      tryCatch({
        title <- sanitize_text(input$title, max_chars = 180, required = TRUE, field = "Meeting title")
        duration <- validate_duration(input$duration_minutes)
        timezone <- validate_timezone(input$timezone)
        description <- sanitize_text(input$description, max_chars = 2000)
        notes <- sanitize_text(input$notes, max_chars = 2000)
        if (nzchar(notes)) {
          description <- paste(c(description, paste0("Participant instructions: ", notes)), collapse = "\n\n")
        }
        organizer_name <- sanitize_text(input$organizer_name, max_chars = 160, required = TRUE, field = "Organizer name")
        organizer_email <- validate_email(input$organizer_email, field = "Organizer email")
        location_type <- sanitize_text(input$location_type, max_chars = 80)
        location_details <- sanitize_text(input$location_details, max_chars = 1000)
        response_deadline <- input$response_deadline
        response_deadline <- if (is.null(response_deadline) || is.na(response_deadline)) "" else as.character(response_deadline)

        options <- lapply(seq_len(option_count()), function(i) {
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
        options <- do.call(rbind, options)
        options <- unique(options)
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
