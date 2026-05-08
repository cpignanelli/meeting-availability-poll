organizer_portal_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "app-shell organizer-portal-shell",
    app_topbar_ui("Organizer"),
    shiny::uiOutput(ns("portal_page"))
  )
}

organizer_portal_server <- function(id, conn) {
  shiny::moduleServer(id, function(input, output, session) {
    authenticated_email <- shiny::reactiveVal("")
    pending_email <- shiny::reactiveVal("")
    code_requested <- shiny::reactiveVal(FALSE)
    dev_code <- shiny::reactiveVal("")
    selected_poll_id <- shiny::reactiveVal(NULL)
    refresh_counter <- shiny::reactiveVal(0L)
    refresh <- function() refresh_counter(refresh_counter() + 1L)

    portal_polls <- shiny::reactive({
      refresh_counter()
      email <- authenticated_email()
      if (!nzchar(email)) {
        return(data.frame())
      }
      list_polls_for_organizer(conn, email)
    })

    output$portal_page <- shiny::renderUI({
      email <- authenticated_email()
      if (!nzchar(email)) {
        return(organizer_login_flow_ui(session$ns, code_requested(), pending_email(), dev_code()))
      }

      if (!is.null(selected_poll_id())) {
        return(shiny::tagList(
          page_header_ui(
            eyebrow = "Organizer dashboard",
            title = "Poll results",
            subtitle = paste("Signed in as", email),
            actions = shiny::actionButton(session$ns("back_to_portal"), "Back to my polls", class = "btn-outline-secondary")
          ),
          admin_dashboard_body_ui(session$ns("embedded_admin"))
        ))
      }

      polls <- portal_polls()
      shiny::tagList(
        page_header_ui(
          eyebrow = "Organizer portal",
          title = "Organizer workspace",
          subtitle = "Create polls and manage live, expired, closed, and finalized polls connected to your organizer email.",
          actions = shiny::tagList(
            shiny::actionButton(session$ns("refresh_portal"), "Refresh", class = "btn-outline-secondary"),
            shiny::actionButton(session$ns("sign_out"), "Sign out", class = "btn-outline-secondary")
          )
        ),
        shiny::div(
          class = "organizer-workspace-tabs",
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "My polls",
              organizer_poll_tabs_ui(session$ns, polls, conn, session)
            ),
            shiny::tabPanel(
              "Create poll",
              create_poll_ui(session$ns("create"), embedded = TRUE, authenticated = TRUE)
            )
          )
        )
      )
    })

    shiny::observeEvent(input$request_code, {
      tryCatch({
        email <- validate_email(input$organizer_email, field = "Organizer email")
        login <- create_organizer_login_code(conn, email)
        delivery <- send_organizer_magic_code_email(email, login$code)
        pending_email(email)
        code_requested(TRUE)
        dev_code(delivery$dev_code %||% "")
        shiny::showNotification("If this email can receive organizer access, a login code has been sent.", type = "message", duration = 6)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$verify_code, {
      tryCatch({
        email <- pending_email()
        if (!nzchar(email)) {
          stop("Request a login code first.", call. = FALSE)
        }
        if (!verify_organizer_login_code(conn, email, input$magic_code)) {
          stop("That code is invalid or expired. Request a new code if needed.", call. = FALSE)
        }
        authenticated_email(email)
        selected_poll_id(NULL)
        code_requested(FALSE)
        dev_code("")
        refresh()
        shiny::showNotification("Signed in to your organizer portal.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$resend_code, {
      code_requested(FALSE)
      dev_code("")
    })

    shiny::observeEvent(input$view_poll, {
      selected_poll_id(as.integer(input$view_poll))
    })

    shiny::observeEvent(input$back_to_portal, {
      selected_poll_id(NULL)
      refresh()
    })

    shiny::observeEvent(input$refresh_portal, {
      refresh()
    })

    shiny::observeEvent(input$sign_out, {
      authenticated_email("")
      pending_email("")
      code_requested(FALSE)
      dev_code("")
      selected_poll_id(NULL)
    })

    admin_dashboard_server(
      "embedded_admin",
      conn,
      poll_id = shiny::reactive(selected_poll_id()),
      organizer_email = shiny::reactive(authenticated_email())
    )

    create_poll_server(
      "create",
      conn,
      organizer_email = shiny::reactive(authenticated_email()),
      on_created = function(result) {
        selected_poll_id(NULL)
        refresh()
      }
    )
  })
}

organizer_login_flow_ui <- function(ns, code_requested, pending_email, dev_code) {
  if (isTRUE(code_requested)) {
    return(shiny::tagList(
      page_header_ui(
        eyebrow = "Organizer portal",
        title = "Check your email",
        subtitle = "Enter the 6-digit code to view polls connected to your organizer email."
      ),
      section_panel_ui(
        "Enter login code",
        paste("A code was requested for", pending_email),
        shiny::textInput(ns("magic_code"), "6-digit code", placeholder = "123456"),
        if (nzchar(dev_code %||% "")) {
          shiny::div(
            class = "dev-code-panel",
            shiny::strong("Development login code"),
            shiny::p(dev_code)
          )
        },
        shiny::div(
          class = "button-row",
          shiny::actionButton(ns("verify_code"), "View my polls", class = "btn-primary"),
          shiny::actionButton(ns("resend_code"), "Use a different email", class = "btn-outline-secondary")
        )
      )
    ))
  }

  shiny::tagList(
    page_header_ui(
      eyebrow = "Organizer portal",
      title = "Organizer workspace",
      subtitle = "Sign in with your organizer email to create polls and manage live, expired, closed, and finalized polls."
    ),
    section_panel_ui(
      "Email login",
      "We will send a 6-digit code. The code expires after 10 minutes.",
      shiny::textInput(ns("organizer_email"), "Organizer email", placeholder = "you@example.org"),
      shiny::actionButton(ns("request_code"), "Send login code", class = "btn-primary")
    )
  )
}

organizer_poll_tabs_ui <- function(ns, polls, conn, session) {
  if (is.null(polls) || nrow(polls) == 0) {
    return(section_panel_ui(
      "Polls",
      NULL,
      empty_state_ui("No polls found", "Create polls with this organizer email, then return here to manage them.")
    ))
  }

  enriched <- enrich_portal_polls(conn, polls)
  shiny::div(
    class = "portal-tabs",
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("Live", organizer_poll_grid_ui(ns, enriched[enriched$display_status == "open", , drop = FALSE], session)),
      shiny::tabPanel("Expired", organizer_poll_grid_ui(ns, enriched[enriched$display_status == "expired", , drop = FALSE], session)),
      shiny::tabPanel("Closed", organizer_poll_grid_ui(ns, enriched[enriched$display_status == "closed", , drop = FALSE], session)),
      shiny::tabPanel("Finalized", organizer_poll_grid_ui(ns, enriched[enriched$display_status == "finalized", , drop = FALSE], session))
    )
  )
}

enrich_portal_polls <- function(conn, polls) {
  statuses <- vapply(seq_len(nrow(polls)), function(i) {
    options <- get_poll_options(conn, polls$poll_id[[i]])
    poll_display_status(polls[i, , drop = FALSE], options)
  }, character(1))
  expiries <- vapply(seq_len(nrow(polls)), function(i) {
    options <- get_poll_options(conn, polls$poll_id[[i]])
    format_deadline_label(poll_effective_deadline(polls[i, , drop = FALSE], options))
  }, character(1))
  polls$display_status <- statuses
  polls$display_expiry <- expiries
  polls
}

organizer_poll_grid_ui <- function(ns, polls, session) {
  if (is.null(polls) || nrow(polls) == 0) {
    return(empty_state_ui("No polls in this group", "Polls will appear here when their status matches this filter."))
  }
  shiny::div(
    class = "organizer-poll-grid",
    lapply(seq_len(nrow(polls)), function(i) portal_poll_card_ui(ns, polls[i, , drop = FALSE], session))
  )
}

portal_poll_card_ui <- function(ns, poll, session) {
  response_link <- build_app_link(session, "respond", poll$response_token[[1]])
  copy_id <- ns(paste0("portal_response_link_", poll$poll_id[[1]]))
  shiny::div(
    class = "organizer-poll-card",
    shiny::div(
      class = "organizer-poll-card-top",
      shiny::span(class = "option-kicker", "Group poll"),
      status_pill_ui(poll$display_status[[1]], poll_display_status_label(poll$display_status[[1]]))
    ),
    shiny::h2(poll$title[[1]]),
    shiny::div(
      class = "organizer-poll-card-meta",
      detail_item_ui("Responses", poll$response_count[[1]]),
      detail_item_ui("Time options", poll$option_count[[1]]),
      detail_item_ui("Link expiry", poll$display_expiry[[1]])
    ),
    shiny::div(
      class = "organizer-poll-card-actions",
      shiny::tags$button(
        type = "button",
        class = "btn btn-primary",
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("view_poll"), poll$poll_id[[1]]),
        "View results"
      ),
      shiny::tags$input(
        id = copy_id,
        class = "visually-hidden-copy-source",
        type = "text",
        value = response_link,
        readonly = "readonly"
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-secondary copy-button",
        `data-copy-target` = copy_id,
        "Copy response link"
      )
    )
  )
}
