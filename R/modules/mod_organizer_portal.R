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
    authenticated_role <- shiny::reactiveVal("")
    pending_email <- shiny::reactiveVal("")
    code_requested <- shiny::reactiveVal(FALSE)
    dev_code <- shiny::reactiveVal("")
    sign_in_notice <- shiny::reactiveVal("")
    access_request_profile <- shiny::reactiveVal(NULL)
    access_request_code_requested <- shiny::reactiveVal(FALSE)
    access_request_dev_code <- shiny::reactiveVal("")
    access_request_submitted <- shiny::reactiveVal(FALSE)
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
        return(organizer_login_flow_ui(
          ns = session$ns,
          code_requested = code_requested(),
          pending_email = pending_email(),
          dev_code = dev_code(),
          sign_in_notice = sign_in_notice(),
          access_request_code_requested = access_request_code_requested(),
          access_request_profile = access_request_profile(),
          access_request_dev_code = access_request_dev_code(),
          access_request_submitted = access_request_submitted()
        ))
      }

      if (!is.null(selected_poll_id())) {
        return(shiny::tagList(
          shiny::div(
            class = "portal-results-bar",
            shiny::div(
              shiny::span(class = "eyebrow", "Poll results"),
              shiny::strong(paste("Signed in as", email))
            ),
            shiny::actionButton(session$ns("back_to_portal"), "Back to my polls", class = "btn-outline-secondary")
          ),
          admin_dashboard_body_ui(session$ns("embedded_admin"))
        ))
      }

      polls <- portal_polls()
      shiny::tagList(
        page_header_ui(
          eyebrow = "Organizer portal",
          title = "Organizer workspace",
          subtitle = paste("Signed in as", email, "|", owner_role_label(authenticated_role())),
          actions = shiny::tagList(
            shiny::actionButton(session$ns("refresh_portal"), "Refresh", class = "btn-outline-secondary"),
            shiny::actionButton(session$ns("sign_out"), "Sign out", class = "btn-outline-secondary")
          )
        ),
        organizer_workspace_tabs_ui(session$ns, conn, polls, email, authenticated_role(), session)
      )
    })

    shiny::observeEvent(input$request_code, {
      tryCatch({
        app_main_owner_email()
        email <- validate_email(input$organizer_email, field = "Organizer email")
        role <- get_owner_role(conn, email)
        if (!role %in% c("main_owner", "owner")) {
          pending_email("")
          code_requested(FALSE)
          dev_code("")
          sign_in_notice(access_request_guidance_message(role))
          shiny::updateTextInput(session, "request_email", value = email)
          return(shiny::showNotification("Use the request organizer access section to continue.", type = "warning", duration = 8))
        }

        login <- create_organizer_login_code(conn, email)
        delivery <- send_organizer_magic_code_email(email, login$code)
        pending_email(email)
        code_requested(TRUE)
        dev_code(delivery$dev_code %||% "")
        sign_in_notice("")
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

        role <- get_owner_role(conn, email)
        if (!role %in% c("main_owner", "owner")) {
          authenticated_email("")
          authenticated_role("")
          selected_poll_id(NULL)
          code_requested(FALSE)
          dev_code("")
          sign_in_notice(access_request_guidance_message(role))
          refresh()
          return(shiny::showNotification("This email is not approved for organizer access.", type = "warning", duration = 8))
        }

        authenticated_email(email)
        authenticated_role(role)
        session$sendCustomMessage("trustedSession", list(
          scope = "organizer",
          token = issue_trusted_session_token("organizer", email)
        ))
        selected_poll_id(NULL)
        code_requested(FALSE)
        dev_code("")
        sign_in_notice("")
        refresh()
        shiny::showNotification("Signed in to your organizer portal.", type = "message")
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$trusted_session, {
      tryCatch({
        restored <- verify_trusted_session_token(input$trusted_session, expected_scope = "organizer")
        if (!isTRUE(restored$valid)) {
          session$sendCustomMessage("clearTrustedSession", list(scope = "organizer"))
          return()
        }
        role <- get_owner_role(conn, restored$email)
        if (!role %in% c("main_owner", "owner")) {
          authenticated_email("")
          authenticated_role("")
          session$sendCustomMessage("clearTrustedSession", list(scope = "organizer"))
          sign_in_notice(access_request_guidance_message(role))
          return()
        }
        if (!identical(authenticated_email(), restored$email)) {
          authenticated_email(restored$email)
          authenticated_role(role)
          selected_poll_id(NULL)
          code_requested(FALSE)
          dev_code("")
          sign_in_notice("")
          refresh()
        }
      }, error = function(e) {
        session$sendCustomMessage("clearTrustedSession", list(scope = "organizer"))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$resend_code, {
      code_requested(FALSE)
      dev_code("")
      sign_in_notice("")
    })

    shiny::observeEvent(input$request_access_code, {
      tryCatch({
        app_main_owner_email()
        profile <- validate_owner_profile(input$request_first_name, input$request_last_name, input$request_email)
        login <- create_organizer_login_code(conn, profile$email)
        delivery <- send_organizer_magic_code_email(profile$email, login$code)
        access_request_profile(profile)
        access_request_code_requested(TRUE)
        access_request_dev_code(delivery$dev_code %||% "")
        access_request_submitted(FALSE)
        shiny::showNotification("A verification code has been sent to the requested organizer email.", type = "message", duration = 6)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$submit_access_request, {
      tryCatch({
        profile <- access_request_profile()
        if (is.null(profile)) {
          stop("Request a verification code first.", call. = FALSE)
        }
        if (!verify_organizer_login_code(conn, profile$email, input$request_access_magic_code)) {
          stop("That code is invalid or expired. Request a new code if needed.", call. = FALSE)
        }

        role <- get_owner_role(conn, profile$email)
        if (role %in% c("main_owner", "owner")) {
          stop("This email already has organizer access. Sign in instead.", call. = FALSE)
        }

        request <- create_or_update_owner_access_request(
          conn,
          first_name = profile$first_name,
          last_name = profile$last_name,
          email = profile$email
        )
        send_owner_access_request_email(request)
        access_request_submitted(TRUE)
        access_request_code_requested(FALSE)
        access_request_dev_code("")
        refresh()
        shiny::showNotification("Your request was sent to the main owner for review.", type = "message", duration = 8)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$edit_access_request, {
      access_request_code_requested(FALSE)
      access_request_dev_code("")
      access_request_submitted(FALSE)
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

    shiny::observeEvent(input$approve_access_request, {
      tryCatch({
        approve_owner_request(conn, input$approve_access_request, authenticated_email())
        refresh()
        shiny::showNotification("Organizer access approved.", type = "message", duration = 6)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$deny_access_request, {
      tryCatch({
        deny_owner_request(conn, input$deny_access_request, authenticated_email())
        refresh()
        shiny::showNotification("Organizer access request denied.", type = "message", duration = 6)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$revoke_owner_access, {
      tryCatch({
        revoke_approved_owner(conn, input$revoke_owner_access, authenticated_email())
        refresh()
        shiny::showNotification("Organizer access revoked.", type = "message", duration = 6)
      }, error = function(e) {
        shiny::showNotification(safe_error_message(e), type = "error", duration = 8)
      })
    })

    shiny::observeEvent(input$sign_out, {
      authenticated_email("")
      authenticated_role("")
      pending_email("")
      code_requested(FALSE)
      dev_code("")
      sign_in_notice("")
      access_request_profile(NULL)
      access_request_code_requested(FALSE)
      access_request_dev_code("")
      access_request_submitted(FALSE)
      selected_poll_id(NULL)
      session$sendCustomMessage("clearTrustedSession", list(scope = "organizer"))
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

access_request_guidance_message <- function(role) {
  switch(
    role,
    pending = "A request for this email is still pending. The main owner must approve it before organizer sign-in is available.",
    denied = "Organizer sign-in is not currently available for this email. Submit a new access request below if access should be reviewed again.",
    revoked = "Organizer sign-in is not currently available for this email. Contact the main owner if access should be restored.",
    none = "Organizer sign-in is not currently available for this email. Complete the access request section below to ask the main owner for access.",
    "Organizer sign-in is not currently available for this email. Complete the access request section below to ask the main owner for access."
  )
}

organizer_login_flow_ui <- function(
  ns,
  code_requested,
  pending_email,
  dev_code,
  sign_in_notice,
  access_request_code_requested,
  access_request_profile,
  access_request_dev_code,
  access_request_submitted
) {
  config_error <- tryCatch({
    app_main_owner_email()
    ""
  }, error = function(e) conditionMessage(e))

  if (nzchar(config_error)) {
    return(shiny::tagList(
      page_header_ui(
        eyebrow = "Organizer portal",
        title = "Organizer access is not configured",
        subtitle = "The app owner must configure a main owner email before organizers can sign in or request access."
      ),
      section_panel_ui(
        "Configuration required",
        NULL,
        status_banner_ui(
          "closed",
          "Set APP_MAIN_OWNER_EMAIL",
          "Add APP_MAIN_OWNER_EMAIL to the app environment variables, then restart or republish the app."
        )
      )
    ))
  }

  shiny::tagList(
    page_header_ui(
      eyebrow = "Organizer portal",
      title = "Organizer workspace",
      subtitle = "Sign in if you already have access, or request organizer access for this app."
    ),
    shiny::div(
      class = "organizer-auth-grid",
      organizer_sign_in_card_ui(ns, code_requested, pending_email, dev_code, sign_in_notice),
      organizer_request_access_card_ui(
        ns,
        code_requested = access_request_code_requested,
        profile = access_request_profile,
        dev_code = access_request_dev_code,
        submitted = access_request_submitted
      )
    )
  )
}

organizer_sign_in_card_ui <- function(ns, code_requested, pending_email, dev_code, sign_in_notice) {
  if (isTRUE(code_requested)) {
    return(section_panel_ui(
      "Sign in",
      paste("Enter the 6-digit code requested for", pending_email),
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
    ))
  }

  section_panel_ui(
    "Sign in",
    "Approved organizers receive a 6-digit code by email. The code expires after 10 minutes.",
    if (nzchar(sign_in_notice %||% "")) {
      status_banner_ui("pending", "Organizer access required", sign_in_notice)
    },
    shiny::textInput(ns("organizer_email"), "Organizer email", placeholder = "you@example.org"),
    shiny::actionButton(ns("request_code"), "Send login code", class = "btn-primary")
  )
}

organizer_request_access_card_ui <- function(ns, code_requested, profile, dev_code, submitted) {
  if (isTRUE(submitted)) {
    return(section_panel_ui(
      "Request organizer access",
      NULL,
      status_banner_ui(
        "open",
        "Request sent",
        "Your request was sent to the main owner. After approval, sign in with your email to create and manage polls."
      )
    ))
  }

  if (isTRUE(code_requested)) {
    email <- profile$email %||% ""
    name <- paste(profile$first_name %||% "", profile$last_name %||% "")
    return(section_panel_ui(
      "Request organizer access",
      paste("Enter the 6-digit code sent to", email, "to verify this access request."),
      detail_grid_ui(list(
        list(label = "Name", value = name),
        list(label = "Email", value = email)
      )),
      shiny::textInput(ns("request_access_magic_code"), "6-digit code", placeholder = "123456"),
      if (nzchar(dev_code %||% "")) {
        shiny::div(
          class = "dev-code-panel",
          shiny::strong("Development verification code"),
          shiny::p(dev_code)
        )
      },
      shiny::div(
        class = "button-row",
        shiny::actionButton(ns("submit_access_request"), "Submit access request", class = "btn-primary"),
        shiny::actionButton(ns("edit_access_request"), "Edit details", class = "btn-outline-secondary")
      )
    ))
  }

  section_panel_ui(
    "Request organizer access",
    "Enter your name and email. You will verify the email before the main owner reviews the request.",
    shiny::div(
      class = "owner-request-grid",
      shiny::textInput(ns("request_first_name"), "First name"),
      shiny::textInput(ns("request_last_name"), "Last name"),
      shiny::textInput(ns("request_email"), "Email", placeholder = "you@example.org")
    ),
    shiny::actionButton(ns("request_access_code"), "Verify email and request access", class = "btn-primary")
  )
}

organizer_workspace_tabs_ui <- function(ns, conn, polls, email, role, session) {
  panels <- list(
    shiny::tabPanel(
      "My polls",
      organizer_poll_tabs_ui(ns, polls, conn, session)
    ),
    shiny::tabPanel(
      "Create poll",
      create_poll_ui(ns("create"), embedded = TRUE, authenticated = TRUE)
    )
  )

  if (identical(role, "main_owner")) {
    panels <- c(
      panels,
      list(
        shiny::tabPanel("Access requests", owner_access_requests_ui(ns, conn, email)),
        shiny::tabPanel("Approved owners", approved_owners_ui(ns, conn, email))
      )
    )
  }

  shiny::div(
    class = "organizer-workspace-tabs",
    do.call(shiny::tabsetPanel, c(list(type = "tabs"), panels))
  )
}

owner_access_requests_ui <- function(ns, conn, reviewer_email) {
  requests <- list_owner_access_requests(conn, reviewer_email, status = "pending")
  if (nrow(requests) == 0) {
    return(section_panel_ui(
      "Access requests",
      NULL,
      empty_state_ui("No pending requests", "New organizer access requests will appear here after email verification.")
    ))
  }

  section_panel_ui(
    "Access requests",
    "Approve only people who should be able to create and manage polls in this app.",
    shiny::div(
      class = "owner-access-list",
      lapply(seq_len(nrow(requests)), function(i) owner_access_request_card_ui(ns, requests[i, , drop = FALSE]))
    )
  )
}

owner_access_request_card_ui <- function(ns, request) {
  request_id <- request$request_id[[1]]
  shiny::div(
    class = "owner-access-card",
    shiny::div(
      class = "owner-access-card-main",
      shiny::span(class = "option-kicker", "Pending request"),
      shiny::h3(paste(request$first_name[[1]], request$last_name[[1]])),
      shiny::p(request$email[[1]]),
      shiny::p(class = "helper-text", paste("Requested", request$requested_at[[1]]))
    ),
    shiny::div(
      class = "owner-access-card-actions",
      shiny::tags$button(
        type = "button",
        class = "btn btn-primary",
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("approve_access_request"), request_id),
        "Approve"
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-secondary",
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("deny_access_request"), request_id),
        "Deny"
      )
    )
  )
}

approved_owners_ui <- function(ns, conn, reviewer_email) {
  owners <- list_approved_owners(conn, reviewer_email, include_revoked = FALSE)
  if (nrow(owners) == 0) {
    return(section_panel_ui(
      "Approved owners",
      NULL,
      empty_state_ui("No approved secondary owners", "Approved organizers will appear here. The main owner is configured separately in app settings.")
    ))
  }

  section_panel_ui(
    "Approved owners",
    "Revoking access prevents future organizer sign-in for that email. It does not delete existing polls or participant data.",
    shiny::div(
      class = "owner-access-list",
      lapply(seq_len(nrow(owners)), function(i) approved_owner_card_ui(ns, owners[i, , drop = FALSE]))
    )
  )
}

approved_owner_card_ui <- function(ns, owner) {
  owner_id <- owner$owner_id[[1]]
  shiny::div(
    class = "owner-access-card",
    shiny::div(
      class = "owner-access-card-main",
      shiny::span(class = "option-kicker", "Approved owner"),
      shiny::h3(paste(owner$first_name[[1]], owner$last_name[[1]])),
      shiny::p(owner$email[[1]]),
      shiny::p(class = "helper-text", paste("Approved", owner$approved_at[[1]]))
    ),
    shiny::div(
      class = "owner-access-card-actions",
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-secondary",
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("revoke_owner_access"), owner_id),
        "Revoke access"
      )
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
