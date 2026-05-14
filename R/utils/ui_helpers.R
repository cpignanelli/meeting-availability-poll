ui_text <- function(value, fallback = "") {
  value <- as.character(value %||% fallback)
  value <- value[!is.na(value)]
  value <- paste(value, collapse = "\n")
  if (nzchar(trimws(value))) value else fallback
}

app_topbar_ui <- function(context, status = NULL) {
  shiny::div(
    class = "app-topbar",
    shiny::div(
      class = "app-brand",
      shiny::span(class = "brand-mark"),
      shiny::span(class = "brand-name", "Meeting Availability Poll")
    ),
    shiny::div(
      class = "app-context",
      shiny::span(context),
      if (!is.null(status)) status
    )
  )
}

private_creation_page_ui <- function() {
  shiny::div(
    class = "app-shell access-shell",
    app_topbar_ui("Private creation access"),
    page_header_ui(
      eyebrow = "Private organizer access",
      title = "Poll creation is private",
      subtitle = "Use your private creation URL to start a new booking poll. Participant response links and organizer dashboard links continue to work normally."
    ),
    section_panel_ui(
      "Need to create a poll?",
      "Open the private creation link that includes your creation secret. This protects a public pilot app from random visitors creating booking links.",
      shiny::p(class = "helper-text", "The link format is:"),
      shiny::pre(class = "route-example", "?create=<your-private-creation-secret>")
    )
  )
}

page_header_ui <- function(eyebrow, title, subtitle = NULL, meta = NULL, actions = NULL) {
  shiny::div(
    class = "page-hero",
    shiny::div(
      class = "page-hero-main",
      shiny::div(class = "eyebrow", eyebrow),
      shiny::h1(title),
      if (!is.null(subtitle) && nzchar(ui_text(subtitle))) {
        shiny::p(class = "page-subtitle", subtitle)
      },
      if (!is.null(meta)) {
        shiny::div(class = "hero-meta", meta)
      }
    ),
    if (!is.null(actions)) {
      shiny::div(class = "page-hero-actions", actions)
    }
  )
}

section_panel_ui <- function(title, subtitle = NULL, ..., class = "") {
  shiny::div(
    class = paste("section-card", class),
    shiny::div(
      class = "section-heading",
      shiny::h2(title),
      if (!is.null(subtitle) && nzchar(ui_text(subtitle))) {
        shiny::p(class = "helper-text", subtitle)
      }
    ),
    ...
  )
}

empty_state_ui <- function(title, message, action = NULL) {
  shiny::div(
    class = "empty-state",
    shiny::div(class = "empty-state-mark", "!"),
    shiny::h2(title),
    shiny::p(message),
    action
  )
}

privacy_notice_ui <- function(compact = FALSE) {
  shiny::div(
    class = if (compact) "privacy-notice privacy-notice-compact" else "privacy-notice",
    shiny::strong("Privacy notice"),
    shiny::p("This form collects your name, optional email, availability, and optional comments so the organizer can choose a meeting time. Results are visible only through the private organizer link.")
  )
}

detail_item_ui <- function(label, value) {
  shiny::div(
    class = "detail-item",
    shiny::span(class = "detail-label", label),
    shiny::span(class = "detail-value", ui_text(value, "Not provided"))
  )
}

detail_grid_ui <- function(items) {
  shiny::div(
    class = "detail-grid",
    lapply(items, function(item) detail_item_ui(item$label, item$value))
  )
}

format_duration_label <- function(duration_minutes) {
  minutes <- suppressWarnings(as.integer(duration_minutes[[1]] %||% NA_integer_))
  if (is.na(minutes) || minutes <= 0) {
    return("Not provided")
  }
  if (minutes >= 1440L && minutes %% 1440L == 0L) {
    days <- minutes %/% 1440L
    return(if (days == 1L) "All day" else paste(days, "days"))
  }
  if (minutes < 60L) {
    return(paste(minutes, if (minutes == 1L) "minute" else "minutes"))
  }
  hours <- minutes %/% 60L
  remainder <- minutes %% 60L
  hour_label <- paste(hours, if (hours == 1L) "hour" else "hours")
  if (remainder == 0L) {
    return(hour_label)
  }
  paste(hour_label, remainder, if (remainder == 1L) "minute" else "minutes")
}

status_pill_ui <- function(status, label = NULL) {
  status <- tolower(ui_text(status, "unknown"))
  label <- label %||% paste("Status:", poll_display_status_label(status))
  shiny::span(
    class = paste("status-pill", paste0("status-", status)),
    label
  )
}

poll_effective_deadline <- function(poll, options = NULL) {
  stored_deadline <- poll$response_deadline[[1]] %||% ""
  if (nzchar(stored_deadline)) {
    return(stored_deadline)
  }
  if (!is.null(options) && nrow(options) > 0) {
    return(as.character(latest_option_date(options, poll$timezone[[1]])))
  }
  ""
}

poll_display_status <- function(poll, options = NULL) {
  status <- tolower(ui_text(poll$status[[1]], "unknown"))
  response_deadline <- poll_effective_deadline(poll, options)
  if (identical(status, "open") && deadline_has_passed(response_deadline, poll$timezone[[1]])) {
    return("expired")
  }
  status
}

poll_display_status_label <- function(status) {
  labels <- c(
    open = "Open",
    expired = "Expired",
    closed = "Closed",
    finalized = "Finalized",
    unknown = "Unknown"
  )
  value <- unname(labels[tolower(ui_text(status, "unknown"))])
  if (is.na(value)) "Unknown" else value
}

poll_accepts_responses <- function(poll, options = NULL) {
  identical(poll_display_status(poll, options), "open")
}

closed_poll_contact_message <- function(poll) {
  paste0(
    "This booking link is closed. Contact ",
    ui_text(poll$organizer_name[[1]], "the organizer"),
    " at ",
    ui_text(poll$organizer_email[[1]], "the organizer's email"),
    " to ask whether the link can be reopened. Confirmation of the final meeting time will follow from the organizer."
  )
}

finalized_poll_contact_message <- function(poll) {
  paste0(
    "The organizer has finalized this poll. Contact ",
    ui_text(poll$organizer_name[[1]], "the organizer"),
    " at ",
    ui_text(poll$organizer_email[[1]], "the organizer's email"),
    " if you have questions. Confirmation of the final meeting time will follow from the organizer."
  )
}

closed_poll_contact_ui <- function(poll, title = "This booking link is closed") {
  response_contact_state_ui(
    title = title,
    message = closed_poll_contact_message(poll),
    poll = poll
  )
}

status_banner_ui <- function(status, title, message, action = NULL) {
  status <- tolower(ui_text(status, "unknown"))
  shiny::div(
    class = paste("status-banner", paste0("status-banner-", status)),
    shiny::div(
      shiny::strong(title),
      shiny::p(message)
    ),
    action
  )
}

option_time_ui <- function(option, timezone, heading = "h3", show_context = TRUE) {
  primary <- format_readable_option_for_option(option, timezone)
  reference_utc <- if ("start_datetime" %in% names(option)) option$start_datetime[[1]] else NULL
  context <- timezone_label_with_abbreviation(timezone, reference_utc)
  heading_tag <- shiny::tags[[heading]]
  shiny::div(
    class = "time-display",
    heading_tag(class = "time-display-primary", primary),
    if (isTRUE(show_context)) {
      shiny::span(class = "time-display-secondary", context)
    }
  )
}

metric_card_ui <- function(label, value, helper = NULL, emphasis = FALSE) {
  shiny::div(
    class = paste("summary-card", if (emphasis) "summary-card-emphasis" else ""),
    shiny::div(class = "summary-label", label),
    shiny::div(class = "summary-value", ui_text(value, "Not available")),
    if (!is.null(helper) && nzchar(ui_text(helper))) {
      shiny::div(class = "summary-helper", helper)
    }
  )
}

copy_field_ui <- function(input_id, label, value, helper = NULL, sensitive = FALSE) {
  shiny::div(
    class = "copy-field",
    shiny::tags$label(`for` = input_id, label),
    shiny::div(
      class = "copy-row",
      shiny::tags$input(
        id = input_id,
        class = "form-control copy-input",
        type = "text",
        value = value,
        readonly = "readonly"
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-secondary copy-button",
        `data-copy-target` = input_id,
        "Copy"
      )
    ),
    if (sensitive) {
      shiny::p(class = "helper-text warning-text", "Keep this private. Anyone with this link can view organizer results.")
    },
    if (!is.null(helper) && nzchar(ui_text(helper))) {
      shiny::p(class = "helper-text", helper)
    }
  )
}

availability_short_label <- function(value) {
  labels <- c(
    pending = "Pending",
    preferred = "Preferred",
    available = "Available",
    unavailable = "Unavailable",
    missing = "Missing"
  )
  unname(labels[as.character(value)] %||% "Missing")
}

availability_hint <- function(value) {
  hints <- c(
    pending = "Not answered yet",
    preferred = "Can attend; works especially well",
    available = "Can attend",
    unavailable = "Cannot attend",
    missing = "No response yet"
  )
  unname(hints[as.character(value)] %||% "No response yet")
}

availability_badge_ui <- function(value, label = NULL) {
  value <- as.character(value %||% "missing")
  shiny::span(
    class = paste("availability-badge", paste0("availability-", value)),
    label %||% availability_short_label(value)
  )
}

availability_legend_ui <- function() {
  shiny::div(
    class = "availability-legend",
    availability_badge_ui("preferred", "Preferred"),
    availability_badge_ui("available", "Available"),
    availability_badge_ui("unavailable", "Unavailable"),
    availability_badge_ui("missing", "Missing")
  )
}

response_availability_choices <- function() {
  c(
    "Preferred" = "preferred",
    "Available" = "available",
    "Unavailable" = "unavailable"
  )
}

response_availability_cycle <- function() {
  c("pending", "available", "preferred", "unavailable")
}

availability_icon <- function(value) {
  icons <- c(
    pending = "○",
    preferred = "★",
    available = "✓",
    unavailable = "×",
    missing = "○"
  )
  unname(icons[as.character(value)] %||% "○")
}

availability_cycle_button_ui <- function(input_id, label = "Pending", value = "pending", readonly = FALSE) {
  value <- as.character(value %||% "pending")
  if (!value %in% response_availability_cycle()) {
    value <- "pending"
  }
  shiny::tags$button(
    type = "button",
    class = paste("availability-cycle-button", paste0("availability-state-", value), if (isTRUE(readonly)) "availability-cycle-readonly" else ""),
    `data-availability-cycle` = if (isTRUE(readonly)) NULL else "true",
    `data-availability-input` = input_id,
    `data-availability-value` = if (identical(value, "pending")) "" else value,
    `data-availability-state` = value,
    disabled = if (isTRUE(readonly)) "disabled" else NULL,
    `aria-label` = paste(label, "availability is", availability_short_label(value), if (isTRUE(readonly)) "" else "Activate to change response."),
    shiny::span(class = "availability-cycle-icon", availability_icon(value)),
    shiny::span(class = "availability-cycle-label", availability_short_label(value)),
    shiny::span(class = "availability-cycle-hint", availability_hint(value))
  )
}

response_notice_ui <- function(title, message, variant = "info") {
  shiny::div(
    class = paste("response-notice", paste0("response-notice-", variant)),
    shiny::strong(title),
    shiny::p(message)
  )
}

response_privacy_callout_ui <- function() {
  response_notice_ui(
    "Your information is private to the organizer",
    "Other verified participants can see your name and availability. Your email and comments are visible only to the organizer.",
    "privacy"
  )
}

timezone_selector_ui <- function(ns, input_id = "timezone_override", selected = device_timezone_choice(), label = "Times shown in") {
  timezone_choices <- stats::setNames(OlsonNames(), OlsonNames())
  choices <- c("Use my device time zone" = device_timezone_choice(), timezone_choices)
  shiny::selectizeInput(
    ns(input_id),
    label,
    choices = choices,
    selected = selected %||% device_timezone_choice(),
    options = list(maxOptions = 2000)
  )
}

response_contact_state_ui <- function(title, message, poll) {
  shiny::div(
    class = "empty-state response-contact-state",
    shiny::div(class = "empty-state-mark", "!"),
    shiny::h2(title),
    shiny::p(message),
    shiny::div(
      class = "response-contact-card",
      shiny::span(class = "detail-label", "Organizer contact"),
      shiny::strong(ui_text(poll$organizer_name[[1]], "The organizer")),
      shiny::span(ui_text(poll$organizer_email[[1]], "Organizer email unavailable"))
    )
  )
}

response_success_ui <- function(poll) {
  shiny::div(
    class = "response-success-panel",
    response_notice_ui(
      "Final confirmation will follow",
      "The organizer will review all responses through their private dashboard and follow up with the final meeting time.",
      "success"
    ),
    shiny::div(
      class = "response-contact-card",
      shiny::span(class = "detail-label", "Organizer"),
      shiny::strong(ui_text(poll$organizer_name[[1]], "The organizer")),
      shiny::span(ui_text(poll$organizer_email[[1]], "Organizer email unavailable"))
    )
  )
}

validation_summary_ui <- function(message) {
  shiny::div(
    class = "validation-summary",
    shiny::strong("Check this section"),
    shiny::p(message)
  )
}

poll_detail_items <- function(poll, options = NULL) {
  location <- ui_text(poll$location_details[[1]], "")
  location_type <- ui_text(poll$location_type[[1]], "")
  location_value <- if (nzchar(location)) {
    paste(location_type, location, sep = ": ")
  } else {
    location_type
  }
  reference_utc <- if (!is.null(options) && nrow(options) > 0) options$start_datetime[[1]] else NULL

  list(
    list(label = "Organizer", value = poll$organizer_name[[1]]),
    list(label = "Duration", value = format_duration_label(poll$duration_minutes[[1]])),
    list(label = "Time zone", value = timezone_with_offset_label(poll$timezone[[1]], reference_utc)),
    list(label = "Link expiry", value = format_deadline_label(poll_effective_deadline(poll, options))),
    list(label = "Location", value = location_value)
  )
}

response_poll_detail_items <- function(poll, options = NULL, viewer_timezone = NULL) {
  location <- ui_text(poll$location_details[[1]], "")
  location_type <- ui_text(poll$location_type[[1]], "")
  location_value <- if (nzchar(location)) {
    paste(location_type, location, sep = ": ")
  } else {
    location_type
  }
  reference_utc <- if (!is.null(options) && nrow(options) > 0) options$start_datetime[[1]] else NULL
  viewer_timezone <- viewer_timezone %||% poll$timezone[[1]]
  times_shown <- timezone_label_with_abbreviation(viewer_timezone, reference_utc)

  list(
    list(label = "Organizer", value = poll$organizer_name[[1]]),
    list(label = "Duration", value = format_duration_label(poll$duration_minutes[[1]])),
    list(label = "Link expiry", value = format_deadline_label(poll_effective_deadline(poll, options))),
    list(label = "Location", value = location_value),
    list(label = "Times shown", value = times_shown)
  )
}
