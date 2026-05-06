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
    shiny::p("This form collects your name, email, organization, availability, and optional comments so the organizer can choose a meeting time. Results are visible only through the private organizer link.")
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

status_pill_ui <- function(status, label = NULL) {
  status <- tolower(ui_text(status, "unknown"))
  label <- label %||% paste("Status:", poll_display_status_label(status))
  shiny::span(
    class = paste("status-pill", paste0("status-", status)),
    label
  )
}

poll_display_status <- function(poll) {
  status <- tolower(ui_text(poll$status[[1]], "unknown"))
  if (identical(status, "open") && deadline_has_passed(poll$response_deadline[[1]], poll$timezone[[1]])) {
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

poll_accepts_responses <- function(poll) {
  identical(poll_display_status(poll), "open")
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
  empty_state_ui(
    title,
    closed_poll_contact_message(poll)
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
    preferred = "Preferred",
    available = "Available",
    unavailable = "Unavailable",
    missing = "Missing"
  )
  unname(labels[as.character(value)] %||% "Missing")
}

availability_hint <- function(value) {
  hints <- c(
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

validation_summary_ui <- function(message) {
  shiny::div(
    class = "validation-summary",
    shiny::strong("Check this section"),
    shiny::p(message)
  )
}

poll_detail_items <- function(poll) {
  location <- ui_text(poll$location_details[[1]], "")
  location_type <- ui_text(poll$location_type[[1]], "")
  location_value <- if (nzchar(location)) {
    paste(location_type, location, sep = ": ")
  } else {
    location_type
  }

  list(
    list(label = "Organizer", value = poll$organizer_name[[1]]),
    list(label = "Duration", value = paste(poll$duration_minutes[[1]], "minutes")),
    list(label = "Time zone", value = poll$timezone[[1]]),
    list(label = "Link expiry", value = format_deadline_label(poll$response_deadline[[1]])),
    list(label = "Location", value = location_value)
  )
}
