smtp_config <- function() {
  port <- suppressWarnings(as.integer(Sys.getenv("SMTP_PORT", unset = "587")))
  if (is.na(port)) {
    port <- 587L
  }
  list(
    host = Sys.getenv("SMTP_HOST", unset = ""),
    port = port,
    username = Sys.getenv("SMTP_USERNAME", unset = ""),
    password = Sys.getenv("SMTP_PASSWORD", unset = ""),
    from = Sys.getenv("SMTP_FROM", unset = ""),
    use_ssl = truthy_env("SMTP_USE_SSL", default = FALSE)
  )
}

smtp_is_configured <- function(config = smtp_config()) {
  nzchar(config$host) && nzchar(config$from)
}

magic_code_email_text <- function(code, purpose = "login") {
  purpose <- sanitize_text(purpose, max_chars = 80, required = FALSE, field = "Code purpose")
  paste(
    paste0("Your Meeting Availability Poll ", purpose, " code is:"),
    "",
    code,
    "",
    paste0("This code expires in ", magic_code_expires_minutes(), " minutes."),
    "If you did not request this code, you can ignore this email.",
    sep = "\n"
  )
}

owner_access_request_email_text <- function(first_name, last_name, email) {
  first_name <- sanitize_text(first_name, max_chars = 80, required = TRUE, field = "First name")
  last_name <- sanitize_text(last_name, max_chars = 80, required = TRUE, field = "Last name")
  email <- validate_email(email, field = "Organizer email")
  paste(
    "A new organizer access request was submitted for Meeting Availability Poll.",
    "",
    paste("Name:", paste(first_name, last_name)),
    paste("Email:", email),
    "",
    "Sign in to the app as the main owner to approve or deny this request.",
    "No action links are included in this email.",
    sep = "\n"
  )
}

email_subject_text <- function(subject, max_chars = 180) {
  subject <- sanitize_text(subject, max_chars = max_chars, required = TRUE, field = "Email subject")
  gsub("[\r\n\t]+", " ", subject)
}

response_notification_text <- function(type, poll_title, participant_name, link, action = "submitted") {
  type <- sanitize_text(type, max_chars = 20, required = TRUE, field = "Notification type")
  poll_title <- sanitize_text(poll_title, max_chars = 160, required = TRUE, field = "Poll title")
  participant_name <- sanitize_text(participant_name, max_chars = 160, required = TRUE, field = "Participant name")
  link <- sanitize_text(link, max_chars = 2000, required = TRUE, field = "Poll link")
  action <- sanitize_text(action, max_chars = 20, required = TRUE, field = "Response action")
  if (!action %in% c("submitted", "updated")) {
    stop("Response action is invalid.", call. = FALSE)
  }
  if (!type %in% c("organizer", "participant")) {
    stop("Notification type is invalid.", call. = FALSE)
  }

  if (identical(type, "organizer")) {
    return(paste(
      paste(participant_name, action, "availability for your meeting poll."),
      "",
      paste("Poll:", poll_title),
      paste("Participant:", participant_name),
      "",
      "View this poll in your organizer workspace:",
      link,
      "",
      "This link requires organizer sign-in. Participant emails, comments, and private admin links are not included in this message.",
      sep = "\n"
    ))
  }

  paste(
    "Your availability response was saved.",
    "",
    paste("Poll:", poll_title),
    "",
    "You can return to the poll to view or edit your availability:",
    link,
    "",
    "This link requires your participant email-code sign-in.",
    sep = "\n"
  )
}

send_magic_code_email <- function(email, code, purpose = "login", subject = "Your Meeting Availability Poll code", config = smtp_config()) {
  email <- validate_email(email, field = "Email")
  code <- validate_magic_code(code)

  sender <- getOption("meeting_poll.magic_code_sender", NULL)
  if (is.function(sender)) {
    sender(email, code)
    return(list(sent = TRUE, dev_code = ""))
  }

  if (!smtp_is_configured(config)) {
    if (allow_dev_auth_code_display()) {
      return(list(sent = FALSE, dev_code = code))
    }
    stop("Email-code login is not configured. Set SMTP environment variables before using this feature.", call. = FALSE)
  }

  if (requireNamespace("blastula", quietly = TRUE)) {
    email_body <- blastula::compose_email(
      body = blastula::md(magic_code_email_text(code, purpose = purpose))
    )
    credentials <- blastula::creds_envvar(
      user = if (nzchar(config$username)) config$username else NULL,
      pass_envvar = "SMTP_PASSWORD",
      host = config$host,
      port = config$port,
      use_ssl = config$use_ssl
    )
    blastula::smtp_send(
      email = email_body,
      from = config$from,
      to = email,
      subject = subject,
      credentials = credentials
    )
    return(list(sent = TRUE, dev_code = ""))
  }

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Install the blastula or curl package to send email login codes.", call. = FALSE)
  }

  message <- paste0(
    "To: ", email, "\r\n",
    "From: ", config$from, "\r\n",
    "Subject: ", subject, "\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: text/plain; charset=UTF-8\r\n\r\n",
    magic_code_email_text(code, purpose = purpose)
  )
  smtp_server <- paste0(if (isTRUE(config$use_ssl)) "smtps://" else "smtp://", config$host, ":", config$port)
  curl::send_mail(
    mail_from = config$from,
    mail_rcpt = email,
    message = charToRaw(message),
    smtp_server = smtp_server,
    use_ssl = if (isTRUE(config$use_ssl)) "force" else "try",
    username = if (nzchar(config$username)) config$username else NULL,
    password = if (nzchar(config$password)) config$password else NULL,
    verbose = FALSE
  )
  list(sent = TRUE, dev_code = "")
}

send_organizer_magic_code_email <- function(email, code, config = smtp_config()) {
  email <- validate_email(email, field = "Organizer email")
  send_magic_code_email(
    email = email,
    code = code,
    purpose = "organizer login",
    subject = "Your organizer login code",
    config = config
  )
}

send_participant_magic_code_email <- function(email, code, poll_title = "", config = smtp_config()) {
  email <- validate_email(email, field = "Participant email")
  poll_title <- sanitize_text(poll_title, max_chars = 160, required = FALSE, field = "Poll title")
  purpose <- if (nzchar(poll_title)) {
    paste("poll response for", poll_title)
  } else {
    "poll response"
  }
  send_magic_code_email(
    email = email,
    code = code,
    purpose = purpose,
    subject = "Your poll response code",
    config = config
  )
}

send_owner_access_request_email <- function(request, config = smtp_config()) {
  main_owner <- app_main_owner_email()
  first_name <- sanitize_text(request$first_name[[1]], max_chars = 80, required = TRUE, field = "First name")
  last_name <- sanitize_text(request$last_name[[1]], max_chars = 80, required = TRUE, field = "Last name")
  email <- validate_email(request$email[[1]], field = "Organizer email")

  sender <- getOption("meeting_poll.owner_request_sender", NULL)
  if (is.function(sender)) {
    sender(main_owner, list(first_name = first_name, last_name = last_name, email = email))
    return(list(sent = TRUE))
  }

  if (!smtp_is_configured(config)) {
    stop("Owner access request notifications are not configured. Set SMTP environment variables before using this feature.", call. = FALSE)
  }

  body_text <- owner_access_request_email_text(first_name, last_name, email)
  if (requireNamespace("blastula", quietly = TRUE)) {
    email_body <- blastula::compose_email(body = blastula::md(body_text))
    credentials <- blastula::creds_envvar(
      user = if (nzchar(config$username)) config$username else NULL,
      pass_envvar = "SMTP_PASSWORD",
      host = config$host,
      port = config$port,
      use_ssl = config$use_ssl
    )
    blastula::smtp_send(
      email = email_body,
      from = config$from,
      to = main_owner,
      subject = "Organizer access request",
      credentials = credentials
    )
    return(list(sent = TRUE))
  }

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Install the blastula or curl package to send organizer access request email.", call. = FALSE)
  }

  message <- paste0(
    "To: ", main_owner, "\r\n",
    "From: ", config$from, "\r\n",
    "Subject: Organizer access request\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: text/plain; charset=UTF-8\r\n\r\n",
    body_text
  )
  smtp_server <- paste0(if (isTRUE(config$use_ssl)) "smtps://" else "smtp://", config$host, ":", config$port)
  curl::send_mail(
    mail_from = config$from,
    mail_rcpt = main_owner,
    message = charToRaw(message),
    smtp_server = smtp_server,
    use_ssl = if (isTRUE(config$use_ssl)) "force" else "try",
    username = if (nzchar(config$username)) config$username else NULL,
    password = if (nzchar(config$password)) config$password else NULL,
    verbose = FALSE
  )
  list(sent = TRUE)
}

send_response_notification_email <- function(to, subject, body_text, config = smtp_config()) {
  to <- validate_email(to, field = "Notification recipient")
  subject <- email_subject_text(subject)
  body_text <- sanitize_text(body_text, max_chars = 8000, required = TRUE, field = "Notification body")

  sender <- getOption("meeting_poll.response_notification_sender", NULL)
  if (is.function(sender)) {
    sender(to = to, subject = subject, body = body_text)
    return(list(sent = TRUE))
  }

  if (!smtp_is_configured(config)) {
    stop("Response notifications are not configured. Set SMTP environment variables before using this feature.", call. = FALSE)
  }

  if (requireNamespace("blastula", quietly = TRUE)) {
    email_body <- blastula::compose_email(body = blastula::md(body_text))
    credentials <- blastula::creds_envvar(
      user = if (nzchar(config$username)) config$username else NULL,
      pass_envvar = "SMTP_PASSWORD",
      host = config$host,
      port = config$port,
      use_ssl = config$use_ssl
    )
    blastula::smtp_send(
      email = email_body,
      from = config$from,
      to = to,
      subject = subject,
      credentials = credentials
    )
    return(list(sent = TRUE))
  }

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Install the blastula or curl package to send response notification email.", call. = FALSE)
  }

  message <- paste0(
    "To: ", to, "\r\n",
    "From: ", config$from, "\r\n",
    "Subject: ", subject, "\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: text/plain; charset=UTF-8\r\n\r\n",
    body_text
  )
  smtp_server <- paste0(if (isTRUE(config$use_ssl)) "smtps://" else "smtp://", config$host, ":", config$port)
  curl::send_mail(
    mail_from = config$from,
    mail_rcpt = to,
    message = charToRaw(message),
    smtp_server = smtp_server,
    use_ssl = if (isTRUE(config$use_ssl)) "force" else "try",
    username = if (nzchar(config$username)) config$username else NULL,
    password = if (nzchar(config$password)) config$password else NULL,
    verbose = FALSE
  )
  list(sent = TRUE)
}

send_response_submission_notifications <- function(
  poll,
  participant_name,
  participant_email,
  response_link,
  organizer_link,
  action = "submitted",
  config = smtp_config()
) {
  poll_title <- sanitize_text(poll$title[[1]], max_chars = 160, required = TRUE, field = "Poll title")
  organizer_email <- validate_email(poll$organizer_email[[1]], field = "Organizer email")
  participant_email <- validate_email(participant_email, field = "Participant email")
  participant_name <- sanitize_text(participant_name, max_chars = 160, required = TRUE, field = "Participant name")
  action <- sanitize_text(action, max_chars = 20, required = TRUE, field = "Response action")

  organizer_body <- response_notification_text(
    type = "organizer",
    poll_title = poll_title,
    participant_name = participant_name,
    link = organizer_link,
    action = action
  )
  participant_body <- response_notification_text(
    type = "participant",
    poll_title = poll_title,
    participant_name = participant_name,
    link = response_link,
    action = action
  )

  organizer_result <- tryCatch(
    send_response_notification_email(
      to = organizer_email,
      subject = paste("New response for", poll_title),
      body_text = organizer_body,
      config = config
    ),
    error = function(e) e
  )
  participant_result <- tryCatch(
    send_response_notification_email(
      to = participant_email,
      subject = paste("Your response was saved for", poll_title),
      body_text = participant_body,
      config = config
    ),
    error = function(e) e
  )

  failures <- vapply(list(organizer_result, participant_result), inherits, logical(1), what = "error")
  if (any(failures)) {
    stop("One or more response notification emails could not be sent.", call. = FALSE)
  }
  list(sent = TRUE, organizer = organizer_result, participant = participant_result)
}
