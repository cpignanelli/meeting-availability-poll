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

magic_code_email_text <- function(code) {
  paste(
    "Your Meeting Availability Poll organizer code is:",
    "",
    code,
    "",
    paste0("This code expires in ", magic_code_expires_minutes(), " minutes."),
    "If you did not request this code, you can ignore this email.",
    sep = "\n"
  )
}

send_organizer_magic_code_email <- function(email, code, config = smtp_config()) {
  email <- validate_email(email, field = "Organizer email")
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
    stop("Organizer email login is not configured. Set SMTP environment variables before using this feature.", call. = FALSE)
  }

  if (requireNamespace("blastula", quietly = TRUE)) {
    email_body <- blastula::compose_email(
      body = blastula::md(magic_code_email_text(code))
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
      subject = "Your organizer login code",
      credentials = credentials
    )
    return(list(sent = TRUE, dev_code = ""))
  }

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Install the blastula or curl package to send organizer login email.", call. = FALSE)
  }

  message <- paste0(
    "To: ", email, "\r\n",
    "From: ", config$from, "\r\n",
    "Subject: Your organizer login code\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: text/plain; charset=UTF-8\r\n\r\n",
    magic_code_email_text(code)
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
