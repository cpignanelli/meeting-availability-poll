db_now <- function() {
  utc_timestamp()
}

db_scalar_id <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[[1]]
}

audit_event <- function(conn, poll_id, event_type, event_detail = "") {
  if (is_mongo_connection(conn)) {
    return(mongo_audit_event(conn, poll_id, event_type, event_detail))
  }
  DBI::dbExecute(
    conn,
    "INSERT INTO audit_log (poll_id, event_type, event_detail, created_at)
     VALUES (?, ?, ?, ?)",
    params = list(poll_id, event_type, sanitize_text(event_detail, max_chars = 300), db_now())
  )
}

create_poll_record <- function(conn, poll, options, expected = data.frame()) {
  if (is_mongo_connection(conn)) {
    return(mongo_create_poll_record(conn, poll, options, expected))
  }
  admin_token <- generate_token()
  response_token <- generate_token()
  created_at <- db_now()

  result <- with_db_transaction(conn, function(tx) {
    DBI::dbExecute(
      tx,
      "INSERT INTO polls (
        admin_token_hash, response_token, title, description, organizer_name, organizer_email,
        organizer_email_normalized,
        duration_minutes, timezone, location_type, location_details, response_deadline,
        status, created_at, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'open', ?, ?)",
      params = list(
        hash_token(admin_token),
        response_token,
        poll$title,
        poll$description,
        poll$organizer_name,
        poll$organizer_email,
        normalize_email(poll$organizer_email),
        poll$duration_minutes,
        poll$timezone,
        poll$location_type,
        poll$location_details,
        poll$response_deadline,
        created_at,
        created_at
      )
    )
    poll_id <- db_scalar_id(tx)

    for (i in seq_len(nrow(options))) {
      DBI::dbExecute(
        tx,
        "INSERT INTO poll_options (
          poll_id, start_datetime, end_datetime, display_label, option_order
        ) VALUES (?, ?, ?, ?, ?)",
        params = list(
          poll_id,
          options$start_datetime[[i]],
          options$end_datetime[[i]],
          options$display_label[[i]],
          options$option_order[[i]]
        )
      )
    }

    if (nrow(expected) > 0) {
      for (i in seq_len(nrow(expected))) {
        DBI::dbExecute(
          tx,
          "INSERT INTO expected_participants (
            poll_id, name, email, organization, is_required
          ) VALUES (?, ?, ?, ?, ?)",
          params = list(
            poll_id,
            expected$name[[i]],
            expected$email[[i]],
            expected$organization[[i]],
            expected$is_required[[i]]
          )
        )
      }
    }

    audit_event(tx, poll_id, "poll_created", "Poll created")
    list(
      poll_id = poll_id,
      admin_token = admin_token,
      response_token = response_token
    )
  })

  result
}

get_poll_by_response_token <- function(conn, response_token) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_poll_by_response_token(conn, response_token))
  }
  response_token <- validate_token(response_token, field = "Response link token")
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM polls WHERE response_token = ? LIMIT 1",
    params = list(response_token)
  )
  if (nrow(result) == 0) NULL else result
}

get_poll_by_admin_token <- function(conn, admin_token) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_poll_by_admin_token(conn, admin_token))
  }
  admin_token <- validate_token(admin_token, field = "Admin link token")
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM polls WHERE admin_token_hash = ? LIMIT 1",
    params = list(hash_token(admin_token))
  )
  if (nrow(result) == 0) NULL else result
}

get_poll_for_organizer <- function(conn, poll_id, organizer_email) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_poll_for_organizer(conn, poll_id, organizer_email))
  }
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  poll_id <- as.integer(poll_id)
  if (is.na(poll_id)) {
    return(NULL)
  }
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM polls
     WHERE poll_id = ? AND organizer_email_normalized = ?
     LIMIT 1",
    params = list(poll_id, organizer_email)
  )
  if (nrow(result) == 0) NULL else result
}

list_polls_for_organizer <- function(conn, organizer_email) {
  if (is_mongo_connection(conn)) {
    return(mongo_list_polls_for_organizer(conn, organizer_email))
  }
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  DBI::dbGetQuery(
    conn,
    "SELECT
       p.*,
       COUNT(DISTINCT o.option_id) AS option_count,
       COUNT(DISTINCT participant.participant_id) AS response_count
     FROM polls p
     LEFT JOIN poll_options o ON o.poll_id = p.poll_id
     LEFT JOIN participants participant ON participant.poll_id = p.poll_id
     WHERE p.organizer_email_normalized = ?
     GROUP BY p.poll_id
     ORDER BY p.updated_at DESC, p.created_at DESC",
    params = list(organizer_email)
  )
}

get_poll_options <- function(conn, poll_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_poll_options(conn, poll_id))
  }
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM poll_options WHERE poll_id = ? ORDER BY option_order, start_datetime",
    params = list(poll_id)
  )
}

get_expected_participants <- function(conn, poll_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_expected_participants(conn, poll_id))
  }
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM expected_participants WHERE poll_id = ? ORDER BY name, email",
    params = list(poll_id)
  )
}

get_participants <- function(conn, poll_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_participants(conn, poll_id))
  }
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM participants WHERE poll_id = ? ORDER BY submitted_at, name",
    params = list(poll_id)
  )
}

get_responses_for_poll <- function(conn, poll_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_responses_for_poll(conn, poll_id))
  }
  DBI::dbGetQuery(
    conn,
    "SELECT
       r.response_id, r.participant_id, r.option_id, r.availability, r.comment,
       p.name, p.email, p.organization,
       o.display_label, o.start_datetime, o.end_datetime, o.option_order
     FROM responses r
     INNER JOIN participants p ON p.participant_id = r.participant_id
     INNER JOIN poll_options o ON o.option_id = r.option_id
     WHERE p.poll_id = ?
     ORDER BY p.name, o.option_order",
    params = list(poll_id)
  )
}

get_finalized_meeting <- function(conn, poll_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_finalized_meeting(conn, poll_id))
  }
  result <- DBI::dbGetQuery(
    conn,
    "SELECT f.*, o.display_label, o.start_datetime, o.end_datetime
     FROM finalized_meetings f
     INNER JOIN poll_options o ON o.option_id = f.selected_option_id
     WHERE f.poll_id = ?
     LIMIT 1",
    params = list(poll_id)
  )
  if (nrow(result) == 0) NULL else result
}

get_poll_dashboard_data <- function(conn, poll_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_poll_dashboard_data(conn, poll_id))
  }
  poll <- DBI::dbGetQuery(conn, "SELECT * FROM polls WHERE poll_id = ? LIMIT 1", params = list(poll_id))
  if (nrow(poll) == 0) {
    return(NULL)
  }
  options <- get_poll_options(conn, poll_id)
  expected <- get_expected_participants(conn, poll_id)
  participants <- get_participants(conn, poll_id)
  responses <- get_responses_for_poll(conn, poll_id)
  finalized <- get_finalized_meeting(conn, poll_id)
  ranked <- rank_time_options(options, responses, participants, expected, poll$timezone[[1]])
  missing_expected <- find_missing_expected_participants(expected, participants)
  list(
    poll = poll,
    options = options,
    expected = expected,
    participants = participants,
    responses = responses,
    finalized = finalized,
    ranked = ranked,
    missing_expected = missing_expected,
    heatmap = build_availability_matrix(options, responses, participants, poll$timezone[[1]])
  )
}

submit_poll_response <- function(conn, poll_id, participant, response_values, comment = "") {
  if (is_mongo_connection(conn)) {
    return(mongo_submit_poll_response(conn, poll_id, participant, response_values, comment))
  }
  submitted_at <- db_now()
  participant$email <- validate_optional_email(participant$email, field = "Participant email")
  participant$organization <- sanitize_text(participant$organization %||% "", max_chars = 160, required = FALSE, field = "Organization")
  has_email <- !is.na(participant$email) && nzchar(participant$email)

  with_db_transaction(conn, function(tx) {
    existing <- if (has_email) {
      DBI::dbGetQuery(
        tx,
        "SELECT participant_id FROM participants WHERE poll_id = ? AND email = ? LIMIT 1",
        params = list(poll_id, participant$email)
      )
    } else {
      data.frame()
    }

    if (nrow(existing) == 0) {
      DBI::dbExecute(
        tx,
        "INSERT INTO participants (poll_id, name, email, organization, submitted_at, updated_at)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(
          poll_id,
          participant$name,
          participant$email,
          participant$organization,
          submitted_at,
          submitted_at
        )
      )
      participant_id <- db_scalar_id(tx)
    } else {
      participant_id <- existing$participant_id[[1]]
      DBI::dbExecute(
        tx,
        "UPDATE participants
         SET name = ?, organization = ?, updated_at = ?
         WHERE participant_id = ?",
        params = list(participant$name, participant$organization, submitted_at, participant_id)
      )
      DBI::dbExecute(
        tx,
        "DELETE FROM responses WHERE participant_id = ?",
        params = list(participant_id)
      )
    }

    for (i in seq_len(nrow(response_values))) {
      DBI::dbExecute(
        tx,
        "INSERT INTO responses (participant_id, option_id, availability, comment)
         VALUES (?, ?, ?, ?)",
        params = list(
          participant_id,
          response_values$option_id[[i]],
          response_values$availability[[i]],
          comment
        )
      )
    }

    audit_event(tx, poll_id, "response_submitted", "Participant response submitted")
    participant_id
  })
}

finalize_meeting <- function(conn, poll_id, selected_option_id, final_notes = "") {
  if (is_mongo_connection(conn)) {
    return(mongo_finalize_meeting(conn, poll_id, selected_option_id, final_notes))
  }
  finalized_at <- db_now()
  final_notes <- sanitize_text(final_notes, max_chars = 2000)
  with_db_transaction(conn, function(tx) {
    existing <- DBI::dbGetQuery(
      tx,
      "SELECT final_id FROM finalized_meetings WHERE poll_id = ? LIMIT 1",
      params = list(poll_id)
    )
    if (nrow(existing) == 0) {
      DBI::dbExecute(
        tx,
        "INSERT INTO finalized_meetings (poll_id, selected_option_id, final_notes, finalized_at)
         VALUES (?, ?, ?, ?)",
        params = list(poll_id, selected_option_id, final_notes, finalized_at)
      )
    } else {
      DBI::dbExecute(
        tx,
        "UPDATE finalized_meetings
         SET selected_option_id = ?, final_notes = ?, finalized_at = ?
         WHERE poll_id = ?",
        params = list(selected_option_id, final_notes, finalized_at, poll_id)
      )
    }
    DBI::dbExecute(
      tx,
      "UPDATE polls SET status = 'finalized', updated_at = ?, closed_at = COALESCE(closed_at, ?)
       WHERE poll_id = ?",
      params = list(finalized_at, finalized_at, poll_id)
    )
    audit_event(tx, poll_id, "poll_finalized", "Meeting finalized")
  })
  invisible(TRUE)
}

close_poll <- function(conn, poll_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_close_poll(conn, poll_id))
  }
  closed_at <- db_now()
  with_db_transaction(conn, function(tx) {
    DBI::dbExecute(
      tx,
      "UPDATE polls SET status = 'closed', updated_at = ?, closed_at = COALESCE(closed_at, ?)
       WHERE poll_id = ? AND status != 'finalized'",
      params = list(closed_at, closed_at, poll_id)
    )
    audit_event(tx, poll_id, "poll_closed", "Poll closed")
  })
  invisible(TRUE)
}

reopen_poll <- function(conn, poll_id, response_deadline = "") {
  if (is_mongo_connection(conn)) {
    return(mongo_reopen_poll(conn, poll_id, response_deadline))
  }
  response_deadline <- sanitize_text(response_deadline, max_chars = 40)
  reopened_at <- db_now()

  with_db_transaction(conn, function(tx) {
    current <- DBI::dbGetQuery(
      tx,
      "SELECT status FROM polls WHERE poll_id = ? LIMIT 1",
      params = list(poll_id)
    )
    if (nrow(current) == 0) {
      stop("Poll not found.", call. = FALSE)
    }
    if (identical(current$status[[1]], "finalized")) {
      stop("Finalized polls cannot be reopened.", call. = FALSE)
    }

    DBI::dbExecute(
      tx,
      "UPDATE polls
       SET status = 'open', response_deadline = ?, updated_at = ?, closed_at = NULL
       WHERE poll_id = ? AND status != 'finalized'",
      params = list(response_deadline, reopened_at, poll_id)
    )
    audit_event(tx, poll_id, "poll_reopened", "Poll response link reopened")
  })

  invisible(TRUE)
}

owner_role_labels <- function() {
  c(
    main_owner = "Main owner",
    owner = "Approved owner",
    pending = "Pending approval",
    denied = "Access denied",
    revoked = "Access revoked",
    none = "No access"
  )
}

owner_role_label <- function(role) {
  labels <- owner_role_labels()
  label <- unname(labels[as.character(role)])
  if (is.na(label)) "No access" else label
}

get_owner_role <- function(conn, organizer_email) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_owner_role(conn, organizer_email))
  }
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  if (is_main_owner_email(organizer_email)) {
    return("main_owner")
  }

  owner <- DBI::dbGetQuery(
    conn,
    "SELECT status FROM approved_owners WHERE email_normalized = ? LIMIT 1",
    params = list(organizer_email)
  )
  if (nrow(owner) > 0) {
    status <- owner$status[[1]]
    if (identical(status, "approved")) {
      return("owner")
    }
    if (identical(status, "revoked")) {
      return("revoked")
    }
  }

  request <- DBI::dbGetQuery(
    conn,
    "SELECT status FROM owner_access_requests WHERE email_normalized = ? LIMIT 1",
    params = list(organizer_email)
  )
  if (nrow(request) > 0 && request$status[[1]] %in% c("pending", "denied")) {
    return(request$status[[1]])
  }

  "none"
}

owner_has_workspace_access <- function(conn, organizer_email) {
  get_owner_role(conn, organizer_email) %in% c("main_owner", "owner")
}

validate_owner_profile <- function(first_name, last_name, email) {
  first_name <- sanitize_text(first_name, max_chars = 80, required = TRUE, field = "First name")
  last_name <- sanitize_text(last_name, max_chars = 80, required = TRUE, field = "Last name")
  email <- validate_email(email, field = "Organizer email")
  list(
    first_name = first_name,
    last_name = last_name,
    email = email,
    email_normalized = email
  )
}

create_or_update_owner_access_request <- function(conn, first_name, last_name, email) {
  if (is_mongo_connection(conn)) {
    return(mongo_create_or_update_owner_access_request(conn, first_name, last_name, email))
  }
  profile <- validate_owner_profile(first_name, last_name, email)
  if (is_main_owner_email(profile$email)) {
    stop("The main owner can sign in directly.", call. = FALSE)
  }

  requested_at <- db_now()
  with_db_transaction(conn, function(tx) {
    owner <- DBI::dbGetQuery(
      tx,
      "SELECT status FROM approved_owners WHERE email_normalized = ? LIMIT 1",
      params = list(profile$email_normalized)
    )
    if (nrow(owner) > 0 && identical(owner$status[[1]], "approved")) {
      stop("This email already has organizer access. Sign in instead.", call. = FALSE)
    }

    existing <- DBI::dbGetQuery(
      tx,
      "SELECT request_id FROM owner_access_requests WHERE email_normalized = ? LIMIT 1",
      params = list(profile$email_normalized)
    )
    if (nrow(existing) == 0) {
      DBI::dbExecute(
        tx,
        "INSERT INTO owner_access_requests (
          first_name, last_name, email, email_normalized, status,
          requested_at, verified_at, reviewed_at, reviewed_by_email, updated_at
        ) VALUES (?, ?, ?, ?, 'pending', ?, ?, NULL, NULL, ?)",
        params = list(
          profile$first_name,
          profile$last_name,
          profile$email,
          profile$email_normalized,
          requested_at,
          requested_at,
          requested_at
        )
      )
      request_id <- db_scalar_id(tx)
    } else {
      request_id <- existing$request_id[[1]]
      DBI::dbExecute(
        tx,
        "UPDATE owner_access_requests
         SET first_name = ?, last_name = ?, email = ?, status = 'pending',
             requested_at = ?, verified_at = ?, reviewed_at = NULL,
             reviewed_by_email = NULL, updated_at = ?
         WHERE request_id = ?",
        params = list(
          profile$first_name,
          profile$last_name,
          profile$email,
          requested_at,
          requested_at,
          requested_at,
          request_id
        )
      )
    }

    DBI::dbGetQuery(
      tx,
      "SELECT * FROM owner_access_requests WHERE request_id = ? LIMIT 1",
      params = list(request_id)
    )
  })
}

list_owner_access_requests <- function(conn, reviewer_email, status = "pending") {
  if (is_mongo_connection(conn)) {
    return(mongo_list_owner_access_requests(conn, reviewer_email, status))
  }
  require_main_owner(reviewer_email)
  status <- sanitize_text(status, max_chars = 20, required = TRUE, field = "Request status")
  if (!status %in% c("pending", "approved", "denied")) {
    stop("Request status is invalid.", call. = FALSE)
  }
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM owner_access_requests
     WHERE status = ?
     ORDER BY requested_at ASC, updated_at ASC",
    params = list(status)
  )
}

list_approved_owners <- function(conn, reviewer_email, include_revoked = FALSE) {
  if (is_mongo_connection(conn)) {
    return(mongo_list_approved_owners(conn, reviewer_email, include_revoked))
  }
  require_main_owner(reviewer_email)
  if (isTRUE(include_revoked)) {
    return(DBI::dbGetQuery(
      conn,
      "SELECT * FROM approved_owners ORDER BY updated_at DESC, approved_at DESC"
    ))
  }
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM approved_owners
     WHERE status = 'approved'
     ORDER BY approved_at DESC, updated_at DESC"
  )
}

approve_owner_request <- function(conn, request_id, reviewer_email) {
  if (is_mongo_connection(conn)) {
    return(mongo_approve_owner_request(conn, request_id, reviewer_email))
  }
  require_main_owner(reviewer_email)
  reviewer_email <- validate_email(reviewer_email, field = "Reviewer email")
  request_id <- suppressWarnings(as.integer(request_id))
  if (is.na(request_id)) {
    stop("Access request is invalid.", call. = FALSE)
  }
  reviewed_at <- db_now()

  with_db_transaction(conn, function(tx) {
    request <- DBI::dbGetQuery(
      tx,
      "SELECT * FROM owner_access_requests WHERE request_id = ? LIMIT 1",
      params = list(request_id)
    )
    if (nrow(request) == 0) {
      stop("Access request not found.", call. = FALSE)
    }
    if (!identical(request$status[[1]], "pending")) {
      stop("Only pending access requests can be approved.", call. = FALSE)
    }
    if (is_main_owner_email(request$email_normalized[[1]])) {
      stop("The main owner does not need approval.", call. = FALSE)
    }

    existing_owner <- DBI::dbGetQuery(
      tx,
      "SELECT owner_id FROM approved_owners WHERE email_normalized = ? LIMIT 1",
      params = list(request$email_normalized[[1]])
    )
    if (nrow(existing_owner) == 0) {
      DBI::dbExecute(
        tx,
        "INSERT INTO approved_owners (
          first_name, last_name, email, email_normalized, status,
          approved_at, approved_by_email, revoked_at, updated_at
        ) VALUES (?, ?, ?, ?, 'approved', ?, ?, NULL, ?)",
        params = list(
          request$first_name[[1]],
          request$last_name[[1]],
          request$email[[1]],
          request$email_normalized[[1]],
          reviewed_at,
          reviewer_email,
          reviewed_at
        )
      )
    } else {
      DBI::dbExecute(
        tx,
        "UPDATE approved_owners
         SET first_name = ?, last_name = ?, email = ?, status = 'approved',
             approved_at = ?, approved_by_email = ?, revoked_at = NULL, updated_at = ?
         WHERE owner_id = ?",
        params = list(
          request$first_name[[1]],
          request$last_name[[1]],
          request$email[[1]],
          reviewed_at,
          reviewer_email,
          reviewed_at,
          existing_owner$owner_id[[1]]
        )
      )
    }

    DBI::dbExecute(
      tx,
      "UPDATE owner_access_requests
       SET status = 'approved', reviewed_at = ?, reviewed_by_email = ?, updated_at = ?
       WHERE request_id = ?",
      params = list(reviewed_at, reviewer_email, reviewed_at, request_id)
    )
  })
  invisible(TRUE)
}

deny_owner_request <- function(conn, request_id, reviewer_email) {
  if (is_mongo_connection(conn)) {
    return(mongo_deny_owner_request(conn, request_id, reviewer_email))
  }
  require_main_owner(reviewer_email)
  reviewer_email <- validate_email(reviewer_email, field = "Reviewer email")
  request_id <- suppressWarnings(as.integer(request_id))
  if (is.na(request_id)) {
    stop("Access request is invalid.", call. = FALSE)
  }
  reviewed_at <- db_now()

  with_db_transaction(conn, function(tx) {
    affected <- DBI::dbExecute(
      tx,
      "UPDATE owner_access_requests
       SET status = 'denied', reviewed_at = ?, reviewed_by_email = ?, updated_at = ?
       WHERE request_id = ? AND status = 'pending'",
      params = list(reviewed_at, reviewer_email, reviewed_at, request_id)
    )
    if (identical(as.integer(affected), 0L)) {
      stop("Only pending access requests can be denied.", call. = FALSE)
    }
  })
  invisible(TRUE)
}

revoke_approved_owner <- function(conn, owner_id, reviewer_email) {
  if (is_mongo_connection(conn)) {
    return(mongo_revoke_approved_owner(conn, owner_id, reviewer_email))
  }
  require_main_owner(reviewer_email)
  reviewer_email <- validate_email(reviewer_email, field = "Reviewer email")
  owner_id <- suppressWarnings(as.integer(owner_id))
  if (is.na(owner_id)) {
    stop("Approved owner is invalid.", call. = FALSE)
  }
  revoked_at <- db_now()

  with_db_transaction(conn, function(tx) {
    owner <- DBI::dbGetQuery(
      tx,
      "SELECT * FROM approved_owners WHERE owner_id = ? LIMIT 1",
      params = list(owner_id)
    )
    if (nrow(owner) == 0 || !identical(owner$status[[1]], "approved")) {
      stop("Only approved owners can be revoked.", call. = FALSE)
    }
    if (is_main_owner_email(owner$email_normalized[[1]])) {
      stop("The main owner cannot be revoked.", call. = FALSE)
    }

    DBI::dbExecute(
      tx,
      "UPDATE approved_owners
       SET status = 'revoked', revoked_at = ?, updated_at = ?
       WHERE owner_id = ?",
      params = list(revoked_at, revoked_at, owner_id)
    )
  })
  invisible(TRUE)
}

create_organizer_login_code <- function(conn, organizer_email, code = generate_magic_code()) {
  if (is_mongo_connection(conn)) {
    return(mongo_create_organizer_login_code(conn, organizer_email, code))
  }
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  code <- validate_magic_code(code)
  created_at <- db_now()
  expires_at <- as_utc_string(add_minutes(parse_utc_timestamp(created_at), magic_code_expires_minutes()))
  code_hash <- hash_magic_code(organizer_email, code)

  DBI::dbExecute(
    conn,
    "INSERT INTO organizer_login_codes (
      organizer_email_normalized, code_hash, created_at, expires_at, used_at, attempts
    ) VALUES (?, ?, ?, ?, NULL, 0)",
    params = list(organizer_email, code_hash, created_at, expires_at)
  )

  list(email = organizer_email, code = code, expires_at = expires_at)
}

verify_organizer_login_code <- function(conn, organizer_email, code) {
  if (is_mongo_connection(conn)) {
    return(mongo_verify_organizer_login_code(conn, organizer_email, code))
  }
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  code <- validate_magic_code(code)
  now <- db_now()

  codes <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM organizer_login_codes
     WHERE organizer_email_normalized = ? AND used_at IS NULL
     ORDER BY created_at DESC
     LIMIT 1",
    params = list(organizer_email)
  )
  if (nrow(codes) == 0) {
    return(FALSE)
  }

  row <- codes[1, , drop = FALSE]
  if (parse_utc_timestamp(row$expires_at[[1]]) < parse_utc_timestamp(now) ||
      as.integer(row$attempts[[1]]) >= magic_code_max_attempts()) {
    return(FALSE)
  }

  supplied_hash <- hash_magic_code(organizer_email, code)
  if (!identical(supplied_hash, row$code_hash[[1]])) {
    DBI::dbExecute(
      conn,
      "UPDATE organizer_login_codes SET attempts = attempts + 1 WHERE login_code_id = ?",
      params = list(row$login_code_id[[1]])
    )
    return(FALSE)
  }

  DBI::dbExecute(
    conn,
    "UPDATE organizer_login_codes SET used_at = ? WHERE login_code_id = ?",
    params = list(now, row$login_code_id[[1]])
  )
  TRUE
}

create_participant_login_code <- function(conn, poll_id, participant_email, code = generate_magic_code()) {
  if (is_mongo_connection(conn)) {
    return(mongo_create_participant_login_code(conn, poll_id, participant_email, code))
  }
  poll_id <- suppressWarnings(as.integer(poll_id))
  if (is.na(poll_id)) {
    stop("Poll is invalid.", call. = FALSE)
  }
  participant_email <- validate_email(participant_email, field = "Participant email")
  code <- validate_magic_code(code)
  created_at <- db_now()
  expires_at <- as_utc_string(add_minutes(parse_utc_timestamp(created_at), magic_code_expires_minutes()))
  code_hash <- hash_magic_code(participant_email, code)

  DBI::dbExecute(
    conn,
    "INSERT INTO participant_login_codes (
      poll_id, participant_email_normalized, code_hash, created_at, expires_at, used_at, attempts
    ) VALUES (?, ?, ?, ?, ?, NULL, 0)",
    params = list(poll_id, participant_email, code_hash, created_at, expires_at)
  )

  list(poll_id = poll_id, email = participant_email, code = code, expires_at = expires_at)
}

verify_participant_login_code <- function(conn, poll_id, participant_email, code) {
  if (is_mongo_connection(conn)) {
    return(mongo_verify_participant_login_code(conn, poll_id, participant_email, code))
  }
  poll_id <- suppressWarnings(as.integer(poll_id))
  if (is.na(poll_id)) {
    stop("Poll is invalid.", call. = FALSE)
  }
  participant_email <- validate_email(participant_email, field = "Participant email")
  code <- validate_magic_code(code)
  now <- db_now()

  codes <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM participant_login_codes
     WHERE poll_id = ? AND participant_email_normalized = ? AND used_at IS NULL
     ORDER BY created_at DESC
     LIMIT 1",
    params = list(poll_id, participant_email)
  )
  if (nrow(codes) == 0) {
    return(FALSE)
  }

  row <- codes[1, , drop = FALSE]
  if (parse_utc_timestamp(row$expires_at[[1]]) < parse_utc_timestamp(now) ||
      as.integer(row$attempts[[1]]) >= magic_code_max_attempts()) {
    return(FALSE)
  }

  supplied_hash <- hash_magic_code(participant_email, code)
  if (!identical(supplied_hash, row$code_hash[[1]])) {
    DBI::dbExecute(
      conn,
      "UPDATE participant_login_codes SET attempts = attempts + 1 WHERE participant_login_code_id = ?",
      params = list(row$participant_login_code_id[[1]])
    )
    return(FALSE)
  }

  DBI::dbExecute(
    conn,
    "UPDATE participant_login_codes SET used_at = ? WHERE participant_login_code_id = ?",
    params = list(now, row$participant_login_code_id[[1]])
  )
  TRUE
}

get_participant_by_email <- function(conn, poll_id, participant_email) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_participant_by_email(conn, poll_id, participant_email))
  }
  poll_id <- suppressWarnings(as.integer(poll_id))
  participant_email <- validate_email(participant_email, field = "Participant email")
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM participants
     WHERE poll_id = ? AND email = ?
     LIMIT 1",
    params = list(poll_id, participant_email)
  )
  if (nrow(result) == 0) NULL else result
}

get_participant_response_values <- function(conn, participant_id) {
  if (is_mongo_connection(conn)) {
    return(mongo_get_participant_response_values(conn, participant_id))
  }
  participant_id <- suppressWarnings(as.integer(participant_id))
  if (is.na(participant_id)) {
    return(data.frame(option_id = integer(), availability = character(), stringsAsFactors = FALSE))
  }
  DBI::dbGetQuery(
    conn,
    "SELECT option_id, availability
     FROM responses
     WHERE participant_id = ?
     ORDER BY option_id",
    params = list(participant_id)
  )
}

get_participant_visible_poll_data <- function(conn, poll_id) {
  participants <- get_participants(conn, poll_id)
  responses <- get_responses_for_poll(conn, poll_id)

  if (nrow(participants) == 0) {
    participants <- data.frame(
      participant_id = integer(),
      name = character(),
      submitted_at = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    )
  } else {
    participants$email <- participants$email %||% character(nrow(participants))
    verified <- !is.na(participants$email) & nzchar(trimws(participants$email))
    participants <- participants[verified, , drop = FALSE]
    participants <- participants[, intersect(c("participant_id", "name", "submitted_at", "updated_at"), names(participants)), drop = FALSE]
  }

  if (nrow(responses) == 0 || nrow(participants) == 0) {
    responses <- data.frame(participant_id = integer(), option_id = integer(), availability = character(), stringsAsFactors = FALSE)
  } else {
    responses <- responses[responses$participant_id %in% participants$participant_id, , drop = FALSE]
    responses <- responses[, intersect(c("participant_id", "option_id", "availability"), names(responses)), drop = FALSE]
  }

  list(participants = participants, responses = responses)
}
