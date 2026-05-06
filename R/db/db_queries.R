db_now <- function() {
  utc_timestamp()
}

db_scalar_id <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[[1]]
}

audit_event <- function(conn, poll_id, event_type, event_detail = "") {
  DBI::dbExecute(
    conn,
    "INSERT INTO audit_log (poll_id, event_type, event_detail, created_at)
     VALUES (?, ?, ?, ?)",
    params = list(poll_id, event_type, sanitize_text(event_detail, max_chars = 300), db_now())
  )
}

create_poll_record <- function(conn, poll, options, expected = data.frame()) {
  admin_token <- generate_token()
  response_token <- generate_token()
  created_at <- db_now()

  result <- with_db_transaction(conn, function(tx) {
    DBI::dbExecute(
      tx,
      "INSERT INTO polls (
        admin_token_hash, response_token, title, description, organizer_name, organizer_email,
        duration_minutes, timezone, location_type, location_details, response_deadline,
        status, created_at, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'open', ?, ?)",
      params = list(
        hash_token(admin_token),
        response_token,
        poll$title,
        poll$description,
        poll$organizer_name,
        poll$organizer_email,
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
  response_token <- validate_token(response_token, field = "Response link token")
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM polls WHERE response_token = ? LIMIT 1",
    params = list(response_token)
  )
  if (nrow(result) == 0) NULL else result
}

get_poll_by_admin_token <- function(conn, admin_token) {
  admin_token <- validate_token(admin_token, field = "Admin link token")
  result <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM polls WHERE admin_token_hash = ? LIMIT 1",
    params = list(hash_token(admin_token))
  )
  if (nrow(result) == 0) NULL else result
}

get_poll_options <- function(conn, poll_id) {
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM poll_options WHERE poll_id = ? ORDER BY option_order, start_datetime",
    params = list(poll_id)
  )
}

get_expected_participants <- function(conn, poll_id) {
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM expected_participants WHERE poll_id = ? ORDER BY name, email",
    params = list(poll_id)
  )
}

get_participants <- function(conn, poll_id) {
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM participants WHERE poll_id = ? ORDER BY submitted_at, name",
    params = list(poll_id)
  )
}

get_responses_for_poll <- function(conn, poll_id) {
  DBI::dbGetQuery(
    conn,
    "SELECT
       r.response_id, r.participant_id, r.option_id, r.availability, r.comment,
       p.name, p.email, p.organization,
       o.display_label, o.option_order
     FROM responses r
     INNER JOIN participants p ON p.participant_id = r.participant_id
     INNER JOIN poll_options o ON o.option_id = r.option_id
     WHERE p.poll_id = ?
     ORDER BY p.name, o.option_order",
    params = list(poll_id)
  )
}

get_finalized_meeting <- function(conn, poll_id) {
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
  poll <- DBI::dbGetQuery(conn, "SELECT * FROM polls WHERE poll_id = ? LIMIT 1", params = list(poll_id))
  if (nrow(poll) == 0) {
    return(NULL)
  }
  options <- get_poll_options(conn, poll_id)
  expected <- get_expected_participants(conn, poll_id)
  participants <- get_participants(conn, poll_id)
  responses <- get_responses_for_poll(conn, poll_id)
  finalized <- get_finalized_meeting(conn, poll_id)
  ranked <- rank_time_options(options, responses, participants, expected)
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
    heatmap = build_availability_matrix(options, responses, participants)
  )
}

submit_poll_response <- function(conn, poll_id, participant, response_values, comment = "") {
  submitted_at <- db_now()
  participant$email <- validate_email(participant$email, field = "Participant email")

  with_db_transaction(conn, function(tx) {
    existing <- DBI::dbGetQuery(
      tx,
      "SELECT participant_id FROM participants WHERE poll_id = ? AND email = ? LIMIT 1",
      params = list(poll_id, participant$email)
    )

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
