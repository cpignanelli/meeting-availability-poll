mongo_collections <- function() {
  c(
    "polls",
    "poll_options",
    "expected_participants",
    "participants",
    "responses",
    "finalized_meetings",
    "audit_log",
    "organizer_login_codes",
    "participant_login_codes",
    "owner_access_requests",
    "approved_owners",
    "counters"
  )
}

get_mongo_connection <- function(
  uri = Sys.getenv("MONGODB_URI", unset = ""),
  database = Sys.getenv("MONGODB_DATABASE", unset = "")
) {
  if (!requireNamespace("mongolite", quietly = TRUE)) {
    stop("Package mongolite is required when DATABASE_BACKEND=mongodb.", call. = FALSE)
  }
  uri <- trimws(uri)
  database <- trimws(database)
  if (!nzchar(uri)) {
    stop("MONGODB_URI is required when DATABASE_BACKEND=mongodb.", call. = FALSE)
  }
  if (!nzchar(database)) {
    stop("MONGODB_DATABASE is required when DATABASE_BACKEND=mongodb.", call. = FALSE)
  }

  conn <- structure(
    list(
      backend = "mongodb",
      uri = uri,
      database = database,
      collections = new.env(parent = emptyenv())
    ),
    class = "meeting_poll_mongo"
  )
  initialize_mongo_database(conn)
  conn
}

is_mongo_connection <- function(conn) {
  inherits(conn, "meeting_poll_mongo")
}

mongo_collection <- function(conn, collection) {
  if (!is_mongo_connection(conn)) {
    stop("MongoDB connection object is invalid.", call. = FALSE)
  }
  if (!collection %in% mongo_collections()) {
    stop("MongoDB collection name is invalid.", call. = FALSE)
  }
  if (!exists(collection, envir = conn$collections, inherits = FALSE)) {
    assign(
      collection,
      mongolite::mongo(collection = collection, db = conn$database, url = conn$uri),
      envir = conn$collections
    )
  }
  get(collection, envir = conn$collections, inherits = FALSE)
}

close_mongo_connection <- function(conn) {
  if (!is_mongo_connection(conn)) {
    return(invisible(TRUE))
  }
  for (collection in ls(conn$collections)) {
    get(collection, envir = conn$collections, inherits = FALSE)$disconnect()
  }
  invisible(TRUE)
}

mongo_json <- function(value = list()) {
  if (is.list(value) && length(value) == 0) {
    return("{}")
  }
  jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", POSIXt = "ISO8601")
}

mongo_empty_frame <- function(columns, integer_columns = character()) {
  values <- lapply(columns, function(column) {
    if (column %in% integer_columns) integer() else character()
  })
  names(values) <- columns
  data.frame(values, stringsAsFactors = FALSE)
}

mongo_normalize_frame <- function(df, columns, integer_columns = character(), logical_columns = character()) {
  if (is.null(df) || nrow(df) == 0) {
    return(mongo_empty_frame(columns, integer_columns))
  }
  if ("_id" %in% names(df)) {
    df[["_id"]] <- NULL
  }
  for (column in columns) {
    if (!column %in% names(df)) {
      df[[column]] <- if (column %in% integer_columns) NA_integer_ else NA_character_
    }
  }
  df <- df[, columns, drop = FALSE]
  for (column in integer_columns) {
    df[[column]] <- suppressWarnings(as.integer(df[[column]]))
  }
  for (column in logical_columns) {
    df[[column]] <- as.logical(df[[column]])
  }
  for (column in setdiff(columns, c(integer_columns, logical_columns))) {
    df[[column]] <- as.character(df[[column]])
  }
  rownames(df) <- NULL
  df
}

mongo_poll_columns <- function() {
  c(
    "poll_id", "admin_token_hash", "response_token", "title", "description",
    "organizer_name", "organizer_email", "organizer_email_normalized",
    "duration_minutes", "timezone", "location_type", "location_details",
    "response_deadline", "status", "created_at", "updated_at", "closed_at"
  )
}

mongo_poll_option_columns <- function() {
  c("option_id", "poll_id", "start_datetime", "end_datetime", "display_label", "option_order")
}

mongo_expected_columns <- function() {
  c("expected_participant_id", "poll_id", "name", "email", "organization", "is_required")
}

mongo_participant_columns <- function() {
  c("participant_id", "poll_id", "name", "email", "organization", "submitted_at", "updated_at")
}

mongo_response_columns <- function() {
  c("response_id", "participant_id", "option_id", "availability", "comment")
}

mongo_response_join_columns <- function() {
  c(
    "response_id", "participant_id", "option_id", "availability", "comment",
    "name", "email", "organization", "display_label", "start_datetime",
    "end_datetime", "option_order"
  )
}

mongo_finalized_columns <- function(include_option = FALSE) {
  columns <- c("final_id", "poll_id", "selected_option_id", "final_notes", "finalized_at")
  if (isTRUE(include_option)) {
    columns <- c(columns, "display_label", "start_datetime", "end_datetime")
  }
  columns
}

mongo_owner_request_columns <- function() {
  c(
    "request_id", "first_name", "last_name", "email", "email_normalized",
    "status", "requested_at", "verified_at", "reviewed_at", "reviewed_by_email",
    "updated_at"
  )
}

mongo_approved_owner_columns <- function() {
  c(
    "owner_id", "first_name", "last_name", "email", "email_normalized",
    "status", "approved_at", "approved_by_email", "revoked_at", "updated_at"
  )
}

mongo_login_code_columns <- function() {
  c(
    "login_code_id", "organizer_email_normalized", "code_hash", "created_at",
    "expires_at", "used_at", "attempts"
  )
}

mongo_participant_login_code_columns <- function() {
  c(
    "participant_login_code_id", "poll_id", "participant_email_normalized",
    "code_hash", "created_at", "expires_at", "used_at", "attempts"
  )
}

mongo_find <- function(conn, collection, query = list(), sort = list(), limit = 0) {
  mongo_collection(conn, collection)$find(
    query = mongo_json(query),
    fields = "{\"_id\":0}",
    sort = mongo_json(sort),
    limit = limit
  )
}

mongo_find_one <- function(conn, collection, query = list(), sort = list(), columns, integer_columns = character()) {
  result <- mongo_find(conn, collection, query = query, sort = sort, limit = 1)
  result <- mongo_normalize_frame(result, columns = columns, integer_columns = integer_columns)
  if (nrow(result) == 0) NULL else result[1, , drop = FALSE]
}

mongo_insert_one <- function(conn, collection, document) {
  mongo_collection(conn, collection)$insert(document, auto_unbox = TRUE, null = "null")
}

mongo_update_one <- function(conn, collection, query, set_fields) {
  mongo_collection(conn, collection)$update(
    query = mongo_json(query),
    update = mongo_json(list("$set" = set_fields)),
    upsert = FALSE,
    multiple = FALSE
  )
}

mongo_remove_many <- function(conn, collection, query) {
  mongo_collection(conn, collection)$remove(query = mongo_json(query), just_one = FALSE)
}

mongo_next_id <- function(conn, counter_name) {
  counter_name <- sanitize_text(counter_name, max_chars = 80, required = TRUE, field = "Counter name")
  result <- mongo_collection(conn, "counters")$run(mongo_json(list(
    findAndModify = "counters",
    query = list(counter_name = counter_name),
    update = list("$inc" = list(seq = 1L)),
    upsert = TRUE,
    new = TRUE
  )))
  value <- result$value
  seq_value <- if (is.data.frame(value)) value$seq[[1]] else value$seq
  as.integer(seq_value)
}

mongo_index <- function(conn, collection, fields) {
  key <- stats::setNames(as.list(rep(1L, length(fields))), fields)
  mongo_collection(conn, collection)$index(add = mongo_json(key))
}

initialize_mongo_database <- function(conn) {
  indexes <- list(
    polls = list("response_token", "admin_token_hash", "organizer_email_normalized"),
    poll_options = list("poll_id", c("poll_id", "option_order"), "option_id"),
    expected_participants = list("poll_id"),
    participants = list("poll_id", c("poll_id", "email"), "participant_id"),
    responses = list("participant_id", "option_id"),
    finalized_meetings = list("poll_id"),
    audit_log = list("poll_id"),
    organizer_login_codes = list(c("organizer_email_normalized", "created_at")),
    participant_login_codes = list(c("poll_id", "participant_email_normalized", "created_at")),
    owner_access_requests = list("email_normalized", c("status", "requested_at")),
    approved_owners = list("email_normalized", c("status", "approved_at"))
  )
  for (collection in names(indexes)) {
    for (fields in indexes[[collection]]) {
      mongo_index(conn, collection, fields)
    }
  }
  invisible(TRUE)
}

mongo_audit_event <- function(conn, poll_id, event_type, event_detail = "") {
  mongo_insert_one(conn, "audit_log", list(
    audit_id = mongo_next_id(conn, "audit_id"),
    poll_id = as.integer(poll_id),
    event_type = event_type,
    event_detail = sanitize_text(event_detail, max_chars = 300),
    created_at = db_now()
  ))
}

mongo_create_poll_record <- function(conn, poll, options, expected = data.frame()) {
  admin_token <- generate_token()
  response_token <- generate_token()
  created_at <- db_now()
  poll_id <- mongo_next_id(conn, "poll_id")

  mongo_insert_one(conn, "polls", list(
    poll_id = poll_id,
    admin_token_hash = hash_token(admin_token),
    response_token = response_token,
    title = poll$title,
    description = poll$description,
    organizer_name = poll$organizer_name,
    organizer_email = poll$organizer_email,
    organizer_email_normalized = normalize_email(poll$organizer_email),
    duration_minutes = as.integer(poll$duration_minutes),
    timezone = poll$timezone,
    location_type = poll$location_type,
    location_details = poll$location_details,
    response_deadline = poll$response_deadline,
    status = "open",
    created_at = created_at,
    updated_at = created_at,
    closed_at = ""
  ))

  if (nrow(options) > 0) {
    for (i in seq_len(nrow(options))) {
      mongo_insert_one(conn, "poll_options", list(
        option_id = mongo_next_id(conn, "option_id"),
        poll_id = poll_id,
        start_datetime = options$start_datetime[[i]],
        end_datetime = options$end_datetime[[i]],
        display_label = options$display_label[[i]],
        option_order = as.integer(options$option_order[[i]])
      ))
    }
  }

  if (nrow(expected) > 0) {
    for (i in seq_len(nrow(expected))) {
      mongo_insert_one(conn, "expected_participants", list(
        expected_participant_id = mongo_next_id(conn, "expected_participant_id"),
        poll_id = poll_id,
        name = expected$name[[i]],
        email = expected$email[[i]],
        organization = expected$organization[[i]],
        is_required = as.integer(expected$is_required[[i]])
      ))
    }
  }

  mongo_audit_event(conn, poll_id, "poll_created", "Poll created")
  list(poll_id = poll_id, admin_token = admin_token, response_token = response_token)
}

mongo_get_poll_by_response_token <- function(conn, response_token) {
  response_token <- validate_token(response_token, field = "Response link token")
  mongo_find_one(
    conn,
    "polls",
    query = list(response_token = response_token),
    columns = mongo_poll_columns(),
    integer_columns = c("poll_id", "duration_minutes")
  )
}

mongo_get_poll_by_admin_token <- function(conn, admin_token) {
  admin_token <- validate_token(admin_token, field = "Admin link token")
  mongo_find_one(
    conn,
    "polls",
    query = list(admin_token_hash = hash_token(admin_token)),
    columns = mongo_poll_columns(),
    integer_columns = c("poll_id", "duration_minutes")
  )
}

mongo_get_poll_for_organizer <- function(conn, poll_id, organizer_email) {
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  poll_id <- suppressWarnings(as.integer(poll_id))
  if (is.na(poll_id)) {
    return(NULL)
  }
  mongo_find_one(
    conn,
    "polls",
    query = list(poll_id = poll_id, organizer_email_normalized = organizer_email),
    columns = mongo_poll_columns(),
    integer_columns = c("poll_id", "duration_minutes")
  )
}

mongo_list_polls_for_organizer <- function(conn, organizer_email) {
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  polls <- mongo_find(
    conn,
    "polls",
    query = list(organizer_email_normalized = organizer_email),
    sort = list(updated_at = -1L, created_at = -1L)
  )
  polls <- mongo_normalize_frame(polls, mongo_poll_columns(), c("poll_id", "duration_minutes"))
  if (nrow(polls) == 0) {
    polls$option_count <- integer()
    polls$response_count <- integer()
    return(polls)
  }
  polls$option_count <- vapply(polls$poll_id, function(poll_id) {
    as.integer(mongo_collection(conn, "poll_options")$count(mongo_json(list(poll_id = as.integer(poll_id)))))
  }, integer(1))
  polls$response_count <- vapply(polls$poll_id, function(poll_id) {
    as.integer(mongo_collection(conn, "participants")$count(mongo_json(list(poll_id = as.integer(poll_id)))))
  }, integer(1))
  polls[order(polls$updated_at, polls$created_at, decreasing = TRUE), , drop = FALSE]
}

mongo_get_poll_options <- function(conn, poll_id) {
  options <- mongo_find(
    conn,
    "poll_options",
    query = list(poll_id = as.integer(poll_id)),
    sort = list(option_order = 1L, start_datetime = 1L)
  )
  mongo_normalize_frame(options, mongo_poll_option_columns(), c("option_id", "poll_id", "option_order"))
}

mongo_get_expected_participants <- function(conn, poll_id) {
  expected <- mongo_find(
    conn,
    "expected_participants",
    query = list(poll_id = as.integer(poll_id)),
    sort = list(name = 1L, email = 1L)
  )
  mongo_normalize_frame(expected, mongo_expected_columns(), c("expected_participant_id", "poll_id", "is_required"))
}

mongo_get_participants <- function(conn, poll_id) {
  participants <- mongo_find(
    conn,
    "participants",
    query = list(poll_id = as.integer(poll_id)),
    sort = list(submitted_at = 1L, name = 1L)
  )
  mongo_normalize_frame(participants, mongo_participant_columns(), c("participant_id", "poll_id"))
}

mongo_get_responses_for_poll <- function(conn, poll_id) {
  participants <- mongo_get_participants(conn, poll_id)
  options <- mongo_get_poll_options(conn, poll_id)
  if (nrow(participants) == 0 || nrow(options) == 0) {
    return(mongo_empty_frame(
      mongo_response_join_columns(),
      c("response_id", "participant_id", "option_id", "option_order")
    ))
  }
  responses <- mongo_find(
    conn,
    "responses",
    query = list(participant_id = list("$in" = unname(as.list(participants$participant_id))))
  )
  responses <- mongo_normalize_frame(responses, mongo_response_columns(), c("response_id", "participant_id", "option_id"))
  if (nrow(responses) == 0) {
    return(mongo_empty_frame(
      mongo_response_join_columns(),
      c("response_id", "participant_id", "option_id", "option_order")
    ))
  }
  joined <- merge(responses, participants, by = "participant_id", all.x = TRUE, sort = FALSE)
  joined <- merge(joined, options, by = "option_id", all.x = TRUE, sort = FALSE)
  joined <- joined[order(joined$name, joined$option_order), , drop = FALSE]
  joined <- joined[, mongo_response_join_columns(), drop = FALSE]
  mongo_normalize_frame(joined, mongo_response_join_columns(), c("response_id", "participant_id", "option_id", "option_order"))
}

mongo_get_finalized_meeting <- function(conn, poll_id) {
  finalized <- mongo_find_one(
    conn,
    "finalized_meetings",
    query = list(poll_id = as.integer(poll_id)),
    columns = mongo_finalized_columns(),
    integer_columns = c("final_id", "poll_id", "selected_option_id")
  )
  if (is.null(finalized)) {
    return(NULL)
  }
  option <- mongo_find_one(
    conn,
    "poll_options",
    query = list(option_id = finalized$selected_option_id[[1]]),
    columns = mongo_poll_option_columns(),
    integer_columns = c("option_id", "poll_id", "option_order")
  )
  finalized$display_label <- if (is.null(option)) "" else option$display_label[[1]] %||% ""
  finalized$start_datetime <- if (is.null(option)) "" else option$start_datetime[[1]] %||% ""
  finalized$end_datetime <- if (is.null(option)) "" else option$end_datetime[[1]] %||% ""
  mongo_normalize_frame(
    finalized,
    mongo_finalized_columns(include_option = TRUE),
    c("final_id", "poll_id", "selected_option_id")
  )
}

mongo_get_poll_dashboard_data <- function(conn, poll_id) {
  poll <- mongo_find_one(
    conn,
    "polls",
    query = list(poll_id = as.integer(poll_id)),
    columns = mongo_poll_columns(),
    integer_columns = c("poll_id", "duration_minutes")
  )
  if (is.null(poll)) {
    return(NULL)
  }
  options <- mongo_get_poll_options(conn, poll_id)
  expected <- mongo_get_expected_participants(conn, poll_id)
  participants <- mongo_get_participants(conn, poll_id)
  responses <- mongo_get_responses_for_poll(conn, poll_id)
  finalized <- mongo_get_finalized_meeting(conn, poll_id)
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

mongo_submit_poll_response <- function(conn, poll_id, participant, response_values, comment = "") {
  submitted_at <- db_now()
  participant$email <- validate_optional_email(participant$email, field = "Participant email")
  participant$organization <- sanitize_text(participant$organization %||% "", max_chars = 160, required = FALSE, field = "Organization")
  has_email <- !is.na(participant$email) && nzchar(participant$email)
  poll_id <- as.integer(poll_id)

  existing <- if (has_email) {
    mongo_find_one(
      conn,
      "participants",
      query = list(poll_id = poll_id, email = participant$email),
      columns = mongo_participant_columns(),
      integer_columns = c("participant_id", "poll_id")
    )
  } else {
    NULL
  }

  if (is.null(existing)) {
    participant_id <- mongo_next_id(conn, "participant_id")
    mongo_insert_one(conn, "participants", list(
      participant_id = participant_id,
      poll_id = poll_id,
      name = participant$name,
      email = participant$email,
      organization = participant$organization,
      submitted_at = submitted_at,
      updated_at = submitted_at
    ))
  } else {
    participant_id <- existing$participant_id[[1]]
    mongo_update_one(conn, "participants", list(participant_id = participant_id), list(
      name = participant$name,
      organization = participant$organization,
      updated_at = submitted_at
    ))
    mongo_remove_many(conn, "responses", list(participant_id = participant_id))
  }

  for (i in seq_len(nrow(response_values))) {
    mongo_insert_one(conn, "responses", list(
      response_id = mongo_next_id(conn, "response_id"),
      participant_id = participant_id,
      option_id = as.integer(response_values$option_id[[i]]),
      availability = response_values$availability[[i]],
      comment = comment
    ))
  }

  mongo_audit_event(conn, poll_id, "response_submitted", "Participant response submitted")
  participant_id
}

mongo_get_participant_by_email <- function(conn, poll_id, participant_email) {
  participant_email <- validate_email(participant_email, field = "Participant email")
  poll_id <- suppressWarnings(as.integer(poll_id))
  if (is.na(poll_id)) {
    return(NULL)
  }
  mongo_find_one(
    conn,
    "participants",
    query = list(poll_id = poll_id, email = participant_email),
    columns = mongo_participant_columns(),
    integer_columns = c("participant_id", "poll_id")
  )
}

mongo_get_participant_response_values <- function(conn, participant_id) {
  participant_id <- suppressWarnings(as.integer(participant_id))
  if (is.na(participant_id)) {
    return(mongo_empty_frame(c("option_id", "availability"), "option_id"))
  }
  responses <- mongo_find(
    conn,
    "responses",
    query = list(participant_id = participant_id),
    sort = list(option_id = 1L)
  )
  responses <- mongo_normalize_frame(responses, mongo_response_columns(), c("response_id", "participant_id", "option_id"))
  if (nrow(responses) == 0) {
    return(mongo_empty_frame(c("option_id", "availability"), "option_id"))
  }
  responses[, c("option_id", "availability"), drop = FALSE]
}

mongo_create_participant_login_code <- function(conn, poll_id, participant_email, code = generate_magic_code()) {
  poll_id <- suppressWarnings(as.integer(poll_id))
  if (is.na(poll_id)) {
    stop("Poll is invalid.", call. = FALSE)
  }
  participant_email <- validate_email(participant_email, field = "Participant email")
  code <- validate_magic_code(code)
  created_at <- db_now()
  expires_at <- as_utc_string(add_minutes(parse_utc_timestamp(created_at), magic_code_expires_minutes()))
  mongo_insert_one(conn, "participant_login_codes", list(
    participant_login_code_id = mongo_next_id(conn, "participant_login_code_id"),
    poll_id = poll_id,
    participant_email_normalized = participant_email,
    code_hash = hash_magic_code(participant_email, code),
    created_at = created_at,
    expires_at = expires_at,
    used_at = "",
    attempts = 0L
  ))
  list(poll_id = poll_id, email = participant_email, code = code, expires_at = expires_at)
}

mongo_verify_participant_login_code <- function(conn, poll_id, participant_email, code) {
  poll_id <- suppressWarnings(as.integer(poll_id))
  if (is.na(poll_id)) {
    stop("Poll is invalid.", call. = FALSE)
  }
  participant_email <- validate_email(participant_email, field = "Participant email")
  code <- validate_magic_code(code)
  now <- db_now()
  codes <- mongo_find(
    conn,
    "participant_login_codes",
    query = list(poll_id = poll_id, participant_email_normalized = participant_email, used_at = ""),
    sort = list(created_at = -1L),
    limit = 1
  )
  codes <- mongo_normalize_frame(
    codes,
    mongo_participant_login_code_columns(),
    c("participant_login_code_id", "poll_id", "attempts")
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
    mongo_update_one(
      conn,
      "participant_login_codes",
      list(participant_login_code_id = row$participant_login_code_id[[1]]),
      list(attempts = as.integer(row$attempts[[1]]) + 1L)
    )
    return(FALSE)
  }
  mongo_update_one(
    conn,
    "participant_login_codes",
    list(participant_login_code_id = row$participant_login_code_id[[1]]),
    list(used_at = now)
  )
  TRUE
}

mongo_finalize_meeting <- function(conn, poll_id, selected_option_id, final_notes = "") {
  finalized_at <- db_now()
  final_notes <- sanitize_text(final_notes, max_chars = 2000)
  poll_id <- as.integer(poll_id)
  selected_option_id <- as.integer(selected_option_id)
  existing <- mongo_find_one(
    conn,
    "finalized_meetings",
    query = list(poll_id = poll_id),
    columns = mongo_finalized_columns(),
    integer_columns = c("final_id", "poll_id", "selected_option_id")
  )
  if (is.null(existing)) {
    mongo_insert_one(conn, "finalized_meetings", list(
      final_id = mongo_next_id(conn, "final_id"),
      poll_id = poll_id,
      selected_option_id = selected_option_id,
      final_notes = final_notes,
      finalized_at = finalized_at
    ))
  } else {
    mongo_update_one(conn, "finalized_meetings", list(final_id = existing$final_id[[1]]), list(
      selected_option_id = selected_option_id,
      final_notes = final_notes,
      finalized_at = finalized_at
    ))
  }

  poll <- mongo_find_one(
    conn,
    "polls",
    query = list(poll_id = poll_id),
    columns = mongo_poll_columns(),
    integer_columns = c("poll_id", "duration_minutes")
  )
  if (is.null(poll)) {
    stop("Poll not found.", call. = FALSE)
  }
  closed_at <- poll$closed_at[[1]]
  if (is.na(closed_at) || !nzchar(closed_at)) {
    closed_at <- finalized_at
  }
  mongo_update_one(conn, "polls", list(poll_id = poll_id), list(
    status = "finalized",
    updated_at = finalized_at,
    closed_at = closed_at
  ))
  mongo_audit_event(conn, poll_id, "poll_finalized", "Meeting finalized")
  invisible(TRUE)
}

mongo_close_poll <- function(conn, poll_id) {
  closed_at <- db_now()
  poll_id <- as.integer(poll_id)
  poll <- mongo_find_one(
    conn,
    "polls",
    query = list(poll_id = poll_id),
    columns = mongo_poll_columns(),
    integer_columns = c("poll_id", "duration_minutes")
  )
  if (is.null(poll)) {
    stop("Poll not found.", call. = FALSE)
  }
  if (!identical(poll$status[[1]], "finalized")) {
    next_closed_at <- poll$closed_at[[1]]
    if (is.na(next_closed_at) || !nzchar(next_closed_at)) {
      next_closed_at <- closed_at
    }
    mongo_update_one(conn, "polls", list(poll_id = poll_id), list(
      status = "closed",
      updated_at = closed_at,
      closed_at = next_closed_at
    ))
  }
  mongo_audit_event(conn, poll_id, "poll_closed", "Poll closed")
  invisible(TRUE)
}

mongo_reopen_poll <- function(conn, poll_id, response_deadline = "") {
  response_deadline <- sanitize_text(response_deadline, max_chars = 40)
  reopened_at <- db_now()
  poll_id <- as.integer(poll_id)
  poll <- mongo_find_one(
    conn,
    "polls",
    query = list(poll_id = poll_id),
    columns = mongo_poll_columns(),
    integer_columns = c("poll_id", "duration_minutes")
  )
  if (is.null(poll)) {
    stop("Poll not found.", call. = FALSE)
  }
  if (identical(poll$status[[1]], "finalized")) {
    stop("Finalized polls cannot be reopened.", call. = FALSE)
  }
  mongo_update_one(conn, "polls", list(poll_id = poll_id), list(
    status = "open",
    response_deadline = response_deadline,
    updated_at = reopened_at,
    closed_at = ""
  ))
  mongo_audit_event(conn, poll_id, "poll_reopened", "Poll response link reopened")
  invisible(TRUE)
}

mongo_get_owner_role <- function(conn, organizer_email) {
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  if (is_main_owner_email(organizer_email)) {
    return("main_owner")
  }
  owner <- mongo_find_one(
    conn,
    "approved_owners",
    query = list(email_normalized = organizer_email),
    columns = mongo_approved_owner_columns(),
    integer_columns = "owner_id"
  )
  if (!is.null(owner)) {
    if (identical(owner$status[[1]], "approved")) return("owner")
    if (identical(owner$status[[1]], "revoked")) return("revoked")
  }
  request <- mongo_find_one(
    conn,
    "owner_access_requests",
    query = list(email_normalized = organizer_email),
    columns = mongo_owner_request_columns(),
    integer_columns = "request_id"
  )
  if (!is.null(request) && request$status[[1]] %in% c("pending", "denied")) {
    return(request$status[[1]])
  }
  "none"
}

mongo_create_or_update_owner_access_request <- function(conn, first_name, last_name, email) {
  profile <- validate_owner_profile(first_name, last_name, email)
  if (is_main_owner_email(profile$email)) {
    stop("The main owner can sign in directly.", call. = FALSE)
  }
  requested_at <- db_now()

  owner <- mongo_find_one(
    conn,
    "approved_owners",
    query = list(email_normalized = profile$email_normalized),
    columns = mongo_approved_owner_columns(),
    integer_columns = "owner_id"
  )
  if (!is.null(owner) && identical(owner$status[[1]], "approved")) {
    stop("This email already has organizer access. Sign in instead.", call. = FALSE)
  }

  existing <- mongo_find_one(
    conn,
    "owner_access_requests",
    query = list(email_normalized = profile$email_normalized),
    columns = mongo_owner_request_columns(),
    integer_columns = "request_id"
  )
  if (is.null(existing)) {
    request_id <- mongo_next_id(conn, "request_id")
    mongo_insert_one(conn, "owner_access_requests", list(
      request_id = request_id,
      first_name = profile$first_name,
      last_name = profile$last_name,
      email = profile$email,
      email_normalized = profile$email_normalized,
      status = "pending",
      requested_at = requested_at,
      verified_at = requested_at,
      reviewed_at = "",
      reviewed_by_email = "",
      updated_at = requested_at
    ))
  } else {
    request_id <- existing$request_id[[1]]
    mongo_update_one(conn, "owner_access_requests", list(request_id = request_id), list(
      first_name = profile$first_name,
      last_name = profile$last_name,
      email = profile$email,
      status = "pending",
      requested_at = requested_at,
      verified_at = requested_at,
      reviewed_at = "",
      reviewed_by_email = "",
      updated_at = requested_at
    ))
  }
  mongo_find_one(
    conn,
    "owner_access_requests",
    query = list(request_id = request_id),
    columns = mongo_owner_request_columns(),
    integer_columns = "request_id"
  )
}

mongo_list_owner_access_requests <- function(conn, reviewer_email, status = "pending") {
  require_main_owner(reviewer_email)
  status <- sanitize_text(status, max_chars = 20, required = TRUE, field = "Request status")
  if (!status %in% c("pending", "approved", "denied")) {
    stop("Request status is invalid.", call. = FALSE)
  }
  requests <- mongo_find(
    conn,
    "owner_access_requests",
    query = list(status = status),
    sort = list(requested_at = 1L, updated_at = 1L)
  )
  mongo_normalize_frame(requests, mongo_owner_request_columns(), "request_id")
}

mongo_list_approved_owners <- function(conn, reviewer_email, include_revoked = FALSE) {
  require_main_owner(reviewer_email)
  query <- if (isTRUE(include_revoked)) list() else list(status = "approved")
  owners <- mongo_find(conn, "approved_owners", query = query, sort = list(approved_at = -1L, updated_at = -1L))
  mongo_normalize_frame(owners, mongo_approved_owner_columns(), "owner_id")
}

mongo_approve_owner_request <- function(conn, request_id, reviewer_email) {
  require_main_owner(reviewer_email)
  reviewer_email <- validate_email(reviewer_email, field = "Reviewer email")
  request_id <- suppressWarnings(as.integer(request_id))
  if (is.na(request_id)) {
    stop("Access request is invalid.", call. = FALSE)
  }
  reviewed_at <- db_now()
  request <- mongo_find_one(
    conn,
    "owner_access_requests",
    query = list(request_id = request_id),
    columns = mongo_owner_request_columns(),
    integer_columns = "request_id"
  )
  if (is.null(request)) stop("Access request not found.", call. = FALSE)
  if (!identical(request$status[[1]], "pending")) stop("Only pending access requests can be approved.", call. = FALSE)
  if (is_main_owner_email(request$email_normalized[[1]])) stop("The main owner does not need approval.", call. = FALSE)

  existing_owner <- mongo_find_one(
    conn,
    "approved_owners",
    query = list(email_normalized = request$email_normalized[[1]]),
    columns = mongo_approved_owner_columns(),
    integer_columns = "owner_id"
  )
  if (is.null(existing_owner)) {
    mongo_insert_one(conn, "approved_owners", list(
      owner_id = mongo_next_id(conn, "owner_id"),
      first_name = request$first_name[[1]],
      last_name = request$last_name[[1]],
      email = request$email[[1]],
      email_normalized = request$email_normalized[[1]],
      status = "approved",
      approved_at = reviewed_at,
      approved_by_email = reviewer_email,
      revoked_at = "",
      updated_at = reviewed_at
    ))
  } else {
    mongo_update_one(conn, "approved_owners", list(owner_id = existing_owner$owner_id[[1]]), list(
      first_name = request$first_name[[1]],
      last_name = request$last_name[[1]],
      email = request$email[[1]],
      status = "approved",
      approved_at = reviewed_at,
      approved_by_email = reviewer_email,
      revoked_at = "",
      updated_at = reviewed_at
    ))
  }
  mongo_update_one(conn, "owner_access_requests", list(request_id = request_id), list(
    status = "approved",
    reviewed_at = reviewed_at,
    reviewed_by_email = reviewer_email,
    updated_at = reviewed_at
  ))
  invisible(TRUE)
}

mongo_deny_owner_request <- function(conn, request_id, reviewer_email) {
  require_main_owner(reviewer_email)
  reviewer_email <- validate_email(reviewer_email, field = "Reviewer email")
  request_id <- suppressWarnings(as.integer(request_id))
  if (is.na(request_id)) {
    stop("Access request is invalid.", call. = FALSE)
  }
  request <- mongo_find_one(
    conn,
    "owner_access_requests",
    query = list(request_id = request_id, status = "pending"),
    columns = mongo_owner_request_columns(),
    integer_columns = "request_id"
  )
  if (is.null(request)) {
    stop("Only pending access requests can be denied.", call. = FALSE)
  }
  reviewed_at <- db_now()
  mongo_update_one(conn, "owner_access_requests", list(request_id = request_id), list(
    status = "denied",
    reviewed_at = reviewed_at,
    reviewed_by_email = reviewer_email,
    updated_at = reviewed_at
  ))
  invisible(TRUE)
}

mongo_revoke_approved_owner <- function(conn, owner_id, reviewer_email) {
  require_main_owner(reviewer_email)
  validate_email(reviewer_email, field = "Reviewer email")
  owner_id <- suppressWarnings(as.integer(owner_id))
  if (is.na(owner_id)) {
    stop("Approved owner is invalid.", call. = FALSE)
  }
  owner <- mongo_find_one(
    conn,
    "approved_owners",
    query = list(owner_id = owner_id),
    columns = mongo_approved_owner_columns(),
    integer_columns = "owner_id"
  )
  if (is.null(owner) || !identical(owner$status[[1]], "approved")) {
    stop("Only approved owners can be revoked.", call. = FALSE)
  }
  if (is_main_owner_email(owner$email_normalized[[1]])) {
    stop("The main owner cannot be revoked.", call. = FALSE)
  }
  revoked_at <- db_now()
  mongo_update_one(conn, "approved_owners", list(owner_id = owner_id), list(
    status = "revoked",
    revoked_at = revoked_at,
    updated_at = revoked_at
  ))
  invisible(TRUE)
}

mongo_create_organizer_login_code <- function(conn, organizer_email, code = generate_magic_code()) {
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  code <- validate_magic_code(code)
  created_at <- db_now()
  expires_at <- as_utc_string(add_minutes(parse_utc_timestamp(created_at), magic_code_expires_minutes()))
  mongo_insert_one(conn, "organizer_login_codes", list(
    login_code_id = mongo_next_id(conn, "login_code_id"),
    organizer_email_normalized = organizer_email,
    code_hash = hash_magic_code(organizer_email, code),
    created_at = created_at,
    expires_at = expires_at,
    used_at = "",
    attempts = 0L
  ))
  list(email = organizer_email, code = code, expires_at = expires_at)
}

mongo_verify_organizer_login_code <- function(conn, organizer_email, code) {
  organizer_email <- validate_email(organizer_email, field = "Organizer email")
  code <- validate_magic_code(code)
  now <- db_now()
  codes <- mongo_find(
    conn,
    "organizer_login_codes",
    query = list(organizer_email_normalized = organizer_email, used_at = ""),
    sort = list(created_at = -1L),
    limit = 1
  )
  codes <- mongo_normalize_frame(codes, mongo_login_code_columns(), c("login_code_id", "attempts"))
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
    mongo_update_one(conn, "organizer_login_codes", list(login_code_id = row$login_code_id[[1]]), list(
      attempts = as.integer(row$attempts[[1]]) + 1L
    ))
    return(FALSE)
  }
  mongo_update_one(conn, "organizer_login_codes", list(login_code_id = row$login_code_id[[1]]), list(used_at = now))
  TRUE
}
