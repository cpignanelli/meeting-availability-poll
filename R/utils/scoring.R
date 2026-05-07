score_value <- function(availability) {
  availability <- as.character(availability %||% "missing")
  ifelse(availability == "preferred", 2L, ifelse(availability == "available", 1L, 0L))
}

rank_time_options <- function(options, responses, participants = data.frame(), expected = data.frame(), timezone = NULL) {
  if (nrow(options) == 0) {
    return(data.frame())
  }

  ranked <- data.frame(
    option_id = options$option_id,
    start_datetime = options$start_datetime,
    end_datetime = options$end_datetime,
    time_option = if (!is.null(timezone)) {
      vapply(seq_len(nrow(options)), function(i) {
        format_readable_option_for_option(options[i, , drop = FALSE], timezone)
      }, character(1))
    } else {
      options$display_label
    },
    time_zone = timezone %||% "",
    preferred_count = integer(nrow(options)),
    available_count = integer(nrow(options)),
    unavailable_count = integer(nrow(options)),
    missing_count = integer(nrow(options)),
    availability_score = integer(nrow(options)),
    required_attendee_conflicts = integer(nrow(options)),
    stringsAsFactors = FALSE
  )

  participant_count <- nrow(participants)
  for (i in seq_len(nrow(options))) {
    option_id <- options$option_id[[i]]
    option_responses <- responses[responses$option_id == option_id, , drop = FALSE]
    ranked$preferred_count[[i]] <- sum(option_responses$availability == "preferred", na.rm = TRUE)
    ranked$available_count[[i]] <- sum(option_responses$availability == "available", na.rm = TRUE)
    ranked$unavailable_count[[i]] <- sum(option_responses$availability == "unavailable", na.rm = TRUE)
    ranked$availability_score[[i]] <- sum(score_value(option_responses$availability), na.rm = TRUE)
    ranked$missing_count[[i]] <- max(0L, participant_count - length(unique(option_responses$participant_id)))
  }

  if (nrow(expected) > 0 && nrow(participants) > 0) {
    required_emails <- normalize_email(expected$email[expected$is_required == 1])
    for (i in seq_len(nrow(options))) {
      option_id <- options$option_id[[i]]
      conflicts <- 0L
      for (email in required_emails) {
        participant <- participants[normalize_email(participants$email) == email, , drop = FALSE]
        if (nrow(participant) == 0) {
          conflicts <- conflicts + 1L
          next
        }
        response <- responses[
          responses$participant_id == participant$participant_id[[1]] &
            responses$option_id == option_id,
          ,
          drop = FALSE
        ]
        if (nrow(response) == 0 || response$availability[[1]] == "unavailable") {
          conflicts <- conflicts + 1L
        }
      }
      ranked$required_attendee_conflicts[[i]] <- conflicts
    }
  }

  ranked <- ranked[order(
    -ranked$availability_score,
    ranked$required_attendee_conflicts,
    ranked$unavailable_count,
    -ranked$preferred_count,
    ranked$time_option
  ), , drop = FALSE]
  rownames(ranked) <- NULL
  ranked
}

build_availability_matrix <- function(options, responses, participants, timezone = NULL) {
  if (nrow(participants) == 0 || nrow(options) == 0) {
    return(data.frame())
  }

  rows <- list()
  counter <- 1L
  for (participant_index in seq_len(nrow(participants))) {
    participant <- participants[participant_index, , drop = FALSE]
    for (option_index in seq_len(nrow(options))) {
      option <- options[option_index, , drop = FALSE]
      response <- responses[
        responses$participant_id == participant$participant_id[[1]] &
          responses$option_id == option$option_id[[1]],
        ,
        drop = FALSE
      ]
      availability <- if (nrow(response) == 0) "missing" else response$availability[[1]]
      participant_email <- as.character(participant$email[[1]] %||% "")
      participant_organization <- as.character(participant$organization[[1]] %||% "")
      if (is.na(participant_email)) participant_email <- ""
      if (is.na(participant_organization)) participant_organization <- ""
      rows[[counter]] <- data.frame(
        participant_id = participant$participant_id[[1]],
        participant = participant$name[[1]],
        email = participant_email,
        organization = participant_organization,
        option_id = option$option_id[[1]],
        time_option = if (!is.null(timezone)) {
          format_readable_option_for_option(option, timezone)
        } else {
          option$display_label[[1]]
        },
        availability = availability,
        availability_label = availability_label(availability),
        stringsAsFactors = FALSE
      )
      counter <- counter + 1L
    }
  }
  do.call(rbind, rows)
}

find_missing_expected_participants <- function(expected, participants) {
  if (nrow(expected) == 0) {
    return(data.frame())
  }
  submitted <- normalize_email(participants$email %||% character())
  missing <- expected[!normalize_email(expected$email) %in% submitted, , drop = FALSE]
  rownames(missing) <- NULL
  missing
}
