generate_final_email_text <- function(poll, option, final_notes = "") {
  location <- sanitize_text(poll$location_details[[1]] %||% "", max_chars = 1000)
  location_line <- if (nzchar(location)) paste0("\nLocation/details: ", location) else ""
  notes <- sanitize_text(final_notes %||% "", max_chars = 2000)
  notes_line <- if (nzchar(notes)) paste0("\n\nAdditional notes:\n", notes) else ""

  paste0(
    "Subject: Final meeting time - ", poll$title[[1]], "\n\n",
    "Hello,\n\n",
    "Thank you for sharing your availability. The meeting has been scheduled for:\n\n",
    format_readable_option_for_option(option, poll$timezone[[1]]),
    "\n",
    "Time zone: ",
    poll$timezone[[1]],
    "\n",
    "Duration: ", poll$duration_minutes[[1]], " minutes",
    location_line,
    "\n\n",
    "Please add this time to your calendar. No calendar invitation has been sent automatically by this app.",
    notes_line,
    "\n\n",
    "Best,\n",
    poll$organizer_name[[1]]
  )
}
