with_database_env <- function(values, code) {
  names <- names(values)
  old <- Sys.getenv(names, unset = NA_character_)
  names(old) <- names
  on.exit({
    for (name in names) {
      if (is.na(old[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(list(old[[name]]), name))
      }
    }
  }, add = TRUE)
  for (name in names) {
    value <- values[[name]]
    if (is.na(value)) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, stats::setNames(list(value), name))
    }
  }
  force(code)
}

testthat::test_that("SQLite remains the default database backend", {
  with_database_env(
    list(DATABASE_BACKEND = NA_character_, DATABASE_URL = NA_character_),
    {
      testthat::expect_equal(database_backend(), "sqlite")
      conn <- get_db_connection(db_path = tempfile(fileext = ".sqlite"), use_pool = FALSE)
      on.exit(close_db_connection(conn), add = TRUE)
      testthat::expect_true(DBI::dbIsValid(conn))
    }
  )
})

testthat::test_that("invalid database backend fails clearly", {
  with_database_env(
    list(DATABASE_BACKEND = "postgres"),
    testthat::expect_error(database_backend(), "DATABASE_BACKEND must be either")
  )
})

testthat::test_that("MongoDB backend requires explicit Atlas settings", {
  with_database_env(
    list(
      DATABASE_BACKEND = "mongodb",
      MONGODB_URI = NA_character_,
      MONGODB_DATABASE = "meeting_poll"
    ),
    testthat::expect_error(get_db_connection(use_pool = FALSE), "MONGODB_URI is required")
  )

  with_database_env(
    list(
      DATABASE_BACKEND = "mongodb",
      MONGODB_URI = "mongodb+srv://user:password@example.invalid/",
      MONGODB_DATABASE = NA_character_
    ),
    testthat::expect_error(get_db_connection(use_pool = FALSE), "MONGODB_DATABASE is required")
  )
})

testthat::test_that("DATABASE_URL is not silently accepted in SQLite mode", {
  with_database_env(
    list(DATABASE_BACKEND = "sqlite", DATABASE_URL = "postgres://example.invalid/app"),
    testthat::expect_error(
      get_db_connection(db_path = tempfile(fileext = ".sqlite"), use_pool = FALSE),
      "DATABASE_URL is not used by this app"
    )
  )
})

testthat::test_that("MongoDB JSON helper emits empty objects for empty query values", {
  testthat::expect_equal(as.character(mongo_json(list())), "{}")
  testthat::expect_equal(as.character(mongo_json(list(response_token = "abc"))), "{\"response_token\":\"abc\"}")
})
