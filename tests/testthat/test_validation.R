testthat::test_that("email and token validation work", {
  testthat::expect_equal(validate_email(" User@Example.ORG "), "user@example.org")
  token <- generate_token()
  testthat::expect_match(token, "^[a-f0-9]{64}$")
  testthat::expect_match(hash_token(token), "^[a-f0-9]{64}$")
  testthat::expect_error(validate_token("not-a-token"), "invalid")
})

testthat::test_that("expected participant parsing validates rows", {
  parsed <- parse_expected_participants("Alex Lee,alex@example.org,Institute A,yes\nSam Patel,sam@example.org,Partner,no")
  testthat::expect_equal(nrow(parsed), 2)
  testthat::expect_equal(parsed$is_required, c(1L, 0L))
  testthat::expect_error(parse_expected_participants("Bad User,not-email,Org,yes"), "valid")
})

testthat::test_that("availability scoring uses configured weights", {
  testthat::expect_equal(score_value(c("preferred", "available", "unavailable", "missing")), c(2L, 1L, 0L, 0L))
})

testthat::test_that("UTC timestamp parsing supports multiple proposed times", {
  parsed <- parse_utc_timestamp(c("2026-05-07T13:00:00Z", "2026-05-04T15:00:00Z"))
  testthat::expect_s3_class(parsed, "POSIXct")
  testthat::expect_equal(length(parsed), 2)
  testthat::expect_false(any(is.na(parsed)))
})
