testthat::test_that("creation route respects optional creation secret", {
  testthat::expect_true(can_create_poll(list(), creation_secret = ""))
  testthat::expect_true(can_create_poll(list(create = "secret-value"), creation_secret = "secret-value"))
  testthat::expect_false(can_create_poll(list(), creation_secret = "secret-value"))
  testthat::expect_false(can_create_poll(list(create = "wrong"), creation_secret = "secret-value"))
})

testthat::test_that("availability helper labels remain stable", {
  testthat::expect_equal(availability_short_label("preferred"), "Preferred")
  testthat::expect_equal(availability_short_label("available"), "Available")
  testthat::expect_equal(availability_short_label("unavailable"), "Unavailable")
  testthat::expect_equal(availability_short_label("missing"), "Missing")
  testthat::expect_equal(availability_hint("preferred"), "Can attend; works especially well")
})
