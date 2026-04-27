test_that("performance_score returns a numeric value", {
  result <- performance_score(3.5, 20, 90, 7, "Yes")
  expect_type(result, "double")
})

test_that("performance_score returns a single value", {
  result <- performance_score(3.5, 20, 90, 7, "Yes")
  expect_equal(length(result), 1)
})

test_that("performance_score is between 0 and 100", {
  result <- performance_score(3.5, 20, 90, 7, "Yes")
  expect_gte(result, 0)
  expect_lte(result, 100)
})

test_that("high achieving student scores higher than struggling student", {
  high <- performance_score(3.8, 35, 95, 8, "Yes")
  low  <- performance_score(1.5,  5, 60, 5, "No")
  expect_gt(high, low)
})

test_that("Yes extracurricular scores higher than No, all else equal", {
  with_extra    <- performance_score(3.0, 20, 80, 7, "Yes")
  without_extra <- performance_score(3.0, 20, 80, 7, "No")
  expect_gt(with_extra, without_extra)
})

test_that("performance_score errors on invalid gpa", {
  expect_error(performance_score(5.0, 20, 80, 7, "Yes"))
})

test_that("performance_score errors on negative hours_studied", {
  expect_error(performance_score(3.0, -5, 80, 7, "Yes"))
})

test_that("performance_score errors on attendance over 100", {
  expect_error(performance_score(3.0, 20, 110, 7, "Yes"))
})

test_that("performance_score errors on invalid extracurricular value", {
  expect_error(performance_score(3.0, 20, 80, 7, "Maybe"))
})

test_that("performance_score produces a message", {
  expect_message(performance_score(3.5, 20, 90, 7, "Yes"))
})
