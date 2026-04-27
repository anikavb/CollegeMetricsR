test_that("performance_score returns a numeric value", {
  result <- performance_score(
    gpa              = 3.5,
    hours_studied    = 20,
    attendance       = 90,
    sleep_hours      = 7,
    extracurricular  = "Yes"
  )
  expect_type(result, "double")
})

test_that("performance_score returns a single value", {
  result <- performance_score(
    gpa              = 3.5,
    hours_studied    = 20,
    attendance       = 90,
    sleep_hours      = 7,
    extracurricular  = "Yes"
  )
  expect_equal(length(result), 1)
})

test_that("performance_score is between 0 and 100", {
  result <- performance_score(
    gpa              = 3.5,
    hours_studied    = 20,
    attendance       = 90,
    sleep_hours      = 7,
    extracurricular  = "Yes"
  )
  expect_gte(result, 0)
  expect_lte(result, 100)
})

test_that("high achieving student scores higher than struggling student", {
  high <- performance_score(
    gpa              = 3.8,
    hours_studied    = 35,
    attendance       = 95,
    sleep_hours      = 8,
    extracurricular  = "Yes"
  )
  low <- performance_score(
    gpa              = 1.5,
    hours_studied    = 5,
    attendance       = 60,
    sleep_hours      = 5,
    extracurricular  = "No"
  )
  expect_gt(high, low)
})

test_that("Yes extracurricular scores higher than No, all else equal", {
  with_extra <- performance_score(
    gpa              = 3.0,
    hours_studied    = 20,
    attendance       = 80,
    sleep_hours      = 7,
    extracurricular  = "Yes"
  )
  without_extra <- performance_score(
    gpa              = 3.0,
    hours_studied    = 20,
    attendance       = 80,
    sleep_hours      = 7,
    extracurricular  = "No"
  )
  expect_gt(with_extra, without_extra)
})


