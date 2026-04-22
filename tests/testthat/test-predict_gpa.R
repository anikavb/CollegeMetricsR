test_that("predict_gpa returns a numeric value", {
  result <- predict_gpa(hours_studied = 20, sleep_hours = 7, attendance = 85, previous_scores = 75, motivation_level = "Medium")
  expect_true(is.numeric(result))
})
test_that("predict_gpa returns a single value", {
  result <- predict_gpa(hours_studied = 20, sleep_hours = 7, attendance = 85, previous_scores = 75, motivation_level = "Medium")
  expect_length(result, 1)
})
test_that("predict_gpa returns a value in a reasonable GPA range", {
  result <- predict_gpa(hours_studied = 20, sleep_hours = 7, attendance = 85, previous_scores = 75, motivation_level = "Medium")
  expect_true(result >= 0 & result <= 4)
})
test_that("predict_gpa gives higher GPA for stronger student inputs", {
  high <- predict_gpa(hours_studied = 30, sleep_hours = 9, attendance = 95, previous_scores = 95, motivation_level = "High")
  low  <- predict_gpa(hours_studied = 10, sleep_hours = 5, attendance = 60, previous_scores = 50, motivation_level = "Low")
  expect_true(high > low)
})
test_that("predict_gpa works with all three motivation levels", {
  expect_no_error(predict_gpa(hours_studied = 20, sleep_hours = 7, attendance = 85, previous_scores = 75, motivation_level = "Low"))
  expect_no_error(predict_gpa(hours_studied = 20, sleep_hours = 7, attendance = 85, previous_scores = 75, motivation_level = "Medium"))
  expect_no_error(predict_gpa(hours_studied = 20, sleep_hours = 7, attendance = 85, previous_scores = 75, motivation_level = "High"))
})
