library(CollegeMetricsR)

test_that("load_student_data returns a dataframe", {
  df <- load_student_data()
  expect_true(is.data.frame(df))
})

test_that("clean_student_data removes missing values", {
  df <- data.frame(x = c(1, NA, 3))
  cleaned <- clean_student_data(df)
  expect_false(any(is.na(cleaned)))
})
