library(CollegeMetricsR)

test_that("load_student_data returns a data frame", {
  result <- load_student_data()
  expect_s3_class(result, "data.frame")
})

test_that("load_student_data has no missing values", {
  result <- load_student_data()
  expect_equal(sum(is.na(result)), 0)
})

test_that("load_student_data returns more than 0 rows", {
  result <- load_student_data()
  expect_gt(nrow(result), 0)
})

