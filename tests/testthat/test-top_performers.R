test_that("top_performers returns requested number of rows", {
  df <- data.frame(
    Student_ID = 1:5,
    Exam_Score = c(70, 95, 80, 88, 60)
  )

  result <- top_performers(df, n = 3)

  expect_equal(nrow(result), 3)
})

test_that("top_performers ranks highest scores first", {
  df <- data.frame(
    Student_ID = 1:5,
    Exam_Score = c(70, 95, 80, 88, 60)
  )

  result <- top_performers(df, n = 2)

  expect_equal(result$Exam_Score, c(95, 88))
  expect_equal(result$rank, c(1, 2))
})

test_that("top_performers works with custom metric", {
  df <- data.frame(
    Student_ID = 1:4,
    Exam_Score = c(70, 80, 90, 60),
    gpa = c(2.8, 3.2, 3.9, 2.4)
  )

  result <- top_performers(df, n = 2, metric = "gpa")

  expect_equal(result$gpa, c(3.9, 3.2))
})

test_that("top_performers errors for missing metric", {
  df <- data.frame(
    Student_ID = 1:3,
    Exam_Score = c(70, 80, 90)
  )

  expect_error(
    top_performers(df, metric = "gpa"),
    "metric must be a column"
  )
})

test_that("top_performers errors for non-numeric metric", {
  df <- data.frame(
    Student_ID = 1:3,
    Exam_Score = c(70, 80, 90),
    Motivation_Level = c("Low", "Medium", "High")
  )

  expect_error(
    top_performers(df, metric = "Motivation_Level"),
    "metric must be numeric"
  )
})
