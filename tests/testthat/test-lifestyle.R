test_that("lifestyle_summary summarizes default variables", {
  df <- data.frame(
    Hours_Studied = c(10, 20, 30),
    Sleep_Hours = c(6, 7, 8),
    Exam_Score = c(70, 80, 90),
    gpa = c(2.8, 3.2, 3.6)
  )

  result <- lifestyle_summary(df)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_true(all(c("variable", "mean", "median", "sd", "min", "max", "n_missing") %in% names(result)))
})

test_that("lifestyle_summary summarizes selected variables", {
  df <- data.frame(
    Hours_Studied = c(10, 20, 30),
    Sleep_Hours = c(6, 7, 8),
    Exam_Score = c(70, 80, 90),
    gpa = c(2.8, 3.2, 3.6)
  )

  result <- lifestyle_summary(df, vars = c("Hours_Studied", "Sleep_Hours"))

  expect_equal(nrow(result), 2)
  expect_equal(result$variable, c("Hours_Studied", "Sleep_Hours"))
})

test_that("lifestyle_summary calculates correct mean", {
  df <- data.frame(
    Hours_Studied = c(10, 20, 30),
    Sleep_Hours = c(6, 7, 8),
    Exam_Score = c(70, 80, 90),
    gpa = c(2.8, 3.2, 3.6)
  )

  result <- lifestyle_summary(df, vars = "Hours_Studied")

  expect_equal(result$mean, 20)
  expect_equal(result$min, 10)
  expect_equal(result$max, 30)
})

test_that("lifestyle_summary counts missing values", {
  df <- data.frame(
    Hours_Studied = c(10, NA, 30),
    Sleep_Hours = c(6, 7, 8),
    Exam_Score = c(70, 80, 90),
    gpa = c(2.8, 3.2, 3.6)
  )

  result <- lifestyle_summary(df, vars = "Hours_Studied")

  expect_equal(result$n_missing, 1)
  expect_equal(result$mean, 20)
})

test_that("lifestyle_summary errors for missing variables", {
  df <- data.frame(
    Hours_Studied = c(10, 20, 30),
    Sleep_Hours = c(6, 7, 8)
  )

  expect_error(
    lifestyle_summary(df, vars = "Attendance"),
    "missing from the data"
  )
})

test_that("lifestyle_summary errors for non-numeric variables", {
  df <- data.frame(
    Hours_Studied = c(10, 20, 30),
    Sleep_Hours = c(6, 7, 8),
    Exam_Score = c(70, 80, 90),
    gpa = c(2.8, 3.2, 3.6),
    Extracurricular_Activities = c("Yes", "No", "Yes")
  )

  expect_error(
    lifestyle_summary(df, vars = "Extracurricular_Activities"),
    "must be numeric"
  )
})



test_that("student_correlation returns a list with expected elements", {
  df <- data.frame(
    Hours_Studied = c(10, 20, 30),
    Sleep_Hours = c(6, 7, 8)
  )

  result <- student_correlation(df, "Hours_Studied", "Sleep_Hours")

  expect_true(is.list(result))
  expect_true("correlation" %in% names(result))
  expect_true("interpretation" %in% names(result))
})

test_that("student_correlation computes correct correlation", {
  df <- data.frame(
    x = c(1, 2, 3),
    y = c(2, 4, 6)
  )

  result <- student_correlation(df, "x", "y")

  expect_equal(result$correlation, 1)
})

test_that("student_correlation errors if variables are missing", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    student_correlation(df, "a", "c"),
    "must exist"
  )
})

test_that("student_correlation errors if variables are not numeric", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c("Yes", "No", "Yes")
  )

  expect_error(
    student_correlation(df, "a", "b"),
    "must be numeric"
  )
})


