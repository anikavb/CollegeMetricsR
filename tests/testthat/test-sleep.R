test_that("sleep_summary calculates correct mean", {
  df <- data.frame(Sleep_Hours = c(6, 8, 10))
  result <- sleep_summary(df)

  expect_equal(result$mean_sleep, 8)
})

test_that("sleep_summary calculates correct standard deviation", {
  df <- data.frame(Sleep_Hours = c(6, 8, 10))
  result <- sleep_summary(df)

  expect_equal(result$sd_sleep, sd(c(6, 8, 10)))
})

test_that("sleep_summary handles missing values correctly", {
  df <- data.frame(Sleep_Hours = c(6, NA, 10))
  result <- sleep_summary(df)

  expect_equal(result$mean_sleep, 8)
})

test_that("sleep_summary returns numeric values", {
  df <- data.frame(Sleep_Hours = c(5, 7, 9))
  result <- sleep_summary(df)

  expect_true(is.numeric(result$mean_sleep))
  expect_true(is.numeric(result$sd_sleep))
})

test_that("sleep_study_relationship returns a list", {
  df <- data.frame(
    Sleep_Hours = c(5, 6, 7),
    Hours_Studied = c(1, 3, 2)
  )
  result <- sleep_study_relationship(df)
  expect_type(result, "list")
})

test_that("sleep_study_relationship has correlation and interpretation", {
  df <- data.frame(
    Sleep_Hours = c(5, 6, 7),
    Hours_Studied = c(1, 3, 2)
  )
  result <- sleep_study_relationship(df)
  expect_true("correlation"    %in% names(result))
  expect_true("interpretation" %in% names(result))
})

test_that("correlation is between -1 and 1", {
  df <- data.frame(
    Sleep_Hours = c(5, 6, 7),
    Hours_Studied = c(1, 3, 2)
  )

  result <- sleep_study_relationship(df)

  expect_true(result >= -1 && result <= 1)
})
