sample_df <- data.frame(
  Sleep_Hours   = c(5, 6, 7, 8, 9),
  Hours_Studied = c(10, 15, 20, 25, 30),
  Exam_Score    = c(55, 65, 75, 85, 95)
)
sample_df <- scale_exam_score(sample_df)

# plot_sleep_vs_gpa tests
test_that("plot_sleep_vs_gpa returns a ggplot object", {
  result <- plot_sleep_vs_gpa(sample_df)
  expect_s3_class(result, "ggplot")
})

test_that("plot_sleep_vs_gpa uses correct x variable", {
  result <- plot_sleep_vs_gpa(sample_df)
  expect_equal(rlang::as_name(result$mapping$x), "Sleep_Hours")
})

test_that("plot_sleep_vs_gpa uses correct y variable", {
  result <- plot_sleep_vs_gpa(sample_df)
  expect_equal(rlang::as_name(result$mapping$y), "gpa")
})

# plot_study_vs_gpa tests
test_that("plot_study_vs_gpa returns a ggplot object", {
  result <- plot_study_vs_gpa(sample_df)
  expect_s3_class(result, "ggplot")
})

test_that("plot_study_vs_gpa uses correct x variable", {
  result <- plot_study_vs_gpa(sample_df)
  expect_equal(rlang::as_name(result$mapping$x), "Hours_Studied")
})

test_that("plot_study_vs_gpa uses correct y variable", {
  result <- plot_study_vs_gpa(sample_df)
  expect_equal(rlang::as_name(result$mapping$y), "gpa")
})

# plot_relationship tests
test_that("plot_relationship returns a ggplot object", {
  result <- plot_relationship(sample_df, "Sleep_Hours", "gpa")
  expect_s3_class(result, "ggplot")
})

test_that("plot_relationship errors when df is not a data frame", {
  expect_error(plot_relationship("not_a_df", "Sleep_Hours", "gpa"))
})

test_that("plot_relationship errors when x_var not in data frame", {
  expect_error(plot_relationship(sample_df, "fake_column", "gpa"))
})

test_that("plot_relationship errors when y_var not in data frame", {
  expect_error(plot_relationship(sample_df, "Sleep_Hours", "fake_column"))
})

test_that("plot_relationship errors when x_var is not numeric", {
  df_with_char <- sample_df
  df_with_char$Name <- c("Alice", "Bob", "Carol", "Dave", "Eve")
  expect_error(plot_relationship(df_with_char, "Name", "gpa"))
})

test_that("plot_relationship errors when y_var is not numeric", {
  df_with_char <- sample_df
  df_with_char$Name <- c("Alice", "Bob", "Carol", "Dave", "Eve")
  expect_error(plot_relationship(df_with_char, "Sleep_Hours", "Name"))
})
