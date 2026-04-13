sample_df <- data.frame(
  Exam_Score = c(100, 75, 50, 25, 0)
)

test_that("scale_exam_score adds a gpa column", {
  result <- scale_exam_score(sample_df)
  expect_true("gpa" %in% names(result))
})

test_that("scale_exam_score maps scores correctly", {
  result <- scale_exam_score(sample_df)
  expect_equal(result$gpa, c(4.0, 3.0, 2.0, 1.0, 0.0))
})

test_that("scale_exam_score keeps all original rows", {
  result <- scale_exam_score(sample_df)
  expect_equal(nrow(result), nrow(sample_df))
})

test_that("grade_analysis returns a one-row data frame", {
  df_gpa <- scale_exam_score(sample_df)
  result <- grade_analysis(df_gpa)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("grade_analysis returns correct column names", {
  df_gpa <- scale_exam_score(sample_df)
  result <- grade_analysis(df_gpa)
  expect_true("mean_gpa"   %in% names(result))
  expect_true("median_gpa" %in% names(result))
  expect_true("sd_gpa"     %in% names(result))
  expect_true("min_gpa"    %in% names(result))
  expect_true("max_gpa"    %in% names(result))
})

test_that("grade_analysis min and max are correct", {
  df_gpa <- scale_exam_score(sample_df)
  result <- grade_analysis(df_gpa)
  expect_equal(result$min_gpa, 0.0)
  expect_equal(result$max_gpa, 4.0)
})

