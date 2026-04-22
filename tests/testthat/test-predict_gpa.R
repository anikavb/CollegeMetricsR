sample_df <- data.frame(
  Hours_Studied    = c(30, 25, 20, 15, 10, 28, 22, 18),
  Sleep_Hours      = c(8, 7, 7, 6, 5, 8, 7, 6),
  Attendance       = c(95, 90, 85, 80, 70, 92, 88, 82),
  Previous_Scores  = c(90, 80, 75, 65, 55, 88, 78, 68),
  Motivation_Level = c("High", "High", "Medium", "Low",
                       "Low", "High", "Medium", "Medium"),
  Exam_Score       = c(95, 85, 75, 65, 55, 90, 80, 70)
)
sample_df <- scale_exam_score(sample_df)

test_that("predict_gpa returns a numeric value", {
  result <- predict_gpa(sample_df,
                        hours_studied    = 20,
                        sleep_hours      = 7,
                        attendance       = 85,
                        previous_scores  = 75,
                        motivation_level = "Medium")
  expect_type(result, "double")
})

test_that("predict_gpa returns a single value", {
  result <- predict_gpa(sample_df,
                        hours_studied    = 20,
                        sleep_hours      = 7,
                        attendance       = 85,
                        previous_scores  = 75,
                        motivation_level = "Medium")
  expect_equal(length(result), 1)
})

test_that("predict_gpa returns a value in a reasonable GPA range", {
  result <- predict_gpa(sample_df,
                        hours_studied    = 20,
                        sleep_hours      = 7,
                        attendance       = 85,
                        previous_scores  = 75,
                        motivation_level = "Medium")
  expect_gte(result, 0)
  expect_lte(result, 4)
})

test_that("predict_gpa gives higher GPA for stronger student inputs", {
  strong_student <- predict_gpa(sample_df,
                                hours_studied    = 30,
                                sleep_hours      = 9,
                                attendance       = 95,
                                previous_scores  = 95,
                                motivation_level = "High")

  weak_student <- predict_gpa(sample_df,
                              hours_studied    = 5,
                              sleep_hours      = 4,
                              attendance       = 60,
                              previous_scores  = 40,
                              motivation_level = "Low")

  expect_gt(strong_student, weak_student)
})

test_that("predict_gpa works with all three motivation levels", {
  low <- predict_gpa(sample_df,
                     hours_studied    = 20,
                     sleep_hours      = 7,
                     attendance       = 85,
                     previous_scores  = 75,
                     motivation_level = "Low")

  medium <- predict_gpa(sample_df,
                        hours_studied    = 20,
                        sleep_hours      = 7,
                        attendance       = 85,
                        previous_scores  = 75,
                        motivation_level = "Medium")

  high <- predict_gpa(sample_df,
                      hours_studied    = 20,
                      sleep_hours      = 7,
                      attendance       = 85,
                      previous_scores  = 75,
                      motivation_level = "High")

  expect_type(low,    "double")
  expect_type(medium, "double")
  expect_type(high,   "double")
})
