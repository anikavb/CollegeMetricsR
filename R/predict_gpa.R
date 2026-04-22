#' Predict GPA
#'
#' @description Predicts a student's GPA based on their personal habits
#'   using a linear regression model trained on the full dataset.
#' @param df A data frame with a \code{gpa} column. Run
#'   \code{scale_exam_score()} first.
#' @param hours_studied Numeric. How many hours the student studies per week.
#' @param sleep_hours Numeric. How many hours of sleep the student gets per night.
#' @param attendance Numeric. The student's attendance percentage (0-100).
#' @param previous_scores Numeric. The student's previous exam score (0-100).
#' @param motivation_level Character. Motivation level - "Low", "Medium", or "High".
#' @return A single predicted GPA value (0.0 - 4.0).
#' @export
#' @examples
#' df_gpa <- scale_exam_score(student_data)
#' predict_gpa(df_gpa,
#'             hours_studied   = 20,
#'             sleep_hours     = 7,
#'             attendance      = 85,
#'             previous_scores = 75,
#'             motivation_level = "Medium")
predict_gpa <- function(df, hours_studied, sleep_hours,
                        attendance, previous_scores, motivation_level) {

  new_data <- data.frame(
    Hours_Studied    = hours_studied,
    Sleep_Hours      = sleep_hours,
    Attendance       = attendance,
    Previous_Scores  = previous_scores,
    Motivation_Level = motivation_level
  )

  model <- lm(gpa ~ Hours_Studied + Sleep_Hours + Attendance +
                Previous_Scores + Motivation_Level, data = df)

  pred <- predict(model, newdata = new_data)
  round(pred, 2)
}

