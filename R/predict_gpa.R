#' Predict GPA
#'
#' @description Predicts a student's GPA based on their personal habits
#'   using a linear regression model trained on the full dataset.
#' @param hours_studied Numeric. How many hours the student studies per week.
#' @param sleep_hours Numeric. How many hours of sleep the student gets per night.
#' @param attendance Numeric. The student's attendance percentage (0-100).
#' @param previous_scores Numeric. The student's previous exam score (0-100).
#' @param motivation_level Character. Motivation level - "Low", "Medium", or "High".
#' @return A single predicted GPA value (0.0 - 4.0).
#' @export
#' @examples
#' predict_gpa(
#'   hours_studied   = 20,
#'   sleep_hours     = 7,
#'   attendance      = 85,
#'   previous_scores = 75,
#'   motivation_level = "Medium"
#' )
predict_gpa <- function(hours_studied, sleep_hours,
                        attendance, previous_scores, motivation_level) {

  if (!is.numeric(hours_studied) || length(hours_studied) != 1) {
    stop("hours_studied must be a single numeric value.")
  }

  if (!is.numeric(sleep_hours) || length(sleep_hours) != 1) {
    stop("sleep_hours must be a single numeric value.")
  }

  if (!is.numeric(attendance) || length(attendance) != 1) {
    stop("attendance must be a single numeric value.")
  }

  if (!is.numeric(previous_scores) || length(previous_scores) != 1) {
    stop("previous_scores must be a single numeric value.")
  }

  if (!is.character(motivation_level) || length(motivation_level) != 1) {
    stop("motivation_level must be a single character string.")
  }

  valid_levels <- c("Low", "Medium", "High")
  if (!motivation_level %in% valid_levels) {
    stop("motivation_level must be one of: 'Low', 'Medium', or 'High'.")
  }

  if (attendance < 0 || attendance > 100) {
    warning("attendance is typically between 0 and 100.")
  }

  if (previous_scores < 0 || previous_scores > 100) {
    warning("previous_scores is typically between 0 and 100.")
  }

  df <- student_data
  df <- scale_exam_score(df)

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

