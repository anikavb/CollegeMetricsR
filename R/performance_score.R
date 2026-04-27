#' Calculate Student Performance Score
#'
#' @description Combines key student lifestyle variables into a single
#'   performance score between 0 and 100. Positive behaviours like studying,
#'   sleeping well, attending class, and participating in extracurriculars
#'   increase the score. Low motivation reduces it.
#' @details Each variable is normalized to a 0-1 scale to make them
#'   comparable, then multiplied by a weight reflecting its importance.
#'   Weights: GPA 30%, Hours Studied 25%, Attendance 15%, Sleep 15%,
#'   Extracurriculars 15%. Motivation level applies a penalty:
#'   Low = -10, Medium = 0, High = +10.
#' @param gpa Numeric. Your GPA on a 0.0 to 4.0 scale.
#' @param hours_studied Numeric. Hours studied per week.
#' @param attendance Numeric. Attendance percentage (0-100).
#' @param sleep_hours Numeric. Average sleep hours per night.
#' @param extracurricular Character. Whether you participate in
#'   extracurriculars - "Yes" or "No".
#' @param motivation_level Character. Your motivation level -
#'   "Low", "Medium", or "High".
#' @return A single performance score between 0 and 100.
#' @export
#' @examples
#' performance_score(
#'   gpa              = 3.5,
#'   hours_studied    = 20,
#'   attendance       = 90,
#'   sleep_hours      = 7,
#'   extracurricular  = "Yes",
#'   motivation_level = "High"
#' )
performance_score <- function(gpa, hours_studied, attendance,
                              sleep_hours, extracurricular) {

  # Normalize each variable to 0-1
  gpa_norm    <- gpa           / 4
  study_norm  <- hours_studied / 44    # max in dataset is 44
  attend_norm <- attendance    / 100
  sleep_norm  <- sleep_hours   / 10    # max in dataset is 10

  # Extracurriculars: Yes = 1, No = 0
  extra_norm  <- ifelse(extracurricular == "Yes", 1, 0)

  # Apply weights
  score <- (gpa_norm    * 30) +
    (study_norm  * 25) +
    (attend_norm * 15) +
    (sleep_norm  * 15) +
    (extra_norm  * 15)

  # Keep score between 0 and 100
  score <- pmin(pmax(score, 0), 100)

  round(score, 2)
}

