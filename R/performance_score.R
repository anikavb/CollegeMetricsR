#' Calculate Student Performance Score
#'
#' @description Combines key student lifestyle variables into a single
#'   performance score between 0 and 100. Positive behaviours like studying,
#'   sleeping well, attending class, and participating in extracurriculars
#'   increase the score.
#' @details Each variable is normalized to a 0-1 scale to make them
#'   comparable, then multiplied by a weight reflecting its importance.
#'   Weights: GPA 30%, Hours Studied 25%, Attendance 15%, Sleep 15%,
#'   Extracurriculars 15%.
#' @param gpa Numeric. Your GPA on a 0.0 to 4.0 scale.
#' @param hours_studied Numeric. Hours studied per week.
#' @param attendance Numeric. Attendance percentage (0-100).
#' @param sleep_hours Numeric. Average sleep hours per night.
#' @param extracurricular Character. Whether you participate in
#'   extracurriculars - "Yes" or "No".
#' @return A single performance score between 0 and 100, with suggestions on what to improve.
#' @export
#' @examples
#' performance_score(
#'   gpa              = 3.5,
#'   hours_studied    = 20,
#'   attendance       = 90,
#'   sleep_hours      = 7,
#'   extracurricular  = "Yes" )

performance_score <- function(gpa, hours_studied, attendance,
                              sleep_hours, extracurricular) {
  if (!is.numeric(gpa) || length(gpa) != 1 || gpa < 0 || gpa > 4) {
    stop("gpa must be a single numeric value between 0 and 4.")
  }
  if (!is.numeric(hours_studied) || length(hours_studied) != 1 || hours_studied < 0) {
    stop("hours_studied must be a single numeric value.")
  }
  if (!is.numeric(attendance) || length(attendance) != 1 || attendance < 0 || attendance > 100) {
    stop("attendance must be a single numeric value between 0 and 100.")
  }
  if (!is.numeric(sleep_hours) || length(sleep_hours) != 1 || sleep_hours < 0) {
    stop("sleep_hours must be a single numeric value.")
  }
  if (!is.character(extracurricular) || length(extracurricular) != 1 ||
      !extracurricular %in% c("Yes", "No")) {
    stop("Indicate participation in extracurriculars with either 'Yes' or 'No'.")
  }

  # Normalize
  gpa_norm    <- gpa           / 4
  study_norm  <- hours_studied / 44
  attend_norm <- attendance    / 100
  sleep_norm  <- sleep_hours   / 10
  extra_norm  <- ifelse(extracurricular == "Yes", 1, 0)

  # Weighted score
  score <- (gpa_norm    * 30) +
    (study_norm  * 25) +
    (attend_norm * 15) +
    (sleep_norm  * 15) +
    (extra_norm  * 15)

  score <- round(pmin(pmax(score, 0), 100), 2)

  # Performance message
  if (score >= 80) {
    message("Strong performance profile: well-balanced habits.")
  } else if (score >= 60) {
    message("Moderate performance: some areas could be improved.")
  } else {
    message("Lower performance: consider improving study time, attendance, or sleep.")
  }

  # Suggestions
  suggestions <- c()
  if (gpa < 2.5) {
    suggestions <- c(suggestions, "Focus on improving academic performance (GPA is relatively low).")
  }
  if (hours_studied < 15) {
    suggestions <- c(suggestions, "Consider increasing weekly study time.")
  }
  if (attendance < 75) {
    suggestions <- c(suggestions, "Improving class attendance could significantly boost performance.")
  }
  if (sleep_hours < 6) {
    suggestions <- c(suggestions, "Try to get more consistent sleep (at least 6-8 hours).")
  }
  if (extracurricular == "No") {
    suggestions <- c(suggestions, "Participating in extracurricular activities may improve balance.")
  }
  if (length(suggestions) > 0) {
    message("Suggestions for improvement:")
    for (s in suggestions) {
      message("- ", s)
    }
  } else {
    message("No major improvements needed: strong overall profile.")
  }

  return(score)
}
