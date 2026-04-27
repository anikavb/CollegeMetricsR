#' Scale Exam Scores to GPA
#'
#' @description Converts exam scores (0-100) to a GPA scale (0.0-4.0)
#'   and adds a \code{gpa} column to the data frame.
#' @param df A cleaned data frame with an \code{exam_score} column.
#' @return The data frame with a new \code{gpa} column.
#' @importFrom stats median sd
#' @export
#' @examples
#' df_gpa <- scale_exam_score(student_data)
#' head(df_gpa[, c("Exam_Score", "gpa")])

scale_exam_score <- function(df){

  if (!is.data.frame(df)) {
    stop("Please input a data frame.")
  }

  if (!"Exam_Score" %in% names(df)) {
    stop("Data must contain an Exam_Score column.")
  }

  if (!is.numeric(df$Exam_Score)) {
    stop("Exam_Score must be numeric.")
  }

  if (any(is.na(df$Exam_Score))) {
    message("Missing values detected in Exam_Score; GPA will be calculated where possible.")
  }

  df$Exam_Score <- pmin(df$Exam_Score, 100)
  df$gpa <- (df$Exam_Score/100)*4
  return(df)
  }

#' Grade Analysis
#'
#' @description Summarizes the GPA distribution across all students.
#' @param df A data frame with a \code{gpa} column. Run
#'   \code{scale_exam_score()} first.
#' @return A one-row data frame with mean, median, SD, min, and max GPA.
#' @export
#' @examples
#' df_gpa <- scale_exam_score(student_data)
#' grade_analysis(df_gpa)
grade_analysis <- function(df) {

  if (!is.data.frame(df)) {
    stop("Please input a data frame.")
  }

  if (!"gpa" %in% names(df)) {
    stop("Data must contain a gpa column. Run scale_exam_score() first.")
  }

  if (!is.numeric(df$gpa)) {
    stop("gpa must be numeric.")
  }

  if (any(is.na(df$gpa))) {
    message("Missing values detected in gpa; summary calculated using available data.")
  }

  message("Displaying gpa analysis for student data...")

  data.frame(
    mean_gpa   = mean(df$gpa, na.rm = TRUE),
    median_gpa = median(df$gpa, na.rm = TRUE),
    sd_gpa     = sd(df$gpa, na.rm = TRUE),
    min_gpa    = min(df$gpa, na.rm = TRUE),
    max_gpa    = max(df$gpa, na.rm = TRUE),
    n          = nrow(df)
  )
}
