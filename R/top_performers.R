#' Get Top Performing Students
#' @description Returns the top students in the CollegeMetricsR dataset based
#'   on a selected performance metric, such as Exam_Score, gpa, or Hours_Studied.
#' @param df A CollegeMetricsR student data frame.
#' @param n Number of students to return. Defaults to 10.
#' @param metric Numeric column to rank by. Defaults to \code{"Exam_Score"}.
#' @return A data frame with the top students and a rank column.
#' @export
#' @examples
#' top_performers(student_data)
#' df <- scale_exam_score(student_data)
#' top_performers(df, n = 5, metric = "gpa")

top_performers <- function(df, n = 10, metric = "Exam_Score") {

  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }

  if (!metric %in% names(df)) {
    stop("metric must be a column in the data.")
  }

  if (!is.numeric(df[[metric]])) {
    stop("metric must be numeric.")
  }

  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop("n must be a positive number.")
  }

  n <- floor(n)

  if (n > nrow(df)) {
    warning("n is larger than the number of rows; returning all rows.")
    n <- nrow(df)
  }

  df <- df[!is.na(df[[metric]]), ]

  ordered_df <- df[order(df[[metric]], decreasing = TRUE), ]

  result <- head(ordered_df, n)
  result$rank <- seq_len(nrow(result))

  result <- result[, c("rank", setdiff(names(result), "rank"))]

  rownames(result) <- NULL

  return(result)
}
