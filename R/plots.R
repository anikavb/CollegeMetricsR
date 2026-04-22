#' Plot Sleep Hours vs GPA
#'
#' @description Scatterplot of sleep hours against GPA with a trend line.
#' @param df A data frame with \code{Sleep_Hours} and \code{gpa} columns.
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @examples
#' df_gpa <- scale_exam_score(student_data)
#' plot_sleep_vs_gpa(df_gpa)
plot_sleep_vs_gpa <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = Sleep_Hours, y = gpa)) +
    ggplot2::geom_point(alpha = 0.3, color = "steelblue") +
    ggplot2::geom_smooth(method = "lm", color = "darkblue") +
    ggplot2::labs(
      title = "Sleep hours vs. GPA",
      x     = "Sleep hours per night",
      y     = "GPA (0.0 - 4.0)"
    ) +
    ggplot2::theme_minimal()
}


#' Plot Study Time vs Exam Score
#'
#' @description Scatterplot of hours studied against exam score with a
#'   trend line.
#' @param df A cleaned data frame with \code{Hours_Studied} and
#'   \code{Exam_Score} columns.
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @examples
#' plot_study_vs_score(student_data)
plot_study_vs_score <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = Hours_Studied, y = Exam_Score)) +
    ggplot2::geom_point(alpha = 0.3, color = "purple") +
    ggplot2::geom_smooth(method = "lm", color = "darkviolet") +
    ggplot2::labs(
      title = "Study time vs. exam score",
      x     = "Hours studied per week",
      y     = "Exam score (0-100)"
    ) +
    ggplot2::theme_minimal()
}
