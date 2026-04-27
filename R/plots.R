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
    ggplot2::geom_point(alpha = 0.3, color = "purple") +
    ggplot2::geom_smooth(method = "lm", color = "darkviolet") +
    ggplot2::labs(
      title = "Sleep hours vs. GPA",
      x     = "Sleep hours per night",
      y     = "GPA (0.0 - 4.0)"
    ) +
    ggplot2::theme_minimal()
}


#' Plot Study Time vs GPA
#'
#' @description Scatterplot of hours studied against GPA with a
#'   trend line.
#' @param df A cleaned data frame with \code{Hours_Studied} and
#'   \code{gpa} columns.
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @examples
#' plot_study_vs_gpa(student_data)
plot_study_vs_gpa <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = Hours_Studied, y = gpa)) +
    ggplot2::geom_point(alpha = 0.3, color = "purple") +
    ggplot2::geom_smooth(method = "lm", color = "darkviolet") +
    ggplot2::labs(
      title = "Study time vs. GPA",
      x     = "Hours studied per week",
      y     = "GPA (0.0 - 4.0)"
    ) +
    ggplot2::theme_minimal()
}

#' Plot Relationship Between Any Two Variables
#'
#' @description Creates a flexible scatterplot that allows users to
#'   visualize the relationship between any two numeric variables, with a trend line.
#' @param df A cleaned data frame.
#' @param x_var A string representing the name of the variable for the x-axis.
#' @param y_var A string representing the name of the variable for the y-axis.
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @examples
#' plot_relationship(student_data, "Motivation_Level", "GPA")
plot_relationship <- function(df, x_var, y_var) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }

  if (!is.character(x_var) || length(x_var) != 1) {
    stop("x_var must be a single character string.")
  }

  if (!is.character(y_var) || length(y_var) != 1) {
    stop("y_var must be a single character string.")
  }

  if (!x_var %in% names(df)) {
    stop(paste("Column", x_var, "not found in data frame."))
  }

  if (!y_var %in% names(df)) {
    stop(paste("Column", y_var, "not found in data frame."))
  }

  if (!is.numeric(df[[x_var]])) {
    stop(paste(x_var, "must be numeric for a scatterplot."))
  }

  if (!is.numeric(df[[y_var]])) {
    stop(paste(y_var, "must be numeric for a scatterplot."))
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_point(alpha = 0.3, color = "purple") +
    ggplot2::geom_smooth(method = "lm", color = "darkviolet") +
    ggplot2::labs(
      x = x_var,
      y = y_var,
      title = paste(x_var, "vs", y_var)
    ) +
    ggplot2::theme_minimal()
}

