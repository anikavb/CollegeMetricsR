#' Summarize Key Student Variables
#' @description Provides summary statistics for selected numeric variables
#'   in the CollegeMetricsR student dataset. If no variables are selected,
#'   the function summarizes key default variables.
#' @param df A data frame from the CollegeMetricsR dataset.
#' @param vars Optional character vector of variables to summarize. If NULL,
#'   summarizes Hours_Studied, Sleep_Hours, Exam_Score, and gpa.
#' @return A data frame with summary statistics for each variable.
#' @importFrom stats median sd
#' @export
#' @examples
#' df <- scale_exam_score(student_data)
#' lifestyle_summary(df)
#' lifestyle_summary(df, vars = c("Hours_Studied", "Sleep_Hours"))

lifestyle_summary <- function(df, vars = NULL) {

  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }

  default_vars <- c("Hours_Studied", "Sleep_Hours", "Exam_Score", "gpa")

  if (is.null(vars)) {
    vars <- default_vars
  }

  missing_vars <- vars[!vars %in% names(df)]

  if (length(missing_vars) > 0) {
    stop("These variables are missing from the data: ",
         paste(missing_vars, collapse = ", "))
  }

  non_numeric <- vars[!sapply(df[vars], is.numeric)]

  if (length(non_numeric) > 0) {
    stop("These variables must be numeric to summarize: ",
         paste(non_numeric, collapse = ", "))
  }

  summarize_var <- function(var) {
    x <- df[[var]]

    data.frame(
      variable  = var,
      mean      = round(mean(x, na.rm = TRUE), 2),
      median    = round(median(x, na.rm = TRUE), 2),
      sd        = round(sd(x, na.rm = TRUE), 2),
      min       = round(min(x, na.rm = TRUE), 2),
      max       = round(max(x, na.rm = TRUE), 2),
      n_missing = sum(is.na(x))
    )
  }

  results_list <- lapply(vars, summarize_var)

  results <- do.call(rbind, results_list)

  return(results)
}

#' Analyze Correlation Between Two Variables
#'
#' @description Calculates the correlation between any two numeric
#'   variables in the student dataset and provides a simple interpretation.
#' @param df A data frame from the CollegeMetricsR dataset.
#' @param var1 First variable (character).
#' @param var2 Second variable (character).
#' @return A list with the correlation and interpretation.
#' @export
#' @examples
#' student_correlation(df, "Sleep_Hours", "Hours_Studied")
student_correlation <- function(df, var1, var2) {

  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }

  if (!var1 %in% names(df) || !var2 %in% names(df)) {
    stop("Both variables must exist in the dataset.")
  }

  if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) {
    stop("Both variables must be numeric.")
  }

  if (var1 == var2) {
    stop("Choose two different variables.")
  }

  if (any(is.na(df[[var1]])) || any(is.na(df[[var2]]))) {
    warning("Missing values detected; using complete observations.")
  }

  message("Calculating correlation between ", var1, " and ", var2, "...")

  r <- cor(df[[var1]], df[[var2]], use = "complete.obs")

  interpretation <- if (r > 0.5) "strong positive relationship"
  else if (r > 0.2)  "moderate positive relationship"
  else if (r > -0.2) "little to no relationship"
  else if (r > -0.5) "moderate negative relationship"
  else               "strong negative relationship"

  list(
    correlation = round(r, 2),
    interpretation = interpretation
  )
}
