#' Summarize Sleep Hours
#'
#' @description Returns mean, median, and standard deviation of student
#'   sleep hours.
#' @param df A cleaned data frame with a \code{sleep_hours} column.
#' @return A one-row data frame with mean, median, SD, and count.
#' @importFrom stats cor median sd
#' @export
#' @examples
#' sleep_summary(student_data)
sleep_summary <- function(df) {
  if (!is.data.frame(df)) {
    stop("Please input a data frame.")
  }

  if (!"Sleep_Hours" %in% names(df)) {
    stop("Data must contain a Sleep_Hours column.")
  }

  if (any(is.na(df$Sleep_Hours))) {
    message("Missing values detected; calculating summary using available data.")
  }
  data.frame(
    mean_sleep = mean(df$Sleep_Hours, na.rm = TRUE),
    median_sleep = median(df$Sleep_Hours, na.rm = TRUE),
    sd_sleep = sd(df$Sleep_Hours, na.rm = TRUE)
  )
}

#' Analyze relationship between sleep and study time
#'
#' @description Returns correlation coefficient between hours of sleep and hours spent studying, based on observations in a dataset.
#' @param df A dataframe
#' @return Correlation coefficient and interpretation
#' @export
sleep_study_relationship <- function(df) {

  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }

  required_cols <- c("Sleep_Hours", "Hours_Studied")

  if (!all(required_cols %in% names(df))) {
    stop("Data must contain Sleep_Hours and Hours_Studied columns.")
  }

  if (!is.numeric(df$Sleep_Hours)) {
    stop("Sleep_Hours must be numeric.")
  }

  if (!is.numeric(df$Hours_Studied)) {
    stop("Hours_Studied must be numeric.")
  }

  if (all(is.na(df$Sleep_Hours)) || all(is.na(df$Hours_Studied))) {
    stop("Columns cannot be entirely missing values.")
  }

  if (any(is.na(df$Sleep_Hours)) || any(is.na(df$Hours_Studied))) {
    warning("Missing values detected; using complete observations only.")
  }

  message("Calculating correlation between sleep and study hours...")
  r <- cor(df$Sleep_Hours, df$Hours_Studied, use = "complete.obs")

  interpretation <- if (r > 0.5) "strong positive correlation"
  else if (r > 0.2)  "moderate positive correlation"
  else if (r > -0.2) "little to no correlation"
  else if (r > -0.5) "moderate negative correlation"
  else               "strong negative correlation"

  list(correlation = r, interpretation = interpretation)
}





