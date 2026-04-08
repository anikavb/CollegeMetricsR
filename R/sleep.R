#' Summarize Sleep Hours
#'
#' @description Returns mean, median, and standard deviation of student
#'   sleep hours.
#' @param df A cleaned data frame with a \code{sleep_hours} column.
#' @return A one-row data frame with mean, median, SD, and count.
#' @export
#' @examples
#' sleep_summary(student_data)
sleep_summary <- function(df) {
  data.frame(
    mean_sleep = mean(df$Sleep_Hours, na.rm = TRUE),
    median_sleep = median(df$Sleep_Hours, na.rm = TRUE),
    sd_sleep = sd(df$Sleep_Hours, na.rm = TRUE)
  )
}

#' Analyze relationship between sleep and study time
#'
#' @param df A dataframe
#' @return Correlation coefficient
#' @export
sleep_study_relationship <- function(df) {
  cor(df$Sleep_Hours, df$Hours_Studied, use = "complete.obs")
}
