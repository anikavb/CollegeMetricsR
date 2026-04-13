#' Load student dataset
#'
#' @return A dataframe of student data
#' @importFrom stats na.omit
#' @export
load_student_data <- function() {
  df <- student_data
}

#' Clean student dataset
#'
#' @param df A dataframe
#' @return Cleaned dataframe
#' @export
clean_student_data <- function(df) {
  na.omit(df)
}
