#' Load a cleaned student dataset
#'
#' @return A cleaned dataframe of student data
#' @importFrom stats na.omit
#' @export
load_student_data <- function() {
  df <- student_data
  df <- na.omit(df)
}
