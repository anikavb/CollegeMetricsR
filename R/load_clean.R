#' Load a cleaned student dataset
#' @description This function cleans our in-built dataset and removes any missing values or N/As
#' @return A cleaned dataframe of student data
#' @importFrom stats na.omit
#' @export
load_student_data <- function() {
  na.omit(student_data)
}
