#' Constructor function for suspected data tibble structure
#'
#' Suspected data identified by row or column is stored in a common structure
#' supporting independent collection of suspicious data prior to final adjudication.
#'
#' @return A tibble structured to support suspecting data
#' @export
#'
#' @examples
#' suspect_data_constructor()
suspect_data_constructor <- function() {
  tibble(
    type = factor(character(), levels = c("row", "column")),
    reason = character(),
    data =  list())
}


#' Identify rows of a data set to be marked as suspicious
#'
#' Given a common tibble of interest, a subset of the data can be identified as
#' suspicious and added to the suspect_data structure for analysis and
#' visualization.
#'
#' @param x a subset of rows from the tibble of interest
#' @param reason a character string describing why this particular subset is
#'   suspect
#' @param suspect_data a specifically structure tibble
#'
#' If suspect_data is NULL, a new suspect_data tibble will be constructed and
#' the identified data added to that structure.
#'
#' @return a tibble with the suspect data structure
#' @export
#'
#' @examples
#' df <- create_suspicious_data()
#' suspect_data <-
#'   df %>%
#'   filter(cut == "Fair") %>%
#'   add_suspected_rows(reason = "Cut was not good or better")
#'
# suspect_data <-
#   df %>%
#   filter(price < 5000) %>%
#   add_suspected_rows(reason = "Price was less than 5000", suspect_data)
#
# suspect_data <-
#   df %>%
#   filter(table < 57) %>%
#   add_suspected_rows(reason = "Table was less than 57", suspect_data)
add_suspected_rows <- function(x,
                              reason,
                              suspect_data = NULL){

  if (is.null(suspect_data)) suspect_data <- suspect_data_constructor()

  suspect_data %>%
    add_row(tibble::tibble_row(
      type = "row",
      reason = reason,
      data = list(x)))
}

#' Identify column(s) to be marked as suspicious
#'
#' Given a common tibble of interest, a subset of the data can be identified as
#' suspicious and added to the suspect_data structure for analysis and
#' visualization.
#'
#' @param x a character string containin the names of suspect columns from the tibble of interest
#' @param reason a character string describing why this particular subset is
#'   suspect
#' @param suspect_data a specifically structure tibble
#'
#' If suspect_data is NULL, a new suspect_data tibble will be constructed and
#' the identified data added to that structure.
#'
#' @return a tibble with the suspect data structure
#' @export
#'
#' @examples
#' df <- create_suspicious_data()
#' suspect_data <-
#' df %>%
#'   select(clarity, y) %>%
#'   add_suspected_columns(reason = "These columns are hinky")
#'
#' suspect_data <-
#'   df %>%
#'   select(depth) %>%
#'   add_suspected_columns(reason = "This column is not deep enough", suspect_data)
add_suspected_columns <- function(x,
                                 reason,
                                 suspect_data = NULL){

  if (is.null(suspect_data)) suspect_data <- suspect_data_constructor()

  suspect_data %>%
    add_row(tibble::tibble_row(
      type = "column",
      reason = reason,
      data = list(x)))
}
