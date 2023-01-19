#' Adjudicate suspected data removing it from the original data set
#'
#' Per the contents of the suspect_data, remove the rows and columns identified.
#'
#' @param ds tibble containing the original data set
#' @param suspect_data a suspect_date object to be applied to the original data
#'   set
#'
#' @return an adjudicated_data object containing the original and adjudicated
#'   data sets and associated metadata.
#' @export
#'
#' @examples
#' original_data <- create_suspicious_data()
#' suspect_data <-
#'   original_data %>%
#'   filter(cut == "Fair") %>%
#'   add_suspected_rows(reason = "Cut was not good or better")
#'
#' suspect_data <-
#'   original_data %>%
#'   filter(price < 5000) %>%
#'   add_suspected_rows(reason = "Price was less than 5000", suspect_data)
#'
#' suspect_data <-
#'   original_data %>% filter(table < 57) %>%
#'   add_suspected_rows(reason = "Table was less than 57", suspect_data)
#'
#' suspect_data <-
#'   original_data %>%
#'   select(clarity, y) %>%
#'   add_suspected_columns(reason = "These columns are hinky", suspect_data)
#'
#' suspect_data <-
#'   original_data %>%
#'   select(depth) %>%
#'   add_suspected_columns(reason = "This column is not deep enough", suspect_data)
#'
#' adjudicated_data <- adjudicate_suspect_data(original_data, suspect_data)
adjudicate_suspect_data <- function(ds, suspect_data){

  type <- data <- NULL

  original_data <- ds
  original_dim <- dim(ds)
  original_na_count <- sum(is.na(ds))
  original_present_count <- sum(!is.na(ds))

  excluded_rows <-
    suspect_data %>%
    filter(type == "row") %>%
    select(data) %>%
    tidyr::unnest(data) %>%
    distinct()

  # remove excluded rows
  ds <- anti_join(
    ds,
    excluded_rows,
    by = names(excluded_rows)
  )

  excluded_columns <-
    suspect_data %>%
    filter(type == "column") %>%
    select(data) %>%
    tidyr::unnest(data) %>%
    names()

  # remove excluded columns
  ds <-
    ds %>%
    select(-any_of(excluded_columns))

  adjudicated_dim = dim(ds)

  list(
    original_data = original_data,
    adjudicated_data = ds,
    original_dim = original_dim,
    original_na_count = original_na_count,
    original_present_count = original_present_count,
    adjudicated_dim = adjudicated_dim,
    adjudicated_na_count = sum(is.na(ds)),
    adjudicated_present_count = sum(!is.na(ds)),
    adjudicated_pct = adjudicated_dim / original_dim,
    adjudicated_diff = original_dim - adjudicated_dim
  )
}
