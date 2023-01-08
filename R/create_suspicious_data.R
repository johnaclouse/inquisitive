#' Create sample data for the inquisitive
#'
#' @description
#' A function to create a set of data useful for testing and evauating inquisitive tools.
#' @return A tibble.
#' @export
#'
#' @example
#' create_suspicious_data()
create_suspicious_data <- function(){
  ds <- ggplot2::diamonds %>%
    mutate(id = row_number()) %>%
    relocate(id)

  ds[1:20, "cut"] <- NA
  ds[40:100, "price"] <- NA
  ds[10:60, "table"] <- NA
  ds$color <- ifelse(stats::runif(nrow(ds)) > 0.33, NA, ds$color)
  ds$x <- ifelse(stats::runif(nrow(ds)) > 0.25, NA, ds$x)
  ds$y <- ifelse(stats::runif(nrow(ds)) > 0.5, NA, ds$y)
  ds$z <- ifelse(stats::runif(nrow(ds)) > 0.95, NA, ds$z)

  ds
}
