#' Create sample data for the inquisitive
#'
#' A function to create a set of data useful for testing and evaluating inquisitive tools.
#'
#' @param seed an integer used to set the random number generator state.
#' @return A tibble.
#' @export
#'
#' @examples
#' create_suspicious_data()
create_suspicious_data <- function(seed = set.seed(23)){
  . <- key_date <- NULL # satisfy R CMD check issue with NSE
  set.seed(seed)

  ds <- ggplot2::diamonds %>%
    mutate(id = row_number(),
           key_date = as.Date(as.character(20190101), format = "%Y%m%d") +
             round(stats::runif(nrow(.)) * 5 * 365)) %>%
    relocate(id, key_date)

  ds[1:20, "cut"] <- NA
  ds[40:100, "price"] <- NA
  ds[10:60, "table"] <- NA
  ds$color <- ifelse(stats::runif(nrow(ds)) > 0.33, NA, ds$color)
  ds$x <- ifelse(stats::runif(nrow(ds)) > 0.25, NA, ds$x)
  ds$y <- ifelse(stats::runif(nrow(ds)) > 0.5, NA, ds$y)
  ds$z <- ifelse(stats::runif(nrow(ds)) > 0.95, NA, ds$z)
  ds$depth[c(1:20000, 25000:30000)] <- NA
  ds[ds$key_date < "2020-01-01", "depth"] <- NA
  ds[ds$key_date > "2022-01-01", "clarity"] <- NA
  ds
}

