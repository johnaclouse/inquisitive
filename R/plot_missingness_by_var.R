#' Plot missingness by variable
#'
#' Build a ggplot depicting missingness in a data frame. The data is first
#' turned into a lithographic copy of the frame with only 'missing' or 'present'
#' by \code{\link{as_lith_tbl}}.
#'
#' If the data set contains more than 5,000 rows, the data will be aggregated
#' and the percentage of missing data per aggregated element will be reported.
#'
#' If a reference column is specified, the data frame will be sorted by the
#' reference column prior to plotting
#'
#' @param ds a data frame to be plotted
#' @param reference_column an unquoted column name
#' @param number_of_axis_lables an integer specifying maximum number of labels
#'   for the x axis
#' @param date_format_fun a function specifying the formatting of of the x axis
#'   when the column selected is of type Date.
#'
#' @return
#' @export
#'
#' @examples
#' ds <- inquisitive::create_suspicious_data()
#' plot_missingness_by_var(ds, reference_column = key_date)
#' plot_missingness_by_var(ds, reference_column = key_date, date_format_fun =
#'  function(x) {format(x, "%b- %Y")})
#' plot_missingness_by_var(ds, reference_column = carat)
#' plot_missingness_by_var(ds)
plot_missingness_by_var <- function(ds,
                                    reference_column = NULL,
                                    number_of_axis_lables = 25,
                                    date_format_fun = function(x) {format(x, "%Y-%m")}) {

  . <- column <- row_identifier <- name <- value <- NULL

  number_of_axis_lables <- pmin(number_of_axis_lables,
                                nrow(ds))

  column_order <-
    purrr::map_df(ds, ~ sum(is.na(.))) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "column",
                        values_to = "count") %>%
    arrange(count) %>%
    pull(column)

  ds <- as_lith_tbl(ds, {
    {
      reference_column
    }
  })

  subject_label <- (rlang::enexpr(reference_column))

  if (is.null(subject_label)) {
    subject_label <- "row identifier"
  } else {
    ds <- ds %>%
      select(-{{subject_label}})
  }

  ds <-
    ds %>%
    mutate(across(where(is.factor), ~ if_else(. == "present", 0, 1)))

  if (nrow(ds) > 5000) {
    observations_per_element <-  ceiling(nrow(ds) / 5000)
    ds <-
      ds %>%
      arrange(reference_column) %>%
      group_by(row_identifier = row_number() %/% observations_per_element) %>%
      summarize(across(everything(), mean))

    caption <- glue::glue("Darker color indicates greater percentage of missing data\nEach bar represents {observations_per_element} observations")

  } else {
    ds <- ds %>%
      arrange(reference_column) %>%
      mutate(row_identifier = row_number())

    caption <- "Dark color indicates missing data"
  }

  x_axis_labels <-
    ds %>%
    # row_identifier is required here to tie labels to original data
    distinct(row_identifier,
             reference_column) %>%
    arrange(reference_column, row_identifier) %>%
    slice(seq(1, nrow(.), by = floor(nrow(.) / number_of_axis_lables)))

  plot_data <-
    ds %>%
    tidyr::pivot_longer(cols = -c(row_identifier,
                                  reference_column)) %>%
    mutate(name = factor(name, levels = column_order))

  ggplot(data = plot_data,
         aes(
           x = row_identifier,
           y = name,
           fill = value
         )) +
    geom_raster() +
    scale_x_continuous(
      breaks = x_axis_labels$row_identifier,
      labels = x_axis_labels$reference_column
    ) +

    scale_fill_viridis_c(
      option = "E",
      direction = -1,
      limits = c(0,1)
    ) +
    ggplot2::labs(
      title = paste("Missing data in features by", quo_name(subject_label)),
      caption = caption,
      x = subject_label,
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      legend.position = "none",
      panel.grid.minor.x = element_blank()
    )
}







#' Create a copy of a tibble replacing all of the elements with a binary missing
#' or present factor.
#'
#' The function takes a tibble composed of arbitrary data types and creates a
#' copy of the structure but replaces all of the values with either 'present' or
#' 'missing'. Something like a photographic lithograph.
#'
#' In addition to replace the values, two columns are added: row_identifier and
#' reference_column. These are used to allow for the transformed data to be
#' incorporated into subsequent analysis and visualization. If not reference
#' column is provided, the row_identifier column will be used as the reference_column.
#'
#' @param ds a tibble to be developed as a data lithograph
#' @param reference_column an unquoted character string designating a reference
#'   column to be preserved for sorting and future visualization. If no column
#'   is designated, the function will use the row_identifier column.
#'
#' @return a tibble comprised of the original columns containing present/missing
#'   values and the reference column in its original state.
#' @export
#'
#' @examples
#' ds <- create_suspicious_data()
#' as_lith_tbl(ds, reference_column = carat) %>% head()
#' as_lith_tbl(ds, reference_column = key_date) %>% head()
#' as_lith_tbl(ds) %>% head()
as_lith_tbl <- function(ds, reference_column = NULL){

  reference_column <- rlang::enquo(reference_column)


  if (rlang::quo_is_null(reference_column)) {
    ds$reference_column <- seq(1, nrow(ds))
  } else if (rlang::quo_is_symbol(reference_column)) {
    ds$reference_column <- rlang::eval_tidy(ds[[rlang::ensym(reference_column)]])
  } else {
    stop(paste(
      "Expected a symbol for reference_column but found",
      class(rlang::get_expr(reference_column))
    ))
  }

  # Base R solution:
  # if (is.null(substitute(reference_column))) {
  #   ds$reference_column <- ds$row_identifier
  # } else {
  #   ds$reference_column <- ds[[deparse(substitute(reference_column))]]
  # }

  ds %>%
    mutate(across(
      c(everything(), -reference_column),
      ~ factor(if_else(is.na(.), "missing", "present"))
    ))
}

