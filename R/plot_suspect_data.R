plot_suspect_rows <- function(suspect_data) {
  type <- data <- row_count <- missing_elements <- present_elements <- NULL
  reason <- present <- rows <- missingness <-

    axis_expansion <- 2

  suspect_row_plot_data <-
    suspect_data %>%
    filter(type == "row") %>%
    rowwise() %>%
    mutate(
      row_count = as.numeric(nrow(data)),
      missing_elements = as.numeric(sum(is.na(data))),
      present_elements = as.numeric(sum(!is.na(data))),
      missing = row_count * missing_elements / (missing_elements + present_elements),
      present = row_count * present_elements / (missing_elements + present_elements)
    ) %>%
    ungroup() %>%
    select(-data) %>%
    mutate(reason = forcats::fct_reorder(reason, row_count, .desc = F)) %>%
    arrange(desc(row_count))

  suspect_row_plot_data %>%
    tidyr::pivot_longer(cols = c(missing, present),
                        names_to = "missingness",
                        values_to = "rows") %>%
    ggplot() +
    geom_col(aes(x = reason, y = rows, fill = missingness)) +
    scale_x_discrete(
      expand = expansion(add = c(axis_expansion, 0)),
      position = "top"
    ) +
    ggplot2::scale_fill_manual(name = "Data elements",
                               values = c("missing" = "#FFEA46",
                                          "present" =  "#575C6D")
    ) +
    labs(
      x = "Rows removed",
      y = "Count of rows removed") +
    coord_flip() +
    theme_minimal() +
    theme(aspect.ratio = 1,
          legend.position = "none",
          axis.title.x.bottom = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))

          # legend.position = "none",
          # axis.title.y = element_blank()
    )
}
# plot_suspect_rows(suspect_data)

plot_suspect_columns <- function(suspect_data) {
  type <- data <- column_name <- row_count <- missing_elements <- NULL
  present_elements <- present <- rows <- missingness <- reason <- NULL


  axis_expansion <- 2

  suspect_column_plot_data <-
    suspect_data %>%
    filter(type == "column") %>%
    select(-type) %>%
    # group_by(reason) %>%
    mutate(
      column_name = purrr::map(data, ~ names(.)),
    ) %>%
    tidyr::unnest(column_name) %>%
    mutate(
      row_count = purrr::map2_dbl(data, column_name, ~ nrow(.x[.y])),
      missing_elements = purrr::map2_dbl(data, column_name, ~ sum(is.na(.x[.y]))),
      present_elements = purrr::map2_dbl(data, column_name, ~ sum(!is.na(.x[.y]))),
      missing = row_count * missing_elements / (missing_elements + present_elements),
      present = row_count * present_elements / (missing_elements + present_elements)
    ) %>%
    ungroup() %>%
    select(-data) %>%
    mutate(column_name = forcats::fct_reorder(column_name, present, .desc = FALSE)) %>%
    arrange(desc(present))

  suspect_column_plot_data %>%
    tidyr::pivot_longer(cols = c(missing, present),
                        names_to = "missingness",
                        values_to = "rows") %>%
    ggplot() +
    geom_col(aes(x = as.numeric(column_name), y = rows, fill = missingness)) +
    # ggtext::geom_richtext(aes(x = column_name, y = 1, label = reason),
    #                       angle = 90,
    #                       hjust = 0,
    #                       vjust = 0.5,
    #                       nudge_y = 0.01 * max(suspect_column_plot_data$row_count, na.rm = TRUE),
    #                       size = 3,
    #                       color = "black",
    #                       fill = "white") +


    # scale_x_discrete(expand = expansion(add = c(axis_expansion, 0)),
  #                  position = "top"
  #                  ) +

  scale_x_continuous(expand = expansion(add = c(axis_expansion, 0)),
                     position = "top",
                     breaks = unique(as.numeric(suspect_column_plot_data$column_name)),
                     labels = unique(suspect_column_plot_data$column_name),
                     sec.axis = dup_axis(
                       labels = suspect_column_plot_data$reason)
  ) +

    ggplot2::scale_fill_manual(
      values = c("missing" = "#FFEA46",
                 "present" =  "#575C6D")
    ) +

    labs(
      x = "Columns removed",
      y = "Count of data elements removed") +
    theme_minimal() +
    theme(aspect.ratio = 1,
          legend.position = "none",
          axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
          axis.text.x.bottom = element_text(angle = 90, vjust = 0.5)
    )
}
# plot_suspect_columns(suspect_data)


plot_suspect_missingness <- function(adjudicated_ds) {

  dataset <- elements <- missingness <- NULL

  synopsis <- list()

  synopsis$original$missing <- adjudicated_ds$original_na_count
  synopsis$original$present <- adjudicated_ds$original_present_count

  synopsis$adjudicated$missing <- adjudicated_ds$adjudicated_na_count
  synopsis$adjudicated$present <- adjudicated_ds$adjudicated_present_count

  missingness_plot_data <-
    bind_rows(synopsis, .id = "dataset") %>%
    tidyr::pivot_longer(cols = -dataset,
                        names_to = "missingness",
                        values_to = "elements")


  missingness_plot_data %>%
    ggplot() +
    geom_col(aes(x = dataset, y = elements, fill = missingness)) +
    # geom_text(aes(x = column_name, y = 1, label = reason),
    #           angle = 90,
    #           hjust = 0,
    #           vjust = 0.5,
    #           nudge_y = 0.01 * max(suspect_column_plot_data$row_count, na.rm = TRUE),
    #           size = 3,
    #           color = "white",) +
    scale_x_discrete(# expand = expansion(add = c(0, 0)),
      position = "top") +
    scale_y_continuous(position = "right", labels = scales::comma) +
    ggplot2::scale_fill_manual(
      values = c("missing" = "#FFEA46",
                 "present" =  "#575C6D")
    ) +

    labs(
      x = "Missingness by pipeline stage",
      y = "Count of data elements") +
    theme_minimal() +
    theme(aspect.ratio = 1,
          legend.position = c(1.4,0.13),

          plot.margin = margin(t = 10,
                               r = 10,
                               b = 10,
                               l = 10),

          # legend.box.spacing = unit(0, "points"),
          # legend.background = element_blank(),
          # legend.spacing.x = unit(1.0, 'cm'),
          # legend.margin = margin(3, .5, .5, .5, "lines"),
          # legend.key.height = unit(1, "lines")
          axis.text.x = element_text(angle = 60, hjust = 0),
          axis.title.x.top = element_text(size = rel(0.8), margin = margin(t = 0, r = 0, b = 10, l = 0)),
          axis.title.y.right = element_text(size = rel(0.8), margin = margin(t = 0, r = 0, b = 0, l = 10))
          # axis.text.x = element_text(angle = 60, hjust = 0.)
          # axis.title.y = element_blank()
    )

}
# plot_suspect_missingness(adjudicated_data)




plot_suspect_area <- function(adjudicated_ds){

  # adjudication_pct <- adjudicated_ds$adjudicated_dim / adjudicated_ds$original_dim
  # adjudication_diff <- adjudicated_ds$original_dim - adjudicated_ds$adjudicated_dim
  adjudication_pct <- adjudicated_ds$adjudicated_pct
  adjudication_diff <- adjudicated_ds$adjudicated_diff

  rejected_data_pct <- round(100 * (1 - prod(adjudication_pct)),1)
  rejected_rows <- format(adjudicated_ds$adjudicated_diff[1], big.mark = ",", scientific = FALSE)
  rejected_columns <- format(adjudicated_ds$adjudicated_diff[2], big.mark = ",", scientific = FALSE)
  retained_data_pct <- round(100 * (prod(adjudication_pct)),1)
  retained_rows <- format(adjudicated_ds$adjudicated_dim[1], big.mark = ",", scientific = FALSE)
  retained_columns <- format(adjudicated_ds$adjudicated_dim[2], big.mark = ",", scientific = FALSE)

  ggplot() +
    # background
    geom_rect(
      aes(xmin = 0, xmax = 1,
          ymin = 0, ymax = 1),
      color = "#B22222",
      fill = "#Fbebeb",
      linewidth = 1,
    ) +
    # rows and columns by percentage
    geom_rect(
      aes(xmin = 0, xmax = adjudication_pct[2],
          ymin = 0, ymax = adjudication_pct[1]),
      color = "black",
      fill = "gray92",
      linewidth = 1
    ) +
    # arrow from 100% of original data to scaled processed data representation
    geom_segment(
      aes(
        x = 1 - (1 - adjudication_pct[2]) * 0.02,
        y = 1 - (1 - adjudication_pct[1]) * 0.02,
        xend = adjudication_pct[2] + (1 - adjudication_pct[2]) * 0.02,
        yend = adjudication_pct[1] + (1 - adjudication_pct[1]) * 0.02,
      ),
      color = "#B22222",
      arrow = arrow(length = unit(0.5, "cm"))
    ) +
    annotate(
      "text",
      label = glue::glue(
        "Rejected data: {rejected_data_pct}%\n",
        "Rows: {rejected_rows}\n",
        "Columns: {rejected_columns}"
      ),
      x = 0.05,
      y = mean(c(adjudication_pct[1], 1)),
      lineheight = 0.9,
      hjust = 0,
      color = "#B22222"
    ) +
    annotate(
      "text",
      label = glue::glue(
        "Retained data: {retained_data_pct}%\n",
        "Rows: {retained_rows}\n",
        "Columns: {retained_columns}"
      ),
      x = 0.05,
      y = adjudication_pct[1] / 2,
      lineheight = 0.9,
      hjust = 0,
      color = "black"
    ) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = 0)) +
    theme_minimal() +
    theme(aspect.ratio = 1,
          legend.position = "none",
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
}
# plot_suspect_area(adjudicated_data)

#' Plot the effects of removing suspected data against the original data set
#'
#' @param suspect_ds a suspect_data object
#' @param adjudicated_ds an adjudicated data object
#'
#' @return
#' @export
#'
#' @examples
#' df <- create_suspicious_data()
#' suspect_data <-
#'   df %>%
#'   filter(cut == "Fair") %>%
#'   add_suspected_rows(reason = "Cut was not good or better")
#'
#' suspect_data <-
#'   df %>%
#'   filter(price < 5000) %>%
#'   add_suspected_rows(reason = "Price was less than 5000", suspect_data)
#'
#' suspect_data <-
#'   df %>%
#'   filter(table < 57) %>%
#'   add_suspected_rows(reason = "Table was less than 57", suspect_data)
#'
#' suspect_data <-
#'   df %>%
#'   select(clarity, y) %>%
#'   add_suspected_columns(reason = "These columns are hinky",
#'                         suspect_data)
#'
#' adjudicated_data <- adjudicate_suspect_data(df, suspect_data)
#'
#' plot_suspect_data(suspect_ds = suspect_data, adjudicated_ds = adjudicated_data)
plot_suspect_data <- function(suspect_ds, adjudicated_ds) {
  suspect_row_plot <- plot_suspect_rows(suspect_ds)
  suspect_missingness_plot <- plot_suspect_missingness(adjudicated_ds)
  suspect_area_plot <- plot_suspect_area(adjudicated_ds)
  suspect_columns_plot <- plot_suspect_columns(suspect_ds)

  # patchwork::wrap_plots(
  suspect_columns_plot +
    patchwork::plot_spacer() +
    suspect_area_plot +
    suspect_row_plot +
    patchwork::inset_element(suspect_missingness_plot,
                             left = 0.25,
                             bottom = 0,
                             right = 1.1,
                             top = 3.2)
  # patchwork::plot_layout(guides = "collect")
}
