#' Plot Adjusted vs. Unadjusted Data for Health Indicators
#'
#' `plot.cd_adjustment_values_filtered` creates a bar plot to compare the unadjusted (raw)
#' and adjusted values of health indicators over time. It allows users to specify
#' the indicator prefix and customize legend labels for flexibility across different
#' health data.
#'
#' @param x A data frame containing the `year` column and columns for the raw
#'   and adjusted values of health indicators (e.g., `ideliv_raw`, `ideliv_adj`).
#' @param indicator A character string specifying the prefix of the indicator to
#'   plot (e.g., `"ideliv"`). Only the provided indicators will be plotted.
#' @param title A character string for the plot title. If `NULL`, a default title
#'   based on the indicator is generated.
#' @param legend_labels A character vector of length 2 specifying custom labels
#'   for the legend. The first element is used for the unadjusted (raw) data, and
#'   the second element is for the adjusted data. If `NULL`, default labels
#'   ("N of `indicator` before adjustment" and "N of `indicator` after adjustment")
#'   are generated.
#' @param ... Additional arguments (currently not used).
#'
#' @return A ggplot2 object showing the comparison of unadjusted and adjusted data
#'   for the specified indicator over time.
#'
#' @details
#' This function helps visualize the difference between raw and adjusted values
#' of a given health indicator, aiding in the assessment of data completeness and
#' adjustments. The difference and percentage difference are calculated within
#' the function but are not directly shown on the plot. Instead, the plot shows
#' the actual unadjusted and adjusted values side-by-side for each year.
#'
#' @examples
#' \dontrun{
#' # Using default legend labels and title
#' plot.cd_adjustment_values_filtered(adjustments, indicator = "ideliv")
#'
#' # Custom legend labels and title
#' plot.cd_adjustment_values_filtered(adjustments,
#'   indicator = "instlivebirths",
#'   title = "Customized Title",
#'   legend_labels = c("Original", "Modified")
#' )
#' }
#'
#' @export
plot.cd_adjustment_values_filtered <- function(x,
                                               title = NULL,
                                               legend_labels = NULL,
                                               ...) {
  year = perc_diff = type = value = NULL

  indicator <- attr_or_abort(x, 'indicator')

  # Set default title if not provided
  if (is.null(title)) {
    title <- paste("Comparison of number of", indicator, "before and after adjustment for completeness and outliers")
  }

  # Set default legend labels if not provided
  if (is.null(legend_labels)) {
    legend_labels <- c(
      paste("N of", indicator, "before adjustment"),
      paste("N of", indicator, "after adjustment")
    )
  }

  # Prepare data with absolute and percentage difference columns
  x %>%
    pivot_longer(-year, names_to = "type", values_to = "value") %>%
    mutate(
      type = factor(type,
        levels = c(paste0(indicator, "_raw"), paste0(indicator, "_adj")),
        labels = legend_labels
      )
    ) %>%
    ggplot(aes(x = year, y = value, fill = type)) +
    geom_col(position = "dodge") +
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    scale_y_continuous(labels = scales::number_format(), breaks = scales::pretty_breaks(n = 10)) +
    scale_fill_manual(values = c("darkgreen", "darkgoldenrod3"), name = "Data Type") +
    cd_plot_theme()
}
