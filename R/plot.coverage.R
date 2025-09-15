#' Plot National Coverage Data
#'
#' This function generates a line plot to visualize national immunization coverage
#' data across years, allowing comparison between different estimates (e.g., DHIS2
#' estimates, WUENIC estimates, and Survey estimates).
#'
#' @param x A data frame of type `cd_coverage_filtered`, containing year-wise
#'   national coverage data for the specified country and indicator.
#' @param ... Additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @details
#' This plot function transforms the coverage data into a long format suitable for
#' plotting with ggplot2, and extracts the denominator information for display in
#' the plot caption. It shows coverage values for each year, with different lines
#' and points for each estimate type.
#'
#' The plot includes:
#' - **Y-axis**: Coverage percentage.
#' - **X-axis**: Year.
#' - **Line color**: Represents different estimates (e.g., DHIS2, Survey).
#' - **Caption**: Indicates the denominator used in the coverage calculation.
#'
#' This function is meant to be used with the `cd_coverage_filtered` class, which
#' contains coverage values for different indicators and estimates.
#'
#' @return A ggplot object displaying a line plot of the national coverage data
#'   by year, with lines and points representing different estimate types.
#'
#' @examples
#' \dontrun{
#'   plot(filter_coverage(dt_adj, indicator = "bcg", denominator = "anc1"))
#' }
#'
#' @export
plot.cd_coverage_filtered <- function(x, ...) {
  estimates = year = value = `Survey estimates` = `DHIS2 estimate` = `WUENIC estimates` =
    `95% CI LL` = `95% CI UL` = NULL

  admin_level <- attr_or_abort(x, "admin_level")
  # indicator <- attr_or_abort(x, 'indicator')
  # region <- attr_or_null(x, 'region')
  denominator <- attr_or_abort(x, 'denominator')

  if (ncol(x) <= 1) {
    cd_abort(c("x" = "The columns data is empty."))
  }

  data_long <- x %>%
    pivot_longer(cols = -estimates, names_to = "year") %>%
    mutate(year = as.integer(year))

  min_y <- min(data_long$value, na.rm = TRUE)
  min_y <- if (min_y < 0) min_y * 1.05 else 0
  max_y <- robust_max(data_long$value) * 1.05

  data_long <- data_long %>%
    pivot_wider(
      names_from = estimates,
      values_from = value,
      names_repair = "minimal"
    )

  surv_data <- data_long %>%
    filter(!is.na(`Survey estimates`))

  plot <- data_long %>%
    ggplot(aes(x = year)) +
    # Add lines and points for DHIS2 and WUENIC estimates
    geom_line(aes(y = `DHIS2 estimate`, color = "DHIS2 estimate"), size = 1) +
    geom_point(aes(y = `DHIS2 estimate`, color = "DHIS2 estimate"), size = 2)

  if (admin_level == "national") {
    plot <- plot +
      geom_line(aes(y = `WUENIC estimates`, color = "WUENIC estimate"), size = 1) +
      geom_point(aes(y = `WUENIC estimates`, color = "WUENIC estimate"), size = 2)
  }

  # Add Survey estimates
  plot +
    geom_line(data = surv_data, aes(y = `Survey estimates`, color = "Survey estimate"), size = 1) +
    geom_point(aes(y = `Survey estimates`, color = "Survey estimate"), size = 2) +

    # Add error bars for 95% CI
    geom_errorbar(
      aes(ymin = `95% CI LL`, ymax = `95% CI UL`, y = `Survey estimates`, color = "95% CI"),
      width = 0.2,
      na.rm = TRUE
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(min_y, max_y), breaks = scales::pretty_breaks(11)) +
    scale_x_continuous(breaks = scales::pretty_breaks(5)) +
    scale_color_manual(
      values = c("Survey estimate" = "royalblue1", "95% CI" = "royalblue1", "DHIS2 estimate" = "forestgreen", "WUENIC estimate" = "gold")
    ) +
    labs(
      y = "Coverage (%)",
      x = "Year",
      caption = paste0("Denominators derived from ", denominator, " estimates")
    ) +
    cd_plot_theme() +
    theme(
      panel.grid.major.y = element_line(colour = "lightblue1", linetype = "dashed"),
      panel.grid.major.x = element_line(colour = "gray90", linetype = "dashed"),
    )
}
