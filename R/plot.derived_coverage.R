#' Plot Derived vs Traditional Coverage Over Time
#'
#' This function generates a line plot comparing traditional (`coverage_old`)
#' and derived (`coverage_new`) coverage estimates over time for a single indicator.
#' It supports both national and subnational views.
#'
#' The plot title and y-axis labels are automatically generated from the
#' indicator metadata stored in the input object attributes.
#'
#' @param x A `cd_coverage_trends` object returned by [generate_coverage_data()].
#' @param region (Optional) A region or district name. Required for subnational data,
#'   must be `NULL` for national data.
#' @param ... Additional arguments passed to `ggplot2` layers (not used).
#'
#' @return A ggplot object showing coverage trends.
#'
#' @examples
#' \dontrun{
#' generate_coverage_data(dhis_data, "penta1", 2019) %>%
#'   plot(region = "Nairobi")
#'
#' generate_coverage_data(dhis_data, "rota1", 2019) %>%
#'   plot()
#' }
#'
#' @export
plot.cd_derived_coverage <- function(x, region = NULL, ...) {
  admin_level <- attr_or_abort(x, "admin_level")
  indicator <- attr_or_abort(x, "indicator")
  indicator_label <- str_to_title(indicator)

  # Validate region input logic
  if (admin_level == "national" && !is.null(region)) {
    cd_abort("x" = "{.arg region} must be null in national data.")
  }

  if (admin_level != "national" && is.null(region)) {
    cd_abort("x" = "{.arg region} must not be null in subnational data.")
  }

  # Filter for specified region if applicable
  data <- if (admin_level != "national") {
    x %>% filter(!!sym(admin_level) == region)
  } else {
    x
  }

  # Dynamic title based on admin level
  title_text <- if (admin_level == "national") {
    str_glue("National Coverage Over Time by Denominator")
  } else {
    str_glue("{region} Coverage Over Time for  by Denominator")
  }

  # suffix -> pretty name map (your list)
  suffix_map <- c(
    "penta1derived" = "Penta1-derived",
    "penta1"        = "Penta1",
    "anc1"          = "ANC1",
    "dhis2"         = "DHIS2",
    "un"            = "UN"
  )

  # coverage_old <- paste0("cov_", indicator, "_penta1")
  # coverage_new <- paste0(coverage_old, "derived")
  cov_indicator <- paste0('cov_', indicator)

  cols <- x %>%
    select(year, starts_with(cov_indicator)) %>%
    pivot_longer(cols = -year, names_to = "series", values_to = "value") %>%
    mutate(
      suffix = gsub(paste0("^", cov_indicator, "_?"), "", series),
      series_label = factor(suffix_map[suffix], levels = suffix_map)
    )

  present_labels <- unique(cols$series_label)

  # Determine upper y-axis limit using rounded max
  max_val <- robust_max(cols$value, fallback = 100)
  y_max <- ceiling(max_val / 10) * 10
  y_max <- max(100, y_max)   # ensure at least 100
  base_pal <- c("#009E73","#E69F00","#0072B2","#8A2BE2","#D55E00","#CC79A7","#F0E442","#000000")
  pal <- setNames(base_pal[seq_along(present_labels)], present_labels)

  ggplot(cols, aes(x = year, y = value, colour = series_label, group = series_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "gray70") +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 13),
      expand = expansion(mult = c(0, 0.05)),
      limits = c(0, y_max),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_color_manual(values = pal, name = NULL) +
    labs(
      title = title_text,
      y = str_glue("{indicator_label} Coverage (%)"),
      x = "Year"
    ) +
    cd_plot_theme()
}
