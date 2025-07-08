#' Calculate Overall Quality Score for Data Quality Metrics
#'
#' This function calculates an overall quality score based on various data
#' quality metrics. It summarizes completeness, outlier presence, and
#' consistency of reporting in immunization health facility data.
#'
#' @param .data A data frame of type `cd_data` containing facility data
#'   including annual reporting rates, completeness, and consistency indicators.
#' @param threshold The data reporting rate threshold.
#' @param ratio_pairs description
#'
#' @details
#' `calculate_overall_score` processes multiple data quality indicators:
#'  - **Completeness metrics**: Percentage of expected reports, districts with
#'    complete reporting, and districts with no missing values.
#'  - **Outlier metrics**: Percentage of monthly values and districts without
#'    extreme outliers.
#'  - **Consistency ratios**: Ratios between different immunization indicators
#'    to ensure internal consistency.
#'
#' The function calculates averages for the selected metrics and includes
#' a row summarizing the annual data quality score.
#'
#' @return A tibble with calculated scores for each metric, including
#'   a summary row for the annual quality score. The result is ordered by
#'   metric codes and ready for reporting in tabular or graphical form.
#'
#' @examples
#' \dontrun{
#' calculate_overall_score(.data = my_data)
#' }
#'
#' @export
calculate_overall_score <- function(.data,
                                    threshold,
                                    ratio_pairs = list(ratioAP = c("anc1", "penta1"), ratioPP = c("penta1", "penta3")),
                                    region = NULL) {

  year = mean_rr = low_mean_rr = mean_mis_vacc_tracer = mean_out_vacc_tracer =
    value = `Data Quality Metrics` = value = no = NULL

  check_cd_data(.data)

  avg_reporting_rate <- calculate_average_reporting_rate(.data, 'adminlevel_1', region = region) %>%
    # select(year, mean_four_rr) %>%
    summarise(mean_four_rr = mean(mean_four_rr, na.rm = TRUE), .by = year) %>%
    pivot_wider(names_from = year, values_from = mean_four_rr) %>%
    mutate(
      `Data Quality Metrics` = "% of expected monthly facility reports (national)",
      no = "1a"
    )

  district_reporting_rate <- calculate_district_reporting_rate(.data, threshold = threshold, region = region) %>%
    select(year, low_mean_four_rr) %>%
    pivot_wider(names_from = year, values_from = low_mean_four_rr) %>%
    mutate(
      `Data Quality Metrics` = paste0("% of districts with completeness of facility reporting >= ", threshold),
      no = "1b"
    )

  district_completeness <- calculate_district_completeness_summary(.data, region = region) %>%
    # select(year, mean_mis_vacc_tracer) %>%
    # pivot_wider(names_from = year, values_from = mean_mis_vacc_tracer) %>%
    select(year, mean_mis_four) %>%
    pivot_wider(names_from = year, values_from = mean_mis_four) %>%
    mutate(
      `Data Quality Metrics` = "% of districts with no missing values for the 4 forms",
      no = "1c"
    )

  outliers <- calculate_outliers_summary(.data, admin_level = 'adminlevel_1', region = region) %>%
    # select(year, mean_out_vacc_tracer) %>%
    # pivot_wider(names_from = year, values_from = mean_out_vacc_tracer) %>%
    # select(year, mean_out_four) %>%
    summarise(mean_out_four = mean(mean_out_four, na.rm = TRUE), .by = year) %>%
    pivot_wider(names_from = year, values_from = mean_out_four) %>%
    mutate(
      `Data Quality Metrics` = "% of monthly values that are not extreme outliers (national)",
      no = "2a"
    )

  outliersd <- calculate_district_outlier_summary(.data, region = region) %>%
    # select(year, mean_out_vacc_tracer) %>%
    # pivot_wider(names_from = year, values_from = mean_out_vacc_tracer) %>%
    select(year, mean_out_four) %>%
    pivot_wider(names_from = year, values_from = mean_out_four) %>%
    mutate(
      `Data Quality Metrics` = "% of districts with no extreme outliers in the year",
      no = "2b"
    )

  adeqratiosd <- calculate_ratios_and_adequacy(.data, ratio_pairs = ratio_pairs, region = region) %>%
    select(year, starts_with("Ratio"), starts_with("% district with")) %>%
    pivot_longer(-year, names_to = "Data Quality Metrics", values_to = "value") %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(
      no = case_match(
        `Data Quality Metrics`,
        "Ratio anc1/penta1" ~ "3a",
        "Ratio penta1/penta3" ~ "3b",
        "% district with anc1/penta1 in expected ranged" ~ "3c",
        "% district with penta1/penta3 in expected ranged" ~ "3d"
      )
    )

  final_data <- bind_rows(
    avg_reporting_rate,
    district_reporting_rate,
    district_completeness,
    outliers,
    outliersd,
    adeqratiosd
  ) %>%
    relocate(no, `Data Quality Metrics`)

  mean_row <- final_data %>%
    filter(no %in% c("1a", "1b", "2a", "2b", "3c", "3d")) %>%
    summarise(across(starts_with("20"), mean, na.rm = TRUE)) %>%
    mutate(
      `Data Quality Metrics` = "Annual data quality score",
      no = "4"
    )

  final_data <- final_data %>%
    bind_rows(mean_row) %>%
    arrange(no)

  return(final_data)
}
