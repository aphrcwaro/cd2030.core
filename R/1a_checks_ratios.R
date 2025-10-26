#' Calculate Yearly Indicator Ratios Summary with Expected Ratios
#'
#' This function computes yearly summaries for specified indicator ratios,
#' assessing trends across years and providing both actual and expected ratios
#' for each indicator. It evaluates specified indicator pairs to calculate
#' ratios and compares actual values with expected ratios, based on predefined
#' survey coverage values. The summary facilitates monitoring consistency of
#' key health indicators over time.
#'
#' @param .data A data frame containing yearly data for specified indicators.
#'   Expected columns include `district`, `year`, and additional numeric columns
#'   for health indicators, such as `anc1` and `penta1`.
#' @param survey_coverage A named vector of assumed coverage rates for each indicator.
#'   Default is `c(anc1 = 0.98, penta1 = 0.97, penta3 = 0.89, opv1 = 0.97, opv3 = 0.78, pcv1 = 0.97, rota1 = 0.96)`.
#' @param anc1_penta1_mortality A numeric multiplier applied specifically to the `"ratioAP"`
#'   ratio of `anc1` to `penta1`, to account for assumed mortality between `anc1` and `penta1`.
#'   Default is `1.07`.
#' @param ratio_pairs A named list specifying indicator pairs for which ratios
#'   should be calculated. Format: `list("ratioName" = c("numerator", "denominator"))`.
#'   Default pairs include:
#'   - `"ratioAP"`: Ratio of `anc1` to `penta1`, adjusted with `anc1_penta1_mortality`.
#'   - `"ratioPP"`: Ratio of `penta1` to `penta3`.
#'   - `"ratioOO"`: Ratio of `opv1` to `opv3`.
#'   - `"ratioPPcv"`: Ratio of `penta1` to `pcv1`.
#'   - `"ratioPR"`: Ratio of `penta1` to `rota1`.
#' @param adequate_range A numeric vector of length 2 defining the acceptable
#'   range for adequacy checks. Ratios within this range are marked adequate;
#'   outside this range, inadequate. Default is `c(1, 1.5)`.
#' @param region Optional. Restrict analysis to one region (`adminlevel_1`).
#'
#' @return A `cd_ratios_summary` object, a data frame containing:
#'   - **`year`**: Year of each calculated ratio, with an additional row `"Expected Ratio"`
#'     to represent expected values for each indicator ratio based on `survey_coverage`.
#'   - **Indicator Ratios**: Columns for each ratio, showing actual yearly values
#'     and an expected value row based on `survey_coverage`.
#'   - **Additional Columns**: Health indicator values for each year.
#'
#' @details
#' The function provides insights into indicator relationships over time and evaluates
#' deviations from expected values. It enables data quality checks and trend analysis
#' by summarizing ratios yearly and including expected values for comparison. The `"Expected Ratio"`
#' row incorporates predefined survey coverage and serves as a reference for assessing
#' the adequacy of each indicator's ratio.
#'
#' @examples
#' \dontrun{
#'   # Basic usage with default parameters
#'   calculate_ratios_summary(cd_data)
#'
#'   # Custom survey coverage and mortality adjustments for "ratioAP"
#'   calculate_ratios_summary(cd_data,
#'     survey_coverage = c(anc1 = 0.95, penta1 = 0.92, penta3 = 0.85),
#'     anc1_penta1_mortality = 1.05
#'   )
#' }
#'
#' @export
calculate_ratios_summary <- function(.data,
                                     survey_coverage = c(anc1 = 0.98, penta1 = 0.97, penta3 = 0.89, opv1 = 0.97, opv3 = 0.78, pcv1 = 0.97, rota1 = 0.96),
                                     anc1_penta1_mortality = 1.07,
                                     ratio_pairs = list(
                                       "ratioAP" = c("anc1", "penta1"),
                                       "ratioPP" = c("penta1", "penta3")
                                     ),
                                     adequate_range = c(1, 1.5),
                                     region = NULL) {
  year <- NULL

  diff <- setdiff(unique(list_c(ratio_pairs)), names(survey_coverage))
  if (length(diff) > 0) {
    cd_abort(
      c("x" = "Please provide the survey coverage for all indicators in the {.arg ratio_pairs}")
    )
  }

  ratios <- map(names(ratio_pairs), ~ {
    pair <- ratio_pairs[[.x]]
    numerator <- pair[1]
    denominator <- pair[2]

    ratio <- survey_coverage[numerator] / survey_coverage[denominator]
    if (.x == "ratioAP") {
      ratio <- ratio * anc1_penta1_mortality
    }

    return(ratio)
  })

  names(ratios) <- names(ratio_pairs)

  ratios <- ratios %>%
    as_tibble() %>%
    rename_with(
      ~ map_chr(.x, function(name) {
        pair <- ratio_pairs[[name]]
        paste0("Ratio ", pair[1], "/", pair[2])
      }),
      starts_with("ratio")
    ) %>%
    mutate(
      year = "Expected Ratio"
    )

  data_summary <- calculate_ratios_and_adequacy(.data, ratio_pairs, adequate_range, region = region) %>%
    select(-starts_with("%")) %>%
    mutate(
      year = as.character(year)
    ) %>%
    bind_rows(ratios)

  new_tibble(data_summary, class = "cd_ratios_summary")
}

#' Calculate District Adequacy Summary
#'
#' This function calculates and summarizes the adequacy of specified indicator
#' ratios by year, evaluating the consistency of key health indicators across
#' districts. It computes the percentage of districts meeting the adequacy criteria
#' for each indicator ratio, based on the specified range.
#'
#' @param .data A data frame containing indicator data by district and year.
#'   The data frame should include all indicators specified in `ratio_pairs`.
#' @param ratio_pairs A named list where each element represents a pair of indicators
#'   for ratio calculation. The default pairs are:
#'   - `"ratioAP"`: ANC1 to PENTA1 (i.e., `anc1/penta1`)
#'   - `"ratioPP"`: PENTA1 to PENTA3 (i.e., `penta1/penta3`)
#'   - `"ratioOO"`: OPV1 to OPV3 (i.e., `opv1/opv3`)
#' @param adequate_range A numeric vector of length 2 that specifies the lower
#'   and upper bounds for adequacy. Ratios within this range will be flagged as adequate.
#'   Default is `c(1, 1.5)`.
#' @param region Optional. Restrict analysis to one region (`adminlevel_1`).
#'
#' @return A tibble of class `cd_district_ratios_summary`, containing the summary of
#'   adequacy checks by year. Each column represents the percentage of districts
#'   meeting the adequacy criteria for the specified ratio, with overall metrics
#'   included for data interpretation and quality monitoring.
#'
#' @details
#' - The function dynamically calculates ratios between the specified indicator pairs.
#' - For each year, it computes the percentage of districts where the calculated ratio
#'   for each indicator pair falls within the adequate range.
#' - The summary table aids in monitoring indicator consistency across districts and
#'   over time, providing insights into areas where health service delivery may vary.
#'
#' @examples
#' \dontrun{
#' # Calculate adequacy summary for ANC1/Penta1 and other indicator ratios
#' calculate_district_ratio_summary(cd_data)
#' }
#'
#' @export
calculate_district_ratios_summary <- function(.data,
                                              ratio_pairs = list(
                                                "ratioAP" = c("anc1", "penta1"),
                                                "ratioPP" = c("penta1", "penta3"),
                                                "ratioOO" = c("opv1", "opv3"),
                                                "ratioPPcv" = c("penta1", "pcv1"),
                                                "ratioPR" = c("penta1", "rota1")
                                              ),
                                              adequate_range = c(1, 1.5),
                                              region = NULL) {
  data_summary <- calculate_ratios_and_adequacy(.data, ratio_pairs, adequate_range, region = region) %>%
    select(-starts_with("Ratio"))

  new_tibble(data_summary, class = "cd_district_ratios_summary")
}


#' Calculate Indicator Ratios and Adequacy Flags
#'
#' Computes ratios for specified pairs of indicators dynamically and flags whether
#' each calculated ratio falls within an adequate range, per district and year.
#' This function is an intermediate utility used to standardize ratio calculations
#' and adequacy checks across indicator pairs.
#'
#' @param .data A data frame of type `cd_data`.
#' @param ratio_pairs A named list of indicator pairs for which ratios should be
#'   calculated.
#'   - Each pair should be a character vector of length two, with the first
#'     element as the numerator and the second as the denominator. Default is:
#'     `list("ratioAP" = c("anc1", "penta1"), "ratioPP" = c("penta1", "penta3"), "ratioOO" = c("opv1", "opv3"))`.
#'   - Each name in the list becomes the name of the resulting ratio column.
#' @param adequate_range A numeric vector of length two specifying the inclusive
#'   lower and upper bounds for the adequate range, typically between `1` and
#'   `1.5`. Ratios within this range are flagged as adequate.
#' @param region Optional. Restrict analysis to one region (`adminlevel_1`).
#'
#' @return A data frame with columns for:
#' - **Summed indicators by district and year**: Summed values for each indicator
#'    across districts within the specified `year`, based on input in `ratio_pairs`.
#' - **Ratios**: Each ratio is calculated as the sum of the specified numerator
#'    divided by the sum of the specified denominator for each pair.
#' - **Adequacy flags**: For each ratio, an adequacy flag (e.g., `adeq_ratioAP`)
#'    is generated, set to `1` if the ratio falls within the `adequate_range`,
#'    and `0` otherwise.
#' - The output is aggregated by year and includes means of ratios and adequacy
#'    flags.
#'
#' @details
#' This function standardizes ratio calculations by dynamically computing ratios
#' and adequacy flags for specified pairs of indicators. It operates in three
#' main stages:
#' 1. **Summing Indicators**: Calculates the sum of each indicator by `district`
#'    and `year`.
#' 2. **Calculating Ratios**: Divides the summed numerator by the summed
#'    denominator for each pair in `ratio_pairs`, creating a column for each ratio.
#' 3. **Checking Adequacy**: Flags each ratio as adequate if it falls within the
#'    specified `adequate_range`, returning `1` for adequate and `0` otherwise.
#'
#' @examples
#' \dontrun{
#' # Calculate ratios and adequacy checks using custom ranges
#' calculate_ratios_and_adequacy(data,
#'   ratio_pairs = list(
#'     "ratioAP" = c("anc1", "penta1"),
#'     "ratioPP" = c("penta1", "penta3")
#'   ),
#'   adequate_range = c(1, 1.5)
#' )
#' }
#'
#' @noRd
 calculate_ratios_and_adequacy <- function(.data,
                                          ratio_pairs = NULL,
                                          adequate_range = c(1, 1.5),
                                          region = NULL) {
  district = year = NULL

  check_cd_data(.data)
  check_required(adequate_range)

  if (is.null(ratio_pairs)) {
    ratio_pairs = list(
      "ratioAP" = c("anc1", "penta1"),
      "ratioPP" = c("penta1", "penta3")
    )

    if (get_selected_group() == 'vaccine') {
      ratio_pairs$ratioOO <- c("opv1", "opv3")
    }
  } else {
    check_ratio_pairs(ratio_pairs)
  }

  # Check that adequate_range is a numeric vector of length 2
  if (!is.numeric(adequate_range) || length(adequate_range) != 2) {
    cd_abort(c("x" = "Adequate range must be a numeric vector of length 2."), call = call)
  }

  all_pairs <- list_c(ratio_pairs)

  data_summary <- .data %>%
    filter(if(!is.null(region)) adminlevel_1 == region else TRUE) %>%
    # Calculate the average of indicators by district and year
    summarise(
      across(all_of(all_pairs), sum, na.rm = TRUE),
      .by = c(adminlevel_1, district, year)
    )

  data_summary <- data_summary %>%
    bind_cols(
      imap_dfc(ratio_pairs, ~ data_summary[[.x[1]]] / data_summary[[.x[2]]] %>% set_names(.y))
    ) %>%
    # Calculate adequacy checks
    mutate(across(names(ratio_pairs), ~ as.integer(.x >= adequate_range[1] & .x <= adequate_range[2]), .names = "adeq_{.col}")) %>%
    # Summarize adequacy checks by year
    summarise(
      across(all_of(all_pairs), sum, na.rm = TRUE),
      across(c(starts_with("adeq_"), starts_with("ratio")), mean, na.rm = TRUE),
      .by = year
    ) %>%
    mutate(
      across(starts_with("adeq_"), ~ round(.x * 100, 1)),
      across(all_of(all_pairs), ~ round(.x, 1))
    ) %>%
    rename_with(
      ~ map_chr(.x, function(name) {
        ratio_name <- str_replace(name, "^adeq_", "")
        pair <- ratio_pairs[[ratio_name]]
        paste0("% district with ", pair[1], "/", pair[2], " in expected ranged")
      }),
      starts_with("adeq_")
    ) %>%
    rename_with(
      ~ map_chr(.x, function(name) {
        pair <- ratio_pairs[[name]]
        paste0("Ratio ", pair[1], "/", pair[2])
      }),
      starts_with("ratio")
    )

  return(data_summary)
}
