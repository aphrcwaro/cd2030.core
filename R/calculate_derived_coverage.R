#' Generate Coverage Data with Derived Denominators
#'
#' Calculates trend-adjusted and subnationally-redistributed coverage estimates
#' using the DTP1-derived denominator logic described in CD2030. It estimates
#' coverage over time based on changes in DHIS2 population counts, while preserving
#' subnational proportions from the base year.
#'
#' This allows estimating:
#' - Trends over time in coverage
#' - Subnational inequities
#'
#' @param .data A `cd_population_metrics` object containing indicator values and DHIS2 population.
#' @param indicator A character string specifying the indicator to calculate coverage for.
#' @param survey_year Integer. The year from which denominator proportions are derived.
#'
#' @return A `cd_derived_coverage` tibble with columns for old and new coverage estimates.
#'
#' @examples
#' calculate_derived_coverage(dhis_data, "penta1", 2019)
#'
#' @export
calculate_derived_coverage <- function(.data, indicator) {
  check_cd_population(.data)
  indicator <- arg_match(indicator, get_all_indicators())

  population <- attr_or_abort(.data, 'population')
  admin_level <- attr_or_abort(.data, 'admin_level')
  region <- attr_or_null(.data, 'region')

  group_vars <- get_admin_columns(admin_level, region)
  penta1_denom <- get_population_column(indicator, 'penta1')
  penta1_derived_denom <- paste0(penta1_denom, 'derived')
  cov_penta1 <- paste0('cov_', indicator, '_penta1')
  cov_anc1 <- paste0('cov_', indicator, '_anc1')
  cov_dhis2 <- paste0('cov_', indicator, '_dhis2')
  cov_un <- paste0('cov_', indicator, '_un')
  cov_penta1_derived <- paste0(cov_penta1, 'derived')

  data <- .data %>%
    select(any_of(c(group_vars, 'year', population, 'population_yoy_change',
                    'population_survey_change', 'population_proportion', indicator,
                    penta1_denom, penta1_derived_denom,
                    cov_penta1, cov_anc1, cov_dhis2, cov_un, cov_penta1_derived)))

  # Return final tibble tagged with admin level
  new_tibble(
    data,
    class = "cd_derived_coverage",
    admin_level = admin_level,
    indicator = indicator
  )
}
