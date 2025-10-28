#' Compute Population Metrics for DHIS-2 and UN Data
#'
#' `prepare_population_metrics` calculates demographic metrics such as total
#' population, live births, growth rates, and related indicators using DHIS-2
#' population data. For national-level data, it optionally integrates UN estimates
#' to provide comparative metrics. The function supports aggregation at different
#' administrative levels, including national, adminlevel_1, and district levels.
#'
#' @param .data A `cd_data` tibble containing processed DHIS-2 health facility
#'   data with population indicators. This dataset must include columns for metrics
#'   like `total_pop`, `live_births`, and `pop_rate`.
#' @param admin_level Character. Specifies the administrative level for aggregation.
#'   Available options are: `'national'`, `'adminlevel_1'`, and `'district'`. Default
#'   is `'national'`.
#' @param un_estimates Optional. A tibble containing UN population estimates with
#'   columns for `un_population`, `un_births`, `un_popgrowth`, and related metrics.
#'   This parameter is only required for national-level calculations.
#'
#' @return A tibble of class `cd_population_metrics` containing demographic metrics
#'   for the specified administrative level and years. Metrics include:
#'   - **For all levels**: DHIS-2 metrics such as `totpop_dhis2` (total population)
#'     and `totlivebirths_dhis2` (total live births).
#'   - **For national level**: Both DHIS-2 and UN metrics for comparison.
#'
#' @examples
#' \dontrun{
#' # National-level demographic metrics
#' population_data <- prepare_population_metrics(
#'   .data = dhis2_data,
#'   admin_level = "national",
#'   un_estimates = un_data
#' )
#'
#' # District-level demographic metrics
#' population_data <- prepare_population_metrics(
#'   .data = dhis2_data,
#'   admin_level = "district"
#' )
#' }
#'
#' @export
prepare_population_metrics <- function(.data,
                                       admin_level = c("national", "adminlevel_1", "district"),
                                       un_estimates = NULL,
                                       region = NULL) {
  totlivebirths_dhis2 = total_pop = under5_pop = under1_pop = live_births =
    total_births = women15_49 = pop_rate = adminlevel_1 = district = year =
    pop_dhis2 = livebirths_dhis2 = totpop_dhis2 = totlivebirths_dhis2 = iso3 = NULL

  # Validate inputs
  check_cd_data(.data)

  # Define grouping variables based on the administrative level
  admin_level <- arg_match(admin_level)
  group_vars <- get_admin_columns(admin_level, region)
  group_vars <- c(group_vars, "year")

  check_un_estimates_data(un_estimates, admin_level)


  # Required columns for DHIS-2 data
  columns <- c(
    "district", "adminlevel_1", "year", "total_pop", "under5_pop",
    "under1_pop", "pop_rate", "live_births", "women15_49", "total_births"
  )

  # Prepare DHIS-2 Data
  dhis_data <- .data %>%
    filter(if (is.null(region)) TRUE else adminlevel_1 == region) %>%
    select(any_of(columns)) %>%
    rename(
      pop_dhis2 = total_pop,
      under5_dhis2 = under5_pop,
      under1_dhis2 = under1_pop,
      livebirths_dhis2 = live_births,
      allbirths_dhis2 = total_births,
      wom15_49_dhis2 = women15_49,
      pop_rate_dhis2 = pop_rate
    ) %>%
    arrange(adminlevel_1, district, year) %>%
    distinct(district, adminlevel_1, year, .keep_all = TRUE) %>%
    mutate(
      totpop_dhis2 = sum(pop_dhis2, na.rm = TRUE) / 1000,
      totbirths_dhis2 = sum(allbirths_dhis2, na.rm = TRUE) / 1000,
      totlivebirths_dhis2 = sum(livebirths_dhis2, na.rm = TRUE) / 1000,
      totunder1_dhis2 = sum(under1_dhis2, na.rm = TRUE) / 1000,
      .by = all_of(group_vars)
    ) %>%
    summarise(
      totpop_dhis2 = robust_max(totpop_dhis2),
      totbirths_dhis2 = robust_max(totbirths_dhis2),
      totlivebirths_dhis2 = robust_max(totlivebirths_dhis2),
      totunder1_dhis2 = robust_max(totunder1_dhis2),
      .by = all_of(group_vars)
    )

  # Integrate UN estimates for national-level calculations
  if (admin_level == "national" && !is.null(un_estimates)) {
    combined_data <- dhis_data %>%
      left_join(un_estimates, by = "year") %>%
      rename(un_under1 = un_under1y)
  } else {
    combined_data <- dhis_data
  }

  # Define the desired order for final output
  desired_order <- c(
    "iso3", "adminlevel_1", "district", "year", "un_population",
    "totpop_dhis2", "un_births", 'totbirths_dhis2', "totlivebirths_dhis2", 'un_under1', "totunder1_dhis2", "un_popgrowth"
  )

  # Finalize the data structure
  combined_data <- combined_data %>%
    select(any_of(desired_order)) %>%
    arrange(year)

  # Return the resulting tibble with attributes
  new_tibble(
    combined_data,
    class = "cd_population_metrics",
    admin_level = admin_level,
    country = attr_or_abort(.data, "country")
  )
}
