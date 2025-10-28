#' Reporting Rate Summary by Administrative Level
#'
#' `calculate_average_reporting_rate()` computes the average reporting rate for
#' each indicator by year and administrative level. For `adminlevel_1`, you can
#' limit the results to a single region.
#'
#' @param .data A tibble of class `cd_data`.
#' @param admin_level Character. One of `"national"`, `"adminlevel_1"`, or `"district"`.
#' @param region Optional. Name of a specific region (`adminlevel_1`) to filter results.
#'   Only used when `admin_level = "adminlevel_1"`.
#'
#' @return A tibble of class `cd_average_reporting_rate`.
#'
#' @examples
#' \dontrun{
#' calculate_average_reporting_rate(data, admin_level = "adminlevel_1")
#' calculate_average_reporting_rate(data, admin_level = "adminlevel_1", region = "Nairobi")
#' }
#'
#' @export
calculate_average_reporting_rate <- function(.data,
                                             admin_level = c("national", "adminlevel_1", "district"),
                                             region = NULL) {
  . = NULL

  check_cd_data(.data)

  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level, region)
  if (admin_level != 'adminlevel_1' && !is.null(region)) {
    cd_abort(c('x' = 'Region can only be specified for {.arg adminlevel_1}'))
  }

  group_name <- get_indicator_group_names()
  # indicators <- paste0(group_name, "_rr")
  indicators <- paste0(group_name[which(group_name != 'ipd')], "_rr")

  reporting_rate <- .data %>%
    filter(if (!is.null(region)) adminlevel_1 == region else TRUE) %>%
    summarise(across(all_of(indicators), mean, na.rm = TRUE), .by = c(admin_level_cols, "year")) %>%
    mutate(
      mean_rr = rowMeans(select(., indicators), na.rm = TRUE)
    ) %>%
    mutate(across(ends_with("_rr"), round, 0))

  new_tibble(
    reporting_rate,
    class = "cd_average_reporting_rate",
    admin_level = admin_level,
    region = region
  )
}

#' District-Level Reporting Rates by Year
#'
#' `calculate_district_reporting_rate` Calculates the percentage of districts that
#' meet or exceed a specified reporting rate threshold for each indicator, by year.
#' Also computes a yearly average across all indicators.
#'
#' @param .data A tibble of class `cd_data`.
#' @param threshold Minimum reporting rate (%) for a district to be considered
#'   compliant. Default is 90.
#' @param region Optional. Filter results for a specific region (admin level 1 name).
#'
#' @return A tibble of class `cd_district_reporting_rate`.
#'
#' @examples
#' \dontrun{
#'   calculate_district_reporting_rate(data, threshold = 90)
#'   calculate_district_reporting_rate(data, threshold = 85, region = "Eastern")
#' }
#'
#' @export
calculate_district_reporting_rate <- function(.data, threshold = 90, region = NULL) {
  year <- district <- . <- NULL

  check_cd_data(.data)

  group_name <- get_indicator_group_names()
  # indicators <- paste0(group_name, "_rr")
  indicators <- paste0(group_name[which(group_name != 'ipd')], '_rr')

  reporting_rate <- .data %>%
    filter(if(is.null(region)) TRUE else adminlevel_1 == region) %>%
    summarise(across(all_of(indicators), mean, na.rm = TRUE), .by = c(district, year)) %>%
    summarise(across(all_of(indicators), ~ mean(.x >= threshold, na.rm = TRUE) * 100, .names = "low_{.col}"), .by = year) %>%
    mutate(
      low_mean_rr = rowMeans(select(., starts_with("low_")), na.rm = TRUE)
    ) %>%
    mutate(across(starts_with("low_"), round, 0))

  new_tibble(
    reporting_rate,
    class = "cd_district_reporting_rate",
    threshold = threshold
  )
}
