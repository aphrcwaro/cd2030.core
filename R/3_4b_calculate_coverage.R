#' Combine Immunization Coverage Data
#'
#' `calculate_coverage` integrates immunization coverage data from three sources:
#' DHIS2 (District Health Information Software), survey-based estimates, and
#' WUENIC (WHO-UNICEF estimates). The combined dataset is prepared for analysis
#' at various administrative levels.
#'
#' @param .data A `cd_data` data frame with DHIS2 coverage metrics.
#' @param admin_level Character. Specifies the administrative level for calculations.
#'   Options include:`"national", "adminlevel_1"`, and `"district"`.
#' @param survey_data A data frame containing survey-based immunization estimates.
#' @param wuenic_data A data frame containing WHO-UNICEF (WUENIC) coverage estimates.
#' @param un_estimates Optional. A tibble containing UN population estimates. Required
#'   for national-level calculations.
#' @param sbr Numeric. The stillbirth rate. Default is `0.02`.
#' @param nmr Numeric. Neonatal mortality rate. Default is `0.025`.
#' @param pnmr Numeric. Post-neonatal mortality rate. Default is `0.024`.
#' @param anc1survey Numeric. Survey-derived coverage rate for ANC-1 (antenatal care, first visit). Default is `0.98`.
#' @param dpt1survey Numeric. Survey-derived coverage rate for Penta-1 (DPT1 vaccination). Default is `0.97`.
#' @param survey_year Integer. The year of Penta-1 survey provided
#' @param preg_loss Numeric. Pregnancy loss rate
#' @param twin Numeric. Twin birth rate. Default is `0.015`.
#' @param subnational_map (Optional) A data frame mapping subnational regions to
#'   parent regions, required for subnational-level analyses. Default is `NULL`.
#'
#' @return A `cd_coverage` data frame containing harmonized coverage estimates
#'   for each year from DHIS2, WUENIC, and survey data. Includes metadata for
#'   administrative levels, regions, and denominators.
#'
#' @examples
#' \dontrun{
#' calculate_coverage(precomputed_data, survey_df, wuenic_df)
#' }
#' @export
calculate_coverage <- function(.data,
                               admin_level = c("national", "adminlevel_1", "district"),
                               survey_data,
                               wuenic_data,
                               region = NULL,
                               un_estimates = NULL,
                               sbr = 0.02,
                               nmr = 0.025,
                               pnmr = 0.024,
                               anc1survey = 0.98,
                               dpt1survey = 0.97,
                               survey_year = 2019,
                               twin = 0.015,
                               preg_loss = 0.03,
                               subnational_map = NULL) {
  year <- NULL

  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level, region)
  admin_level_cols <- c(admin_level_cols, 'year')

  # Validate inputs
  check_cd_data(.data)
  check_wuenic_data(wuenic_data)
  check_survey_data(survey_data, admin_level)

  coverage <- calculate_indicator_coverage(.data,
    admin_level = admin_level,
    region = region,
    un_estimates = un_estimates, sbr = sbr,
    nmr = nmr, pnmr = pnmr, anc1survey = anc1survey,
    dpt1survey = dpt1survey, survey_year = survey_year, twin = twin,
    preg_loss = preg_loss
  )

  # Prepare DHIS2 data
  dhis2_data <- coverage %>%
    select(any_of(admin_level_cols), matches("^cov_"))

  # Prepare survey data
  survey_data <- survey_data %>%
    select(any_of(admin_level_cols), matches("^ll_|^ul|^r_")) %>%
    join_subnational_map(admin_level, subnational_map) %>%
    check_district_column(admin_level, dhis2_data)

  # Prepare WUENIC data
  wuenic_data <- wuenic_data %>%
    select(year, matches("^cov_"))

  admin_level_c <- switch (
    admin_level,
    national = 'year',
    adminlevel_1 = c('adminlevel_1', 'year'),
    district = c('adminlevel_1', 'district', 'year')
  )

  # Join and Transform data
  combined_data <- dhis2_data %>%
    full_join(survey_data, by = admin_level_c, relationship = "many-to-many")

  if (admin_level == 'national') {
    combined_data <- combined_data %>%
      left_join(wuenic_data, by = "year")
  }

  combined_data <- combined_data %>%
    filter(if (is.null(region)) TRUE else adminlevel_1 == region) %>%
    select(-any_of(c("admin_level_1")))

  # Return result
  new_tibble(
    combined_data,
    class = "cd_coverage",
    admin_level = admin_level,
    region = region
  )
}


#' Filter and Reshape Coverage Data
#'
#' `filter_coverage` extracts specific coverage indicators and denominators from a
#' combined coverage dataset, applies regional filtering, and reshapes the data
#' for analysis and visualisation.
#'
#' @param .data A `cd_coverage` data frame with combined immunization coverage data.
#' @param indicator Coverage indicators to include (e.g., `"penta3"`, `"measles1"`).
#' @param denominator Denominator sources for coverage calculations (e.g., `"dhis2"`).
#'
#' @return A reshaped data frame with filtered indicators and denominators.
#'
#' @examples
#' \dontrun{
#' filter_coverage(combined_data, c("penta3", "measles1"), "dhis2", "Central Province")
#' }
#' @export
filter_coverage <- function(.data,
                            indicator,
                            denominator = c("dhis2", "anc1", "penta1", "penta1derived"),
                            region = NULL) {
  . <- value <- estimates <- NULL

  check_cd_coverage(.data)

  admin_level <- attr_or_abort(.data, 'admin_level')
  region_from_calc <- attr_or_null(.data, 'region')

  indicator <- arg_match(indicator, get_indicator_without_opd_ipd())
  denominator <- arg_match(denominator)

  if ((admin_level == 'national' || (admin_level == 'adminlevel_1' && !is.null(region_from_calc))) && !is.null(region)) {
    cd_abort(c("x" = "{.arg region} must be null."))
  }

  if ((admin_level %in% c('adminlevel_1', 'district') && is.null(region_from_calc)) && is.null(region)) {
    cd_abort(c("x" = "{.arg region} must be provided required for subnational analyses."))
  }

  admin_col <- get_admin_columns(admin_level)
  admin_col <- c(admin_col, 'year')
  dhis2_col <- paste0("cov_", indicator, "_", denominator)
  survey_estimate_col <- paste0("r_", indicator)
  lower_ci_col <- paste0("ll_", indicator)
  upper_ci_col <- paste0("ul_", indicator)
  wuenic_col <- paste0("cov_", indicator, "_wuenic")

  data <- .data %>%
    select(any_of(c(admin_col, dhis2_col, survey_estimate_col, lower_ci_col, upper_ci_col, wuenic_col)))

  data <- if (is.null(region_from_calc) && admin_level != 'national') {
    data %>% filter(!!sym(admin_level) == region)
  } else if (!is.null(region_from_calc) && admin_level != 'national') {
    data %>%
      summarise(
        across(any_of(c(dhis2_col, survey_estimate_col, lower_ci_col, upper_ci_col, wuenic_col)), mean, na.rm = TRUE),
        .by = any_of(c(admin_col, 'year'))
      )
  } else {
    data
  }

  data <- data %>%
    # select(any_of(c(admin_col, dhis2_col, survey_estimate_col, lower_ci_col, upper_ci_col, wuenic_col))) %>%
    # filter(if (admin_level == "national") TRUE else !!sym(admin_level) == region) %>% # Filter for region if applicable
    # Transform data
    mutate(
      !!dhis2_col := validate_column_existence(., dhis2_col),
      !!survey_estimate_col := validate_column_existence(., survey_estimate_col),
      !!wuenic_col := validate_column_existence(., wuenic_col),
      !!lower_ci_col := validate_column_existence(., lower_ci_col),
      !!upper_ci_col := validate_column_existence(., upper_ci_col)
    ) %>%
    pivot_longer(cols = contains(indicator), names_to = "estimates") %>%
    arrange(year) %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(
      estimates = case_match(
        estimates,
        dhis2_col ~ "DHIS2 estimate",
        survey_estimate_col ~ "Survey estimates",
        wuenic_col ~ "WUENIC estimates",
        lower_ci_col ~ "95% CI LL",
        upper_ci_col ~ "95% CI UL",
      )
    ) %>%
    select(-any_of(c("adminlevel_1", "district")))

  new_tibble(
    data,
    class = 'cd_coverage_filtered',
    admin_level = admin_level,
    indicator = indicator,
    denominator = denominator,
    region = region
  )
}


validate_column_existence <- function(.data, column) {
  if (!column %in% colnames(.data)) {
    cd_warn(c("!" = "Column {.field {column}} not found in the data."))
    return(rep(NA, nrow(.data)))
  }
  .data[[column]]
}

join_subnational_map <- function(.data, admin_level, map) {
  # admin_level_argument will be used once the district column is introduce in survey

  adminlevel_1 = admin_level_1 = NULL

  if (admin_level != "national") {
    if (!is.null(map)) {
      .data <- .data %>%
        left_join(map, join_by(adminlevel_1)) %>%
        filter(!is.na(admin_level_1)) %>%
        select(-adminlevel_1) %>%
        rename(adminlevel_1 = admin_level_1)
    }
  }

  .data
}

check_district_column <- function(.data, admin_level, dhis2_data) {
  adminlevel_1 <- district <- NULL

  if (admin_level == "district" && !"district" %in% colnames(.data)) {
    .data <- dhis2_data %>%
      distinct(adminlevel_1, district) %>%
      left_join(.data, by = "adminlevel_1", relationship = "many-to-many")
  }

  .data
}
