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
#' @param base_year Integer. The year from which denominator proportions are derived.
#'
#' @return A `cd_derived_coverage` tibble with columns for old and new coverage estimates.
#'
#' @examples
#' calculate_derived_coverage(dhis_data, "penta1", 2019)
#'
#' @export
calculate_derived_coverage <- function(.data, indicator, base_year, region = NULL) {
  check_cd_population(.data)

  # Get admin level attribute (national, adminlevel_1, district)
  admin_level <- attr_or_abort(.data, "admin_level")

  # Match and reference the selected indicator
  indicator <- arg_match(indicator, get_all_indicators())
  indicator_col <- sym(indicator)

  coverage <- paste0("cov_", indicator, "_penta1")
  coverage_col <- sym(coverage)

  coverage_derived <- paste0(coverage, "derived")
  coverage_derived_col <- sym(coverage_derived)

  # Get relevant population column for DHIS2 (e.g., totpop_dhis2)
  dhis2_pop <- get_population_column(indicator, "dhis2")
  dhis2_pop_col <- sym(dhis2_pop)

  penta1_denom <- get_population_column(indicator, "penta1")
  penta1_denom_col <- sym(penta1_denom)

  derived_denom <- paste0(penta1_denom, "derived")
  derived_denom_col <- sym(derived_denom)

  # Determine grouping columns based on admin level
  group_cols <- get_admin_columns(admin_level, region)

  if (!indicator %in% colnames(.data)) {
    return(NULL)
  }

  # If subnational, aggregate to national level first
  nat_data <- if (!is.null(group_cols)) {
    .data %>%
      summarise(
        across(-any_of(c(group_cols, "year")), ~ sum(.x, na.rm = TRUE)),
        .by = year
      )
  } else {
    .data
  }

  # Keep only necessary columns and convert DHIS2 pop to count (×1000)
  nat_data <- nat_data %>%
    select(year, all_of(c(indicator, dhis2_pop, coverage)), !!penta1_denom_col, totunder1_dhis2) # %>%
  # mutate(!!dhis2_pop_col := !!dhis2_pop_col * 1000)

  # Ensure base year is not earlier than first year in data
  base_year <- robust_max(c(base_year, min(nat_data$year, na.rm = TRUE)), 2024)

  # Extract base year values for national denominator and population
  base_row <- nat_data %>%
    filter(year == base_year)

  # base_value <- base_row %>% pull(!!dhis2_pop_col)        # national base population
  base_value <- base_row %>% pull(totunder1_dhis2) # national base population
  base_denom <- base_row %>% pull(!!penta1_denom_col) # national base DTP1-derived denominator

  # ---- NATIONAL-LEVEL DERIVED DENOMINATOR TRENDS ----
  national <- nat_data %>%
    mutate(
      # Step 1: Compute percent change in DHIS2 population over time from base
      # percent_change = (!!dhis2_pop_col - base_value) / base_value,
      percent_change = (totunder1_dhis2 - base_value) / base_value,

      # Step 2: Apply percent change to national base denominator
      !!derived_denom_col := base_denom * (1 + percent_change),

      # Step 3: Calculate traditional and derived coverage values
      # coverage_old = (!!indicator_col / totinftpenta_penta1) * 100,
      !!coverage_derived_col := (!!indicator_col / !!derived_denom_col) * 100
    ) %>%
    select(
      year,
      !!indicator_col,
      # !!dhis2_pop_col,
      totunder1_dhis2,
      !!penta1_denom_col,
      !!derived_denom_col,
      # percent_change,
      !!coverage_col,
      !!coverage_derived_col
    )

  # ---- SUBNATIONAL: DISTRIBUTE DERIVED NATIONAL DENOMINATOR ----
  data_joined <- if (!is.null(group_cols)) {
    # Prepare national population and derived_denom for merge
    national_sel <- national %>%
      # select(year, !!derived_denom_col, !!dhis2_pop_col) %>%
      # rename(national_pop = !!dhis2_pop_col)
      select(year, !!derived_denom_col, totunder1_dhis2) %>%
      rename(national_pop = totunder1_dhis2)

    # Step 1: Aggregate subnational indicator, DHIS2 population, and DTP1 denom
    .data %>%
      summarise(
        !!indicator_col := sum(!!indicator_col, na.rm = TRUE),
        # !!dhis2_pop_col := sum(!!dhis2_pop_col * 1000, na.rm = TRUE),
        totunder1_dhis2 = sum(totunder1_dhis2, na.rm = TRUE),
        !!penta1_denom_col := sum(!!penta1_denom_col, na.rm = TRUE),
        .by = c(year, group_cols)
      ) %>%
      left_join(national_sel, by = "year") %>%
      mutate(
        # Step 2: Compute subnational DHIS2 share of national population
        # nat_pro = !!dhis2_pop_col / national_pop,
        nat_pro = totunder1_dhis2 / national_pop,

        # Step 3: Apply that share to national derived denominator
        !!derived_denom_col := !!derived_denom_col * nat_pro,

        # Step 4: Recalculate coverage
        !!coverage_col := (!!indicator_col / !!penta1_denom_col) * 100,
        !!coverage_derived_col := (!!indicator_col / !!derived_denom_col) * 100
      ) %>%
      select(-national_pop, -nat_pro)
  } else {
    # If admin level is national, use already-computed national data
    national
  }

  # Calculate the number of zerodose based on penta1 and penta1derived figures
  data_joined <- data_joined %>%
    mutate(
      zerodose_penta1 = !!coverage_col - !!indicator_col,
      zerodose_penta1derived = !!derived_denom_col - !!indicator_col
    )

  # Return final tibble tagged with admin level
  new_tibble(
    data_joined,
    class = "cd_derived_coverage",
    admin_level = admin_level,
    indicator = indicator
  )
}
