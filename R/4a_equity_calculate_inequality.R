#' Analyze Subnational Health Coverage Data with Inequality Metrics
#'
#' `calculate_inequality` computes subnational health coverage metrics and evaluates
#' disparities compared to national averages. The function provides metrics such as:
#'
#' - **Mean Absolute Difference to the Mean (MADM)**: Average absolute deviation
#'   from the national mean.
#' - **Weighted MADM**: MADM weighted by population share.
#' - **Mean Relative Difference to the Mean (MRDM)**: MADM as a percentage of
#'   the national mean.
#' - **Weighted MRDM**: MRDM weighted by population share.
#' - **Relative Difference Max (RD Max)**: Adjusted maximum difference metric.
#'
#' The function allows analysis of specific health indicators at subnational levels
#' (`adminlevel_1` or `district`) and compares them with national-level data.
#'
#' @param .data A data frame containing subnational health coverage data.
#' @param admin_level A character string specifying the administrative level for analysis.
#'   Options: `"adminlevel_1"` (e.g., regions) or `"district"`.
#' @param un_estimates (Optional) A data frame with UN population estimates for national
#'   population-level calculations. Required if using population-based metrics.
#' @param sbr Numeric. The stillbirth rate (default: 0.02).
#' @param nmr Numeric. The neonatal mortality rate (default: 0.025).
#' @param pnmr Numeric. The post-neonatal mortality rate (default: 0.024).
#' @param anc1survey Numeric. Survey-based ANC-1 coverage rate (default: 0.98).
#' @param dpt1survey Numeric. Survey-based Penta-1 coverage rate (default: 0.97).
#' @param survey_year Integer. The year of Penta-1 survey provided
#' @param twin Numeric. The twin birth rate (default: 0.015).
#' @param preg_loss Numeric. The pregnancy loss rate (default: 0.03).
#'
#' @return A tibble (`cd_inequality` object) containing:
#'   - Subnational health coverage metrics.
#'   - Population shares.
#'   - MADM, MRDM, and related disparity metrics.
#'
#' @examples
#' \dontrun{
#' # Example analysis for district-level data
#' inequality_metrics <- calculate_inequality(
#'   .data = health_data,
#'   admin_level = "district",
#'   un_estimates = un_data
#' )
#' }
#'
#' @export
calculate_inequality <- function(.data,
                                 admin_level = c("adminlevel_1", "district"),
                                 un_estimates,
                                 region = NULL,
                                 sbr = 0.02,
                                 nmr = 0.025,
                                 pnmr = 0.024,
                                 anc1survey = 0.98,
                                 dpt1survey = 0.97,
                                 survey_year = 2019,
                                 twin = 0.015,
                                 preg_loss = 0.03) {
  year <- NULL

  # Validation
  check_cd_data(.data)
  admin_level <- arg_match(admin_level)
  admin_level_col <- get_admin_columns(admin_level, region)
  admin_level_col <- c(admin_level_col, 'year')

  level <- if (admin_level == 'adminlevel_1' && !is.null(region)) {
    'adminlevel_1'
  } else {
    'national'
  }

  national_data <- calculate_indicator_coverage(.data,
    admin_level = level,
    un_estimates = un_estimates,
    sbr = sbr, nmr = nmr, pnmr = pnmr,
    anc1survey = anc1survey, dpt1survey = dpt1survey,
    survey_year = survey_year, twin = twin, preg_loss = preg_loss
  ) %>%
    filter(if (is.null(region)) TRUE else adminlevel_1 == region) %>%
    select(any_of(admin_level_col), matches("^cov_|^tot"), -ends_with("_un")) %>%
    rename_with(~ paste0("nat_", .x), matches("^cov_|^tot"))

  subnational_data <- calculate_indicator_coverage(.data,
    admin_level = admin_level,
    region = region,
    sbr = sbr, nmr = nmr, pnmr = pnmr,
    anc1survey = anc1survey, dpt1survey = dpt1survey,
    survey_year = survey_year, twin = twin, preg_loss = preg_loss
  ) %>%
    select(any_of(admin_level_col), matches("^cov_|^tot"))

  join_keys <- switch (
    level,
    national = 'year',
    adminlevel_1 = c('adminlevel_1', 'year')
  )

  combined_data <- subnational_data %>%
    left_join(national_data, by = join_keys) %>%
    mutate(
      across(starts_with("cov_"), ~ abs(.x - get(paste0("nat_", cur_column()))), .names = "diff_{.col}"),
      across(starts_with("tot"), ~ .x / get(paste0("nat_", cur_column())), .names = "popshare_{.col}")
    ) %>%
    mutate(
      across(starts_with("diff_"), ~ mean(.x, na.rm = TRUE), .names = "{ gsub('^diff', 'madm', .col) }"),
      across(starts_with("diff_"), ~ {
        indicator <- str_extract(cur_column(), "(?<=_cov_).+(?=_[^_]+$)")
        denominator <- str_extract(cur_column(), "[^_]+$")
        population <- get_population_column(indicator, denominator)
        if (is.na(population)) {
          return(NA)
        }

        stats::weighted.mean(.x, get(paste("popshare", population, sep = "_")), na.rm = TRUE)
      },
      .names = "{ gsub('^diff', 'madmpop', .col) }"
      ),
      across(starts_with("madm_"), ~ {
        indicator <- str_replace(cur_column(), "^madm", "nat")

        (.x / mean(get(indicator))) * 100
      },
      .names = "{ gsub('^madm', 'mrdm', .col) }"
      ),
      across(starts_with("madmpop_"), ~ {
        indicator <- str_replace(cur_column(), "^madmpop", "nat")

        (.x / mean(get(indicator))) * 100
      },
      .names = "{ gsub('^madmpop', 'mrdmpop', .col) }"
      ),
      .by = year
    ) %>%
    mutate(
      across(starts_with("cov_"), ~ {
        rd_max <- round(robust_max(.x, fallback = 100), -1)
        rd_max <- if_else(rd_max < robust_max(.x, fallback = 100), rd_max + 10, rd_max)
        rd_max <- rd_max * 1.05

        if_else(rd_max %% 20 != 0, rd_max + 10, rd_max)
      },
      .names = "rd_max_{.col}"
      )
    )

  new_tibble(
    combined_data,
    class = "cd_inequality",
    admin_level = admin_level,
    region = region
  )
}

#' Filter Subnational Inequality Metrics
#'
#' `filter_inequality` refines the output of `calculate_inequality` by selecting
#' specific indicators and denominators for analysis. It extracts and renames relevant
#' columns to streamline further analyses.
#'
#' @param .data A `cd_inequality` tibble created by `calculate_inequality`.
#' @param indicator A character vector of health indicators to include (e.g., `"penta3"`, `"measles1"`).
#' @param denominator A character vector of denominators to filter by (e.g., `"dhis2"`, `"anc1"`).
#' @param ... Not used
#'
#' @return A tibble containing filtered subnational inequality metrics for
#'   the specified indicators and denominators.
#'
#' @examples
#' \dontrun{
#' # Filter for Penta-3 coverage using DHIS-2 denominator
#' filtered_data <- inequality_metrics %>%
#'   filter_inequality(indicator = "penta3", denominator = "dhis2")
#' }
#'
#' @export
filter_inequality <- function(.data,
                              indicator,
                              denominator = c("dhis2", "anc1", "penta1", "penta1derived")) {

  admin_level <- attr_or_abort(.data, "admin_level")
  region <- attr_or_null(.data, 'region')

  indicator <- arg_match(indicator, get_analysis_indicators())
  denominator <- arg_match(denominator)

  pop_col <- get_population_column(indicator, denominator)
  dhis_col <- paste("cov", indicator, denominator, sep = "_")
  admin_col <- get_admin_columns(admin_level)

  data <- .data %>%
    select(year, any_of(c(pop_col, admin_col)), ends_with(dhis_col), ends_with(pop_col)) %>%
    rename_with(~ str_remove(.x, paste0("_", dhis_col)), ends_with(dhis_col)) %>%
    select(-any_of(paste0("nat_", pop_col))) %>%
    rename_with(~ str_remove(.x, paste0("_", pop_col)), ends_with(pop_col))

  new_tibble(
    data,
    class = 'cd_inequality_filtered',
    admin_level = admin_level,
    region = region,
    indicator = indicator,
    denominator = denominator
  )
}
