#' Calculate Health Coverage Indicators
#'
#' `calculate_indicator_coverage` computes key health coverage indicators across
#' specified administrative levels (national, adminlevel_1, and district). The function
#' integrates data from multiple sources, including DHIS-2, UN estimates, ANC-1,
#' and Penta-1 survey data. It calculates coverage rates for a variety of vaccinations
#' and health metrics based on projected, survey-derived, and estimated denominators.
#'
#' @param .data A `cd_data` tibble containing DHIS-2, UN, ANC-1, and Penta-1 data.
#'   This dataset must include columns for key population and vaccination metrics.
#' @param admin_level Character. Specifies the administrative level for calculations.
#'   Options include:`"national", "adminlevel_1"`, and `"district"`.
#' @param un_estimates Optional. A tibble containing UN population estimates. Required
#'   for national-level calculations.
#' @param sbr Numeric. The stillbirth rate. Default is `0.02`.
#' @param nmr Numeric. Neonatal mortality rate. Default is `0.025`.
#' @param pnmr Numeric. Post-neonatal mortality rate. Default is `0.024`.
#' @param anc1survey Numeric. Survey-derived coverage rate for ANC-1 (antenatal care, first visit). Default is `0.98`.
#' @param dpt1survey Numeric. Survey-derived coverage rate for Penta-1 (DPT1 vaccination). Default is `0.97`.
#' @param survey_year Interger. The year of Penta-1 survey provided
#' @param twin Numeric. Twin birth rate. Default is `0.015`.
#' @param preg_loss Numeric. Pregnancy loss rate. Default is `0.03`.
#'
#' @return A tibble of class `cd_indicator_coverage` containing calculated coverage
#'   indicators for the specified administrative level.
#'
#' @examples
#' \dontrun{
#' # Calculate coverage indicators at the national level
#' coverage_data <- calculate_indicator_coverage(
#'   .data = dhis2_data,
#'   admin_level = "national",
#'   un_estimates = un_data
#' )
#'
#' # Calculate coverage indicators at the district level
#' coverage_data <- calculate_indicator_coverage(
#'   .data = dhis2_data,
#'   admin_level = "district"
#' )
#' }
#'
#' @export
calculate_indicator_coverage <- function(.data,
                                         admin_level = c("national", "adminlevel_1", "district"),
                                         un_estimates = NULL,
                                         region = NULL,
                                         sbr = 0.02,
                                         nmr = 0.025,
                                         pnmr = 0.024,
                                         anc1survey = 0.98,
                                         dpt1survey = 0.97,
                                         survey_year = 2019,
                                         twin = 0.015,
                                         preg_loss = 0.03) {
  check_cd_data(.data)
  check_scalar_integerish(survey_year)
  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level, region)
  admin_level_cols <- c(admin_level_cols, 'year')
  country_iso <- attr_or_abort(.data, 'iso3')

  population <- calculate_populations(.data,
    admin_level = admin_level,
    un_estimates = un_estimates,
    region = region,
    sbr = sbr, nmr = nmr, pnmr = pnmr,
    anc1survey = anc1survey, dpt1survey = dpt1survey,
    twin = twin, preg_loss = preg_loss
  ) # %>%
  # select(any_of(c(admin_level_cols, 'year')), starts_with('cov_'))

  derived_data <- get_indicator_without_opd_ipd() %>%
    map(~ {
      dt <- calculate_derived_coverage(population, .x, survey_year, region)

      if (!is.null(dt)) {
        dt %>%
          select(any_of(admin_level_cols), ends_with("penta1derived"))
      } else {
        dt
      }
    }) %>%
    compact() %>%
    unique() %>%
    reduce(coalesce_join, by = admin_level_cols)

  output_data <- population %>%
    left_join(derived_data, by = admin_level_cols)

  new_tibble(
    output_data,
    class = c("cd_indicator_coverage", "cd_population"),
    admin_level = admin_level,
    iso3 = country_iso,
    region = region
  )
}

#' Filter Indicator Coverage for Plotting
#'
#' Prepares a long-form data frame of coverage values across denominator sources for a specific indicator
#' and year, including a user-defined national survey coverage value.
#'
#' @param .data A `cd_indicator_coverage` object.
#' @param indicator A string. The target health indicator (e.g., `"penta3"`, `"bcg"`).
#' @param survey_coverage A scalar numeric. The national survey coverage to include as a reference. Default is `88`.
#'
#' @return A `tibble` of class `'cd_indicator_coverage_filtered'`, enriched with attributes for plotting.
#'
#' @details
#' The function reshapes wide coverage data into long format, classifies each column by denominator type,
#' and extracts the indicator name. It selects only data for the most recent available year.
#'
#' @examples
#' \dontrun{
#' filtered <- filter_indicator_coverage(df, indicator = "penta3", survey_coverage = 90)
#' plot(filtered)
#' }
#'
#' @seealso [plot.cd_indicator_coverage_filtered()]
#'
#' @export
filter_indicator_coverage <- function(.data, indicator, survey_coverage = 88, survey_year = 2024) {
  check_cd_indicator_coverage(.data)
  indicator <- arg_match(indicator, get_indicator_without_opd_ipd())

  if (!is_scalar_double(survey_coverage)) {
    cd_abort(c("x" = "A scalar numeric is required."))
  }

  min_year <- min(.data$year)
  if (min_year > survey_year) {
    survey_year <- min_year
  }

  # Prepare the data for plotting
  data <- .data %>%
    pivot_longer(-any_of(c("country", "year", "iso3"))) %>%
    mutate(
      category = case_when(
        grepl("_dhis2$", name) ~ "DHIS2 projection",
        grepl("_anc1$", name) ~ "ANC1-derived",
        grepl("_penta1$", name) ~ "Penta1-derived",
        grepl("_un$", name) ~ "UN projection",
        grepl("_penta1derived$", name) ~ "Penta 1 population Growth"
      ),
      category = factor(category, levels = c("DHIS2 projection", "ANC1-derived", "Penta1-derived", "UN projection", "Penta 1 population Growth")),
      indicator_name = str_extract(name, "(?<=cov_)(.*)(?=_[^_]+$)")
    ) %>%
    filter(year == survey_year, indicator_name == indicator)

  new_tibble(
    data,
    class = 'cd_indicator_coverage_filtered',
    indicator = indicator,
    coverage = survey_coverage
  )
}

calculate_populations <- function(.data,
                                  admin_level = c("national", "adminlevel_1", "district"),
                                  un_estimates = NULL,
                                  region = NULL,
                                  sbr = 0.02,
                                  nmr = 0.025,
                                  pnmr = 0.024,
                                  anc1survey = 0.98,
                                  dpt1survey = 0.97,
                                  twin = 0.015,
                                  preg_loss = 0.03) {

  admin_level <- arg_match(admin_level)

  national_population <- prepare_population_metrics(.data, admin_level = admin_level, un_estimates = un_estimates, region = region)
  indicator_numerator <- compute_indicator_numerator(.data, admin_level = admin_level, region = region)

  group_vars <- get_admin_columns(admin_level, region)

  output_data <- national_population %>%
    inner_join(indicator_numerator, by = c(group_vars, "year")) %>%
    mutate(
      # DHIS2 Estimates
      totpreg_dhis2 = totlivebirths_dhis2 * (1 - 0.5 * twin) / ((1 - sbr) * (1 - preg_loss)),
      totdeliv_dhis2 = totpreg_dhis2 * (1 - preg_loss),
      totbirths_dhis2 = totlivebirths_dhis2 / (1 - sbr),
      totinftpenta_dhis2 = totlivebirths_dhis2 - totlivebirths_dhis2 * nmr,
      totinftmeasles_dhis2 = totinftpenta_dhis2 - totinftpenta_dhis2 * pnmr,
      totmeasles2_dhis2 = totinftpenta_dhis2 - totinftpenta_dhis2 * (2 * pnmr),

      # ANC1 Estimates
      totpreg_anc1 = anc1 / anc1survey,
      totdeliv_anc1 = totpreg_anc1 * (1 - preg_loss),
      totbirths_anc1 = totdeliv_anc1 / (1 - 0.5 * twin),
      totlbirths_anc1 = totbirths_anc1 * (1 - sbr),
      totinftpenta_anc1 = totlbirths_anc1 * (1 - nmr),
      totinftmeasles_anc1 = totinftpenta_anc1 * (1 - pnmr),
      totmeasles2_anc1 = totinftpenta_anc1 * (1 - (2 * pnmr)),

      # Penta1 Estimates
      totinftpenta_penta1 = penta1 / dpt1survey,
      totinftmeasles_penta1 = totinftpenta_penta1 * (1 - pnmr),
      totmeasles2_penta1 = totinftpenta_penta1 * (1 - (2 * pnmr)),
      totlbirths_penta1 = totinftpenta_penta1 / (1 - nmr),
      totbirths_penta1 = totlbirths_penta1 / (1 - sbr),
      totdeliv_penta1 = totbirths_penta1 * (1 - 0.5 * twin),
      totpreg_penta1 = totdeliv_penta1 / (1 - preg_loss),
    )

  if (admin_level == "national") {
    output_data <- output_data %>%
      mutate(
        totpreg_un = un_births * (1 - 0.5 * twin) / ((1 - sbr) * (1 - preg_loss)),
        totdeliv_un = totpreg_un * (1 - preg_loss),
        totbirths_un = un_births / (1 - sbr),
        totinftpenta_un = un_births - un_births * nmr,
        totinftmeasles_un = totinftpenta_un - totinftpenta_un * pnmr,
        totmeasles2_un = totinftpenta_un - totinftpenta_un * (2 * pnmr),

        cov_anc1_un = 100 * anc1/(totpreg_un * 1000),
        cov_instlivebirths_un = 100 * instlivebirths/(un_births * 1000),
        cov_instdeliveries_un = 100 * instlivebirths/(totdeliv_un * 1000),
        cov_bcg_un = 100 * bcg/(un_births * 1000),
        cov_penta1_un = 100 * penta1/(totinftpenta_un * 1000),
        cov_penta3_un = 100 * penta3/(totinftpenta_un * 1000),
        cov_measles1_un = 100 * measles1/(totinftmeasles_un * 1000),
        cov_measles2_un = 100 * measles2/(totinftmeasles_un * 1000)
      )

      if (get_selected_group() == 'rmncah') {
        output_data <- output_data %>%
          mutate(
            cov_anc_1trimester_un = 100 * anc_1trimester/(totpreg_un * 1000),
            cov_anc4_un = 100 * anc4/(totpreg_un * 1000),

            cov_ipt2_un = 100 * ipt2/(totpreg_un * 1000),
            cov_ipt3_un = 100 * ipt3/(totpreg_un * 1000),
            cov_ifa90_un = 100 * ifa90/(totpreg_un * 1000),
            cov_syphilis_test_un = 100 * syphilis_test/(totpreg_un * 1000),
            cov_hiv_test_un = 100 * hiv_test/(totpreg_un * 1000),

            cov_sba_un = 100 * sba/(un_births * 1000),

            cov_low_bweight_un = 100 * low_bweight/(un_births * 1000),
            cov_csection_un = 100 * csection/(totdeliv_un * 1000),
            cov_pnc48h_un = 100 * pnc48h/(un_births * 1000)
          )
      }

    if (get_selected_group() == 'vaccine') {
      output_data <- output_data %>%
        mutate(
          cov_penta2_un = 100 * penta2/(totinftpenta_un * 1000),

          cov_opv1_un = 100 * opv1/(totinftpenta_un * 1000),
          cov_opv2_un = 100 * opv2/(totinftpenta_un * 1000),
          cov_opv3_un = 100 * opv3/(totinftpenta_un * 1000),

          cov_pcv1_un = 100 * pcv1/(totinftpenta_un * 1000),
          cov_pcv2_un = 100 * pcv2/(totinftpenta_un * 1000),
          cov_pcv3_un = 100 * pcv3/(totinftpenta_un * 1000),

          cov_rota1_un = 100 * rota1/(totinftpenta_un * 1000),
          cov_rota2_un = 100 * rota2/(totinftpenta_un * 1000),

          cov_ipv1_un = 100 * ipv1/(totinftpenta_un * 1000),
          cov_ipv2_un = 100 * ipv2/(totinftpenta_un * 1000),

          cov_zerodose_un = 100 * ((totinftpenta_un * 1000 - penta1)/totinftpenta_un * 1000),
          cov_undervax_un = 100 * ((totinftpenta_un * 1000 - penta3)/totinftpenta_un * 1000),
          cov_dropout_penta13_un = ((penta1 - penta3)/penta1) * 100,
          cov_dropout_measles12_un = ((measles1 - measles2)/measles1) * 100,
          cov_dropout_penta3mcv1_un = ((penta3 - measles1)/penta3) * 100,
          cov_dropout_penta1mcv1_un = ((penta1-measles1)/penta1) * 100
        )
    }
  }

  # From DHIS2 Derived Denominators
  output_data <- output_data %>%
    # Compute coverage  based on projected lives births in DHIS-2
    mutate(
      cov_anc1_dhis2 = 100 * anc1/(totpreg_dhis2 * 1000),
      cov_instlivebirths_dhis2 = 100 * instlivebirths/(totlivebirths_dhis2 * 1000),
      cov_instdeliveries_dhis2 = 100 * instlivebirths/(totdeliv_dhis2 * 1000),
      cov_bcg_dhis2 = 100 * bcg/(totlivebirths_dhis2 * 1000),
      cov_penta1_dhis2 = 100 * penta1/(totinftpenta_dhis2 * 1000),
      cov_penta3_dhis2 = 100 * penta3/(totinftpenta_dhis2 * 1000),
      cov_measles1_dhis2 = 100 * measles1/(totinftmeasles_dhis2 * 1000),
      cov_measles2_dhis2 = 100 * measles2/(totinftmeasles_dhis2 * 1000)
    )

  if (get_selected_group() == 'rmncah') {
    output_data <- output_data %>%
      mutate(
        cov_anc_1trimester_dhis2 = 100 * anc_1trimester/(totpreg_dhis2 * 1000),
        cov_anc4_dhis2 = 100 * anc4/(totpreg_dhis2 * 1000),

        cov_ipt2_dhis2 = 100 * ipt2/(totpreg_dhis2 * 1000),
        cov_ipt3_dhis2 = 100 * ipt3/(totpreg_dhis2 * 1000),
        cov_ifa90_dhis2 = 100 * ifa90/(totpreg_dhis2 * 1000),
        cov_syphilis_test_dhis2 = 100 * syphilis_test/(totpreg_dhis2 * 1000),
        cov_hiv_test_dhis2 = 100 * hiv_test/(totpreg_dhis2 * 1000),

        cov_sba_dhis2 = 100 * sba/(totlivebirths_dhis2 * 1000),

        cov_low_bweight_dhis2 = 100 * low_bweight/(totlivebirths_dhis2 * 1000),
        cov_csection_dhis2 = 100 * csection/(totdeliv_dhis2 * 1000),
        cov_pnc48h_dhis2 = 100 * pnc48h/(totlivebirths_dhis2 * 1000)
      )
  }

  if (get_selected_group() == 'vaccine') {
    output_data <- output_data %>%
      mutate(
        cov_penta2_dhis2 = 100 * penta2/(totinftpenta_dhis2 * 1000),

        cov_opv1_dhis2 = 100 * opv1/(totinftpenta_dhis2 * 1000),
        cov_opv2_dhis2 = 100 * opv2/(totinftpenta_dhis2 * 1000),
        cov_opv3_dhis2 = 100 * opv3/(totinftpenta_dhis2 * 1000),

        cov_pcv1_dhis2 = 100 * pcv1/(totinftpenta_dhis2 * 1000),
        cov_pcv2_dhis2 = 100 * pcv2/(totinftpenta_dhis2 * 1000),
        cov_pcv3_dhis2 = 100 * pcv3/(totinftpenta_dhis2 * 1000),

        cov_rota1_dhis2 = 100 * rota1/(totinftpenta_dhis2 * 1000),
        cov_rota2_dhis2 = 100 * rota2/(totinftpenta_dhis2 * 1000),

        cov_ipv1_dhis2 = 100 * ipv1/(totinftpenta_dhis2 * 1000),
        cov_ipv2_dhis2 = 100 * ipv2/(totinftpenta_dhis2 * 1000),

        cov_zerodose_dhis2 = 100 * ((totinftpenta_dhis2 * 1000 - penta1)/totinftpenta_dhis2 * 1000),
        # generating undervax indicators
        cov_undervax_dhis2 = 100 * ((totinftpenta_dhis2 * 1000 - penta3)/totinftpenta_dhis2 * 1000),
        # generating drop-out indicators
        cov_dropout_penta13_dhis2 = ((penta1 - penta3)/penta1) * 100,
        cov_dropout_measles12_dhis2 = ((measles1 - measles2)/measles1) * 100,
        cov_dropout_penta3mcv1_dhis2 = ((penta3 - measles1)/penta3) * 100,
        cov_dropout_penta1mcv1_dhis2 = ((penta1 - measles1)/penta1) * 100
      )
  }

    # From ANC-1 Derived Denominators
  output_data <- output_data %>%
    mutate(
      cov_anc1_anc1 = 100 * anc1/totpreg_anc1,
      cov_instlivebirths_anc1 = 100 * instlivebirths/totlbirths_anc1,
      cov_instdeliveries_anc1 = 100 * instlivebirths/totdeliv_anc1,
      cov_bcg_anc1 = 100 * bcg/totlbirths_anc1,
      cov_penta1_anc1 = 100 * penta1/totinftpenta_anc1,
      cov_penta3_anc1 = 100 * penta3/totinftpenta_anc1,
      cov_measles1_anc1 = 100 * measles1/totinftmeasles_anc1,
      cov_measles2_anc1 = 100 * measles2/totinftmeasles_anc1
    )

  if (get_selected_group() == 'rmncah') {
    output_data <- output_data %>%
      mutate(
        cov_anc_1trimester_anc1 = 100 * anc_1trimester/totpreg_anc1,
        cov_anc4_anc1 = 100 * anc4/totpreg_anc1,

        cov_ipt2_anc1 = 100 * ipt2/totpreg_anc1,
        cov_ipt3_anc1 = 100 * ipt3/totpreg_anc1,
        cov_ifa90_anc1 = 100 * ifa90/totpreg_anc1,
        cov_syphilis_test_anc1 = 100 * syphilis_test/totpreg_anc1,
        cov_hiv_test_anc1 = 100 * hiv_test/totpreg_anc1,

        cov_sba_anc1 = 100 * sba/totlbirths_anc1,
        cov_low_bweight_anc1 = 100 * low_bweight/totlbirths_anc1,
        cov_csection_anc1 = 100 * csection/totdeliv_anc1,
        cov_pnc48h_anc1 = 100 * pnc48h/totlbirths_anc1,
      )
  }

  if (get_selected_group() == 'vaccine') {
    output_data <- output_data %>%
      mutate(
        cov_penta2_anc1 = 100 * penta2/totinftpenta_anc1,

        cov_opv1_anc1 = 100 * opv1/totinftpenta_anc1,
        cov_opv2_anc1 = 100 * opv2/totinftpenta_anc1,
        cov_opv3_anc1 = 100 * opv3/totinftpenta_anc1,

        cov_pcv1_anc1 = 100 * pcv1/totinftpenta_anc1,
        cov_pcv2_anc1 = 100 * pcv2/totinftpenta_anc1,
        cov_pcv3_anc1 = 100 * pcv3/totinftpenta_anc1,

        cov_rota1_anc1 = 100 * rota1/totinftpenta_anc1,
        cov_rota2_anc1 = 100 * rota2/totinftpenta_anc1,

        cov_ipv1_anc1 = 100 * ipv1/totinftpenta_anc1,
        cov_ipv2_anc1 = 100 * ipv2/totinftpenta_anc1,

        cov_zerodose_anc1 = 100 * ((totinftpenta_anc1 * 1000 - penta1)/totinftpenta_anc1 * 1000),
        # generating undervax indicators
        cov_undervax_anc1 = 100 * ((totinftpenta_anc1 * 1000 - penta3)/totinftpenta_anc1 * 1000),
        # generating drop-out indicators
        cov_dropout_penta13_anc1 = ((penta1 - penta3)/penta1) * 100,
        cov_dropout_measles12_anc1 = ((measles1 - measles2)/measles1) * 100,
        cov_dropout_penta3mcv1_anc1 = ((penta3-measles1)/penta3) * 100,
        cov_dropout_penta1mcv1_anc1 = ((penta1 - measles1)/penta1) * 100
      )
  }

    # From PENTA-1 Derived Denominators
  output_data <- output_data %>%
    mutate(
      cov_anc1_penta1 = 100 * anc1/totpreg_penta1,
      cov_instlivebirths_penta1 = 100 * instlivebirths/totlbirths_penta1,
      cov_instdeliveries_penta1 = 100 * instlivebirths/totdeliv_penta1,
      cov_bcg_penta1 = 100 * bcg/totlbirths_penta1,
      cov_penta1_penta1 = 100 * penta1/totinftpenta_penta1,
      cov_penta3_penta1 = 100 * penta3/totinftpenta_penta1,
      cov_measles1_penta1 = 100 * measles1/totinftmeasles_penta1,
      cov_measles2_penta1 = 100 * measles2/totinftmeasles_penta1
    )

  if (get_selected_group() == 'rmncah') {
    output_data <- output_data %>%
      mutate(
        cov_anc_1trimester_penta1 = 100 * anc_1trimester/totpreg_penta1,
        cov_anc4_penta1 = 100 * anc4/totpreg_penta1,

        cov_ipt2_penta1 = 100 * ipt2/totpreg_penta1,
        cov_ipt3_penta1 = 100 * ipt3/totpreg_penta1,
        cov_ifa90_penta1 = 100 * ifa90/totpreg_penta1,
        cov_syphilis_test_penta1 = 100 * syphilis_test/totpreg_penta1,
        cov_hiv_test_penta1 = 100 * hiv_test/totpreg_penta1,

        cov_sba_penta1 = 100 * sba/totlbirths_penta1,

        cov_low_bweight_penta1 = 100 * low_bweight/totlbirths_penta1,
        cov_csection_penta1 = 100 * csection/totdeliv_penta1,
        cov_pnc48h_penta1 = 100 * pnc48h/totlbirths_penta1,
      )
  }

  if (get_selected_group() == 'vaccine') {
    output_data <- output_data %>%
      mutate(
        cov_penta2_penta1 = 100 * penta2/totinftpenta_penta1,

        cov_opv1_penta1 = 100 * opv1/totinftpenta_penta1,
        cov_opv2_penta1 = 100 * opv2/totinftpenta_penta1,
        cov_opv3_penta1 = 100 * opv3/totinftpenta_penta1,

        cov_pcv1_penta1 = 100 * pcv1/totinftpenta_penta1,
        cov_pcv2_penta1 = 100 * pcv2/totinftpenta_penta1,
        cov_pcv3_penta1 = 100 * pcv3/totinftpenta_penta1,

        cov_rota1_penta1 = 100 *rota1/totinftpenta_penta1,
        cov_rota2_penta1 = 100 * rota2/totinftpenta_penta1,

        cov_ipv1_penta1 = 100 * ipv1/totinftpenta_penta1,
        cov_ipv2_penta1 = 100 * ipv2/totinftpenta_penta1,

        cov_zerodose_penta1 = 100 * ((totinftpenta_penta1 * 1000 - penta1)/totinftpenta_penta1 * 1000),
        # generating undervax indicators
        cov_undervax_penta1 = 100 * ((totinftpenta_penta1 * 1000 - penta3)/totinftpenta_penta1 * 1000),
        # generating drop-out indicators
        cov_dropout_penta13_penta1 = ((penta1 - penta3)/penta1) * 100,
        cov_dropout_measles12_penta1 = ((measles1 - measles2)/measles1) * 100,
        cov_dropout_penta3mcv1_penta1 = ((penta3 - measles1)/penta3) * 100,
        cov_dropout_penta1mcv1_penta1 = ((penta1 - measles1)/penta1) * 100
      )
  }

  new_tibble(
    output_data,
    class = "cd_population"
  )
}

coalesce_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), join = left_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)

  # Identify columns with suffixes
  x_cols_suffix <- names(joined)[str_ends(names(joined), fixed(suffix[1]))]
  y_cols_suffix <- names(joined)[str_ends(names(joined), fixed(suffix[2]))]

  # Base names (remove suffix)
  x_base <- str_remove(x_cols_suffix, fixed(suffix[1]))
  y_base <- str_remove(y_cols_suffix, fixed(suffix[2]))

  # Columns truly common to x and y
  common_cols <- intersect(x_base, y_base)

  # Build coalesced columns
  coalesced_cols <- map(common_cols, function(col) {
    coalesce(
      joined[[paste0(col, suffix[1])]],
      joined[[paste0(col, suffix[2])]]
    )
  }) %>% set_names(common_cols)

  # Build final tibble
  joined %>%
    mutate(!!!coalesced_cols) %>%
    select(
      -any_of(paste0(common_cols, suffix[1])),
      -any_of(paste0(common_cols, suffix[2]))
    )
}
