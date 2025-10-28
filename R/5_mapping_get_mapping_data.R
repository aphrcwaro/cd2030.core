#' Get Mapping Data for Subnational Coverage Analysis
#'
#' Merges coverage data with a country's shapefile to enable spatial visualization
#' of health indicators. It computes subnational indicator coverage using
#' UN estimates and survey rates, and joins results with geographic features.
#'
#' @param .data A data frame of input data containing model output or estimates.
#'        Must include metadata attribute `iso3` (ISO3 country code).
#' @param admin_level Character. The subnational level of analysis.
#'        Must be one of `"adminlevel_1"` (default) or `"district"`.
#' @param un_estimates A data frame of UN estimates including population and birth rates.
#' @param sbr Numeric. Stillbirth rate. Default = `0.02`.
#' @param nmr Numeric. Neonatal mortality rate. Default = `0.025`.
#' @param pnmr Numeric. Post-neonatal mortality rate. Default = `0.024`.
#' @param anc1survey Numeric. Survey coverage for ANC1. Default = `0.98`.
#' @param dpt1survey Numeric. Survey coverage for Penta1. Default = `0.97`.
#' @param survey_year Integer. Year of survey used in indicator estimation. Default = `2019`.
#' @param twin Numeric. Twin birth rate. Default = `0.015`.
#' @param preg_loss Numeric. Pregnancy loss rate. Default = `0.03`.
#' @param subnational_map Optional. A data frame to join with the shapefile to add metadata.
#'
#' @return A `tibble` of class `"cd_mapping"` that combines the input data with shapefile geometries.
#'
#' @examples
#' \dontrun{
#' rates <- list(
#'   sbr = 0.02, nmr = 0.03, pnmr = 0.02, anc1 = 0.8, penta1 = 0.75,
#'   twin_rate = 0.015, preg_loss = 0.03
#' )
#'
#' get_mapping_data(
#'   .data = data,
#'   un_estimates = un_estimates,
#'   sbr = rates$sbr,
#'   nmr = rates$nmr,
#'   pnmr = rates$pnmr,
#'   anc1survey = rates$anc1,
#'   dpt1survey = rates$penta1,
#'   survey_year = 2021,
#'   twin = rates$twin_rate,
#'   preg_loss = rates$preg_loss
#' )
#' }
#'
#' @export
get_mapping_data <- function(.data,
                             subnational_map = NULL) {
  NAME_1 = NULL

  check_cd_population(.data)

  iso <- attr_or_abort(.data, "iso3")
  admin_level <- attr_or_abort(.data, 'admin_level')
  if (admin_level != 'adminlevel_1') {
    cd_abort(c('x' = 'Only admin 1 data is supported currently.'))
  }

  shapefile <- get_country_shapefile(iso, "admin_level_1")

  shapefile <- if (is.null(subnational_map)) {
    shapefile %>%
      mutate(adminlevel_1 = NAME_1)
  } else {
    shapefile %>%
      left_join(subnational_map, by = "NAME_1") %>%
      rename(adminlevel_1 = admin_level_1)
  }

  merged_data <- .data %>%
    left_join(shapefile, by = join_by(adminlevel_1))

  new_tibble(
    merged_data,
    class = "cd_mapping",
    admin_level = admin_level
  )
}

#' Filter Mapping Data for Specific Indicator and Denominator
#'
#' Extracts and filters indicator coverage data by year, denominator type, and
#' color palette for visualization.
#'
#' @param .data A `cd_mapping` object returned by `get_mapping_data()`.
#' @param indicator Character. Indicator name (e.g., `"anc4"`, `"penta1"`).
#' @param denominator Character. One of: `"dhis2"`, `"anc1"`, `"penta1"`, `"penta1derived"`.
#' @param palette Character. Color palette for mapping.
#'        One of: `"Reds"`, `"Blues"`, `"Greens"`, `"Purples"`, `"YlGnBu"`.
#' @param plot_year Optional integer or vector of years to filter.
#'
#' @return A `tibble` of class `"cd_mapping_filtered"`.
#'
#' @examples
#' \dontrun{
#' filtered <- filter_mapping_data(
#'   data, indicator = "anc4", denominator = "anc1", palette = "Blues", plot_year = 2021
#' )
#' }
#'
#' @export
filter_mapping_data <- function(.data,
                                indicator,
                                denominator = c("dhis2", "anc1", "penta1", "penta1derived"),
                                palette = c("Reds", "Blues", "Greens", "Purples", "YlGnBu"),
                                plot_year = NULL) {
  check_cd_mapping(.data)

  indicator <- arg_match(indicator, get_analysis_indicators())
  denominator <- arg_match(denominator)
  palette <- arg_match(palette)

  column <- paste0("cov_", indicator, "_", denominator)
  data <- .data %>%
    filter(if (is.null(plot_year)) TRUE else year %in% plot_year) %>%
    select(any_of(c('adminlevel_1', 'district', 'year', column)), starts_with('geom')) %>%
    rename_with(~ 'geometry', starts_with('geom'))

  if (!column %in% colnames(data)) {
    cd_abort(c("x" = "{.field {column}} not in the data"))
  }

  new_tibble(
    data,
    class = 'cd_mapping_filtered',
    indicator = indicator,
    column = column,
    palette = palette
  )
}

#' Load Country Shapefile for Mapping
#'
#' Loads the shapefile of administrative boundaries for a given country and level.
#' Ensures geometries are valid and standardized to WGS84 (EPSG:4326).
#'
#' @param country_iso A string with the ISO3 code of the country (e.g., `"KEN"`).
#' @param level A character string specifying the admin level. One of:
#'   - `"admin_level_1"` (default): First-level admin areas
#'   - `"district"`: District-level admin areas
#'
#' @return An `sf` object containing valid geometries for the specified country and level.
#'
#' @examples
#' shapefile <- get_country_shapefile("KEN", level = "admin_level_1")
#'
#' @export
get_country_shapefile <- function(country_iso, level = c("admin_level_1", "district")) {
  check_required(country_iso)
  level <- arg_match(level)

  shapefile_path <- system.file(file.path("shapefiles", "admin_level_1.gpkg"), package = "cd2030.core")
  check_file_path(shapefile_path)

  sf_data <- st_read(shapefile_path, layer = country_iso, quiet = TRUE)

  # Validate geometry and reproject to WGS84 (EPSG:4326)
  sf_data %>%
    st_make_valid() %>%
    st_transform(4326)
}
