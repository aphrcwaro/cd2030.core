subnationalMappingUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('subnational_mapping'),
    dashboardTitle = i18n$t('title_subnational_mapping'),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(3, denominatorInputUI(ns('denominator'), i18n)),
      column(3, selectizeInput(ns('years'), label = i18n$t("title_select_years"), choice = NULL, multiple = TRUE)),
      column(3, selectizeInput(ns('palette'), label = i18n$t("title_palette"), choices = c('Greens', 'Blues', 'Reds')))
    ),

    tabBox(
      title = i18n$t('title_subnational_mapping'),
      width = 12,

      tabPanel(title = i18n$t("opt_anc4"), downloadCoverageUI(ns('anc4'))),
      tabPanel(title = i18n$t("opt_livebirths"), downloadCoverageUI(ns('livebirths'))),
      tabPanel(title = i18n$t("opt_lbw"), downloadCoverageUI(ns('lbw'))),
      tabPanel(title = i18n$t("opt_penta3"), downloadCoverageUI(ns('penta3'))),
      tabPanel(title = i18n$t("opt_mcv1"), downloadCoverageUI(ns('measles1'))),
      tabPanel(
        title = i18n$t("opt_custom_check"),
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = i18n$t("title_indicator"),
                                   choices = c('Select' = '', get_indicator_without_opd_ipd())))
        ),
        downloadCoverageUI(ns('custom'))
      )
    )
  )
}

subnationalMappingServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominatorInputServer('denominator', cache, i18n)

      mapping_dt <- reactive({
        req(cache(), cache()$check_inequality_params)
        cache()$get_mapping_data('adminlevel_1')
      })

      years <- reactive({
        req(cache())
        cache()$mapping_years
      })

      anc4_mapping <- reactive({
        req(mapping_dt(), input$palette)
        mapping_dt() %>%
          filter_mapping_data('anc4', denominator = cache()$get_denominator('anc4'),
                              palette = input$palette, plot_year = years())
      })

      livebirths_mapping <- reactive({
        req(mapping_dt(), input$palette)
        mapping_dt() %>%
          filter_mapping_data('instlivebirths', denominator = cache()$get_denominator('instdlivebirths'),
                              palette = input$palette, plot_year = years())
      })

      lbw_mapping <- reactive({
        req(mapping_dt(), input$palette)
        mapping_dt() %>%
          filter_mapping_data('low_bweight', denominator = cache()$get_denominator('low_bweight'),
                              palette = input$palette, plot_year = years())
      })

      penta3_mapping <- reactive({
        req(mapping_dt(), input$palette)
        mapping_dt() %>%
          filter_mapping_data('penta3', denominator = cache()$get_denominator('penta3'),
                              palette = input$palette, plot_year = years())
      })

      measles1_mapping <- reactive({
        req(mapping_dt(), input$palette)
        mapping_dt() %>%
          filter_mapping_data('measles1', denominator = cache()$get_denominator('measles1'),
                              palette = input$palette, plot_year = years())
      })

      custom_mapping <- reactive({
        req(mapping_dt(), input$palette, input$indicator)
        mapping_dt() %>%
          filter_mapping_data(input$indicator, denominator = cache()$get_denominator(input$indicator),
                              palette = input$palette, plot_year = years())
      })

      observe({
        req(cache(), cache()$adjusted_data)

        survey_years <- cache()$adjusted_data %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)

        survey_years <- c('All years' = '', survey_years)
        updateSelectizeInput(session, 'years', choices = survey_years, selected = years())
      })

      observeEvent(input$years, {
        req(cache())
        cache()$set_mapping_years(as.integer(input$years))
      })

      downloadCoverageServer(
        id = 'anc4',
        filename = reactive(paste0('anc4_adminlevel_1_map_', cache()$get_denominator('anc4'))),
        data_fn = anc4_mapping,
        sheet_name = reactive(i18n$t("title_anc4")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'livebirths',
        filename = reactive(paste0('livebirths_adminlevel_1_map_', cache()$get_denominator('instlivebirths'))),
        data_fn = livebirths_mapping,
        sheet_name = reactive(i18n$t("title_livebirths")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'lbw',
        filename = reactive(paste0('lbw_adminlevel_1_map_', cache()$get_denominator('low_bweight'))),
        data_fn = lbw_mapping,
        sheet_name = reactive(i18n$t("title_lbw")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'penta3',
        filename = reactive(paste0('penta3_adminlevel_1_map_', cache()$get_denominator('penta3'))),
        data_fn = penta3_mapping,
        sheet_name = reactive(i18n$t("title_penta3")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'measles1',
        filename = reactive(paste0('measles1_adminlevel_1_map_', cache()$get_denominator('measles1'))),
        data_fn = measles1_mapping,
        sheet_name = reactive(i18n$t("title_measles1")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'custom',
        filename = reactive(paste0(input$indicator, '_adminlevel_1_map_', cache()$get_denominator(input$indicator))),
        data_fn = custom_mapping,
        sheet_name = reactive(paste(input$indicator, i18n$t("title_coverage"))),
        i18n = i18n
      )

      contentHeaderServer(
        'subnational_mapping',
        cache = cache,
        path = 'national-subnational-mapping',
        i18n = i18n
      )
    }
  )
}
