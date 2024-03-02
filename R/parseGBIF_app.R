#' @title parseGBIF App
#' @name parseGBIF_app
#'
#' @description parseGBIF App
#' @return CSV files
#' @author Pablo Hendrigo Alves de Melo,
#        Nadia Bystriakova &
#        Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @examples
#' \donttest{
#' parseGBIF_app()
#' }
#' @import leaflet
#' @import shiny
#' @import rnaturalearth
#' @import dplyr
#' @import readr
#' @import shinydashboard
#' @import DT
#' @import rhandsontable
#' @import shinyWidgets
#' @export
parseGBIF_app <- function()
{

  {
    wd <- rnaturalearth::ne_countries(returnclass = "sf")

    map_on <- leaflet() %>%
      addTiles()

    # https://github.com/rstudio/leaflet/blob/main/inst/examples/proj4Leaflet.R
    map_on_moll <- leaflet(options =
                             leafletOptions(maxZoom = 5,
                                            crs = leafletCRS(crsClass = "L.Proj.CRS", code = "ESRI:53009",
                                                             proj4def = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs",
                                                             resolutions = c(65536, 32768, 16384, 8192, 4096, 2048)
                                            )),
                           width = '800px', height = '600px') %>%
      # addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
      # addProviderTiles("Esri.WorldImagery", group = 'WorldImagery') %>%
      # addProviderTiles("Stamen.Terrain", group = 'Terrain') %>%
      # addLayersControl(
      #   # overlayGroups = c("Vértice EOO", "Validados", "Invalidados"),
      #   baseGroups = c("Mapnik", "WorldImagery", "Terrain")) %>%
      addPolygons(data = wd,
                  fillOpacity = 0,
                  weight = 2,
                  color = "black")


  }


  path_data <- getwd()


  # plumber::plumb(file='C:/Dados/Kew/checkName_WCVP/plumber.R')$run()
  # Running plumber API at http://127.0.0.1:5750

  # 0 - Preparar ambiente R
  {

    # carregar pacotes básicos
    {


      # install.packages('tidyr', dependencies = TRUE)
      # library(tidyr)

      # # install.packages('biogeo', dependencies = TRUE)
      # library(biogeo)

      # install.packages('readr', dependencies = TRUE)

      # install.packages('stringr', dependencies = TRUE)
      # library(stringr)

      # # install.packages('devtools', dependencies = TRUE)
      # library(devtools)
      #
      # # devtools::install_github("ropensci/CoordinateCleaner")
      # # library(CoordinateCleaner)
      #
      # # install.packages('textclean', dependencies = TRUE)
      # library(textclean)
      #
      # # install.packages('googledrive', dependencies = TRUE)
      # library(googledrive)
      #
      # # install.packages('rvest', dependencies = TRUE)
      # library(rvest)
      #
      # # # install.packages('flora', dependencies = TRUE)
      # # library(flora)
      #
      # # install.packages('raster', dependencies = TRUE)
      # library(raster)
      #
      # # install.packages('sp', dependencies = TRUE)
      # library(sp)

      # install.packages('lubridate', dependencies = TRUE)
      # library(lubridate)

      # install.packages('rnaturalearthdata', dependencies = TRUE)
      # library(rnaturalearthdata)

      # # install.packages('geobr', dependencies = TRUE)
      # library(geobr)
      #
      # # install.packages('monographaR', dependencies = TRUE)
      # library(monographaR)

      # install.packages('jsonlite', dependencies = TRUE)
      # library(jsonlite)

      # install.packages('sqldf', dependencies = TRUE)
      # library(sqldf)

      # install.packages('shiny', dependencies = TRUE)

      # install.packages('shinydashboardPlus', dependencies = TRUE)
      # library(shinydashboardPlus)

      # install.packages('shinydashboard', dependencies = TRUE)

      # # install.packages('mapview', dependencies = TRUE)
      # library(mapview)
      #
      # # install.packages('DT', dependencies = TRUE)

      # install.packages('rhandsontable', dependencies = TRUE)
      # library(rhandsontable) # tabela editavel
      #
      # # install.packages('shinyWidgets', dependencies = TRUE)
      # library(shinyWidgets) # botoes
      #
      # # install.packages('measurements', dependencies = TRUE)
      # library(measurements)

      # # install.packages('downloader', dependencies = TRUE)
      # library(downloader)

      # renv::install("pablopains/parseGBIF")
      # renv::install("matildabrown/rWCVPdata")


      # library(parseGBIF)
      # library(rWCVPdata)
    }




    options(shiny.maxRequestSize=10000*1024^2)

    # # devtools::load_all(devtools::as.package('c:/app/r'))
    #
    # source('r/collectors_get_name.R')
    # source('r/collectors_prepare_dictionary.R')
    # source('r/EnumOccurrenceIssue.R')
    # # source'c:/app/r/export_data.R')
    # source('r/export_data_v2.3.R')
    # source('r/extract_gbif_issue.R')
    # source('r/generate_collection_event_key.R')
    # source('r/parseGBIF_summary.R')
    # source('r/prepare_gbif_occurrence_data.R')
    # # source('c:/app/r/select_digital_voucher.R')
    # source('r/select_digital_voucher_v2.1.R')
    # source('r/select_gbif_fields.R')
    # source('r/standardize_scientificName.R')
    # source('r/wcvp_check_name.R')
    # source('r/wcvp_check_name_batch.R')
    # source('r/wcvp_get_data.R')
    # source('r/wcvp_get_data_v2.1.R')

    # r <- getOption("repos")
    # r["matildabrown"] <- "https://matildabrown.github.io/drat"
    # options(repos=r)
    # install.packages("rWCVP")
    # install.packages("rWCVPdata", dependencies = TRUE)
    #
    # install.packages("rWCVPdata", repos=c("https://matildabrown.github.io/drat", "https://cloud.r-project.org"))
    #
    # library(rWCVPdata)


  }

  # app
  {
    # DT editável
    {
      dt_output = function(id) {
        fluidRow(column(
          12,
          DTOutput(id)
        ))
      }

      render_dt = function(data, editable = 'cell', server = TRUE, ...) {
        renderDT(data, selection = 'none', server = server, editable = editable, ...)
      }
    }

    {

      occ <<- {}
      spp <<- {}

      spp_list <<- {}

      sp_sel <<- ''

      dataset_map_list <<- {}


    }

    # Tela APP--
    ui <-
      {
        dashboardPage(
          dashboardHeader(title = "parseGBIF APP"),
          dashboardSidebar(width = 350,
                           collapsed = TRUE,

                           box(status = "primary", width = 12,
                               title = 'Lista de espécies', background = 'navy', # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

                               # selectInput("sp", label = 'Selecione uma espécie:',choices = spp$FB2020_AcceptedNameUsage),

                               actionButton("selectBtn", "Selecionar espécie", icon = icon("play")),
                           )
          ),

          dashboardBody(
            navbarPage("parseGBIF workflow",

                       # navbarPage("Global strategy for plant exploration - Framework for filling in the gaps in our knowledge of plant diversity", #"Global strategy for plant exploration",
                       ###
                       # WF
                       {tabPanel(icon("house"),
                                 box(title = "1. GBIF data preparation",
                                     status = "primary",
                                     width = 12,

                                     # 1.1. Getting occurrence data of the species records from GBIF
                                     {box(title = "1.1. Getting occurrence data of the species records from GBIF",
                                          status = "primary",
                                          width = 12,

                                          fluidRow(
                                            column(width = 12,
                                                   shiny::tags$a('GBIF | Global Biodiversity Information Facility', href = 'https://www.gbif.org/occurrence/search?occurrence_status=present&q='),
                                                   h5('1.1.1. Access a registered account in GBIF'),
                                                   h5('1.1.2. Filter occurrences using available fields, for instance:'),
                                                   h6('   Basis of record: Preserved specimen'),
                                                   h6('   Occurrence status: present'),
                                                   h6('   Scientific name: Botanical family name (e.g. Achatocarpaceae) or filter by other fields'),
                                                   h5('1.1.3. Request to download information in DARWIN CORE ARCHIVE FORMAT'),
                                                   h5('1.1.4. Download compressed file')
                                            ))
                                     )},

                                     # 1.2. Preparing occurrence data downloaded from GBIF
                                     {box(title = "1.2. Preparing occurrence data downloaded from GBIF",
                                          status = "primary",
                                          width = 12,
                                          fluidRow(
                                            column(width = 12,
                                                   fileInput(inputId = "gbifFile",
                                                             label = "Upload compressed file / TXT occurrence file(s) downloaded from GBIF ou CSV parseGBIF occurrence data file",
                                                             multiple = TRUE))),
                                          br(),
                                          fluidRow(
                                            column(width = 12,
                                                   DT::dataTableOutput('gbifContents'))),

                                          box(title = "Data summary",
                                              status = "primary",
                                              width = 12,
                                              fluidRow(
                                                column(width = 12,
                                                       # textOutput("DataSummary_text"))
                                                       verbatimTextOutput("DataSummary_text"))),

                                              br(),

                                              fluidRow(
                                                column(width = 12,
                                                       downloadButton("occGBIFDownload", "Download parseGBIF occurrence data"))),

                                          )
                                     )},

                                     # 1.3. Extracting GBIF issues
                                     {box(title = "1.3. Extracting GBIF issues",
                                          status = "primary",
                                          width = 12,

                                          fluidRow(
                                            column(width = 12,
                                                   actionButton("extractIssueBtn", "Extract issues", icon = icon("play"))
                                            )),


                                          br(),
                                          fluidRow(
                                            column(width = 12,
                                                   tabsetPanel(
                                                     tabPanel("Summary GBIF issues", DT::dataTableOutput( 'issueSummaryContents'),
                                                              br(),
                                                              downloadButton("issueSummaryDownload", "Download summary GBIF issues")),
                                                     tabPanel("Results GBIF issues", DT::dataTableOutput("issueOccurrenceContents"),
                                                              br(),
                                                              downloadButton("issueOccurrenceDownload", "Download results GBIF issues"))
                                                   )))

                                          # fluidRow(
                                          #   column(width = 6,
                                          #          downloadButton("issueSummaryDownload", "Download summary GBIF issues")),
                                          #   column(width = 6,
                                          #          downloadButton("issueOccurrenceDownload", "Download results GBIF issues"))
                                          #   )
                                     )},

                                     # 2. Check species names against WCVP database
                                     {box(title = "2. Check species names against WCVP database",
                                          status = "primary",
                                          width = 12,

                                          # fluidRow(
                                          #   column(width = 12,
                                          #          h4('If you are running the app from shinyapps.io, first upload the World Checklist of Vascular Plants (WCVP) CSV file wcvp_names'),
                                          #          fileInput(inputId = "loadWCVPFile",
                                          #                    label = "CSV file wcvp_names",
                                          #                    multiple = FALSE))
                                          # ),

                                          fluidRow(
                                            column(width = 12,
                                                   # h4('If you are running the app locally in rStudio, the the World Checklist of Vascular Plants (WCVP) databese will be automatically downloaded from source'),
                                                   actionButton("applyWCVPBtn", "Check species names", icon = icon("play")))
                                          ),

                                          # fluidRow(
                                          #   column(width = 12,
                                          #         verbatimTextOutput("WCPVSummary_text"))),

                                          fluidRow(
                                            column(width = 12,
                                                   verbatimTextOutput("DataSummary_wcvp_text"))),



                                          br(),

                                          fluidRow(
                                            column(width = 12,
                                                   tabsetPanel(
                                                     tabPanel("Summary check species names",
                                                              DT::dataTableOutput( 'wcvpSummaryContents'),
                                                              br(),
                                                              downloadButton("wcvpSummaryDownload", "Download summary check species names")),
                                                     tabPanel("Results check species names",
                                                              DT::dataTableOutput("wcvpOccurrenceContents"),
                                                              br(),
                                                              downloadButton("wcvpOccurrenceDownload", "Download results check species names"))
                                                   )))

                                     )},

                                     # 3. Collectors Dictionary
                                     {box(title = "3. Collectors Dictionary",
                                          status = "primary",
                                          width = 12,

                                          # 3.1 Prepare dictionary collectors
                                          h4('3.1 Prepare dictionary collectors'),

                                          fluidRow(
                                            column(width = 6,
                                                   br(),
                                                   # br(),
                                                   actionButton("getCollectorsDictionaryFromDatasetBtn", "Prepare dictionary collectors", icon = icon("play"))
                                            ),

                                            column(width = 6,
                                                   fileInput(inputId = "collectorsDictionaryFromDatasetFile",
                                                             label = "Collector's Dictionary File. If empty, the package's default file will be used.",
                                                             multiple = FALSE))
                                          ),

                                          # br(),
                                          # fluidRow(
                                          #   column(width = 12,
                                          #          fileInput(inputId = "collectorsDictionaryFromDatasetFile",
                                          #                    label = "Collector's Dictionary File. Point to a file on your local disk, if empty, the package's default file will be used.",
                                          #                    multiple = FALSE)
                                          #   )),


                                          # 3.2 Check the main collector’s last name
                                          h4('3.2 Check the main collector’s last name'),

                                          fluidRow(
                                            column(width = 12,
                                                   # DT editável,
                                                   dt_output('collectorsDictionaryFromDatasetContents')
                                            )),

                                          fluidRow(
                                            column(width = 12,
                                                   downloadButton("collectorsDictionaryFromDatasetDownload", "Download dictionary collectors")
                                            )),

                                          br(),
                                          # 3.3 Generating the collection event key
                                          h4('3.3 Generating the collection event key'),


                                          fluidRow(
                                            column(width = 12,
                                                   actionButton("applyCollectorsDictionaryBtn", "Generating the collection event key", icon = icon("play"))

                                            )),

                                          br(),
                                          fluidRow(
                                            column(width = 12,
                                                   tabsetPanel(
                                                     tabPanel("Summary collection event key",
                                                              DT::dataTableOutput( 'collectorsDictionarySummaryContents'),
                                                              br(),
                                                              downloadButton("summary_collectorsDictionaryDownload", "Download summary collectors dictionary")), #'applyCollectorsDictionaryContents')),
                                                     tabPanel("Results collection event key",
                                                              DT::dataTableOutput("collectorsDictionaryOccurrenceContents"),
                                                              br(),
                                                              downloadButton("results_collectorsDictionaryDownload", "Download results collectors dictionary")),
                                                     tabPanel("Collectors Dictionary add",
                                                              DT::dataTableOutput("collectorsDictionaryNewContents"),
                                                              br(),
                                                              downloadButton("add_collectorsDictionaryDownload", "Download collectors dictionary add"))
                                                   )
                                            ))

                                     )},

                                     # 4. Selecting the master digital voucher
                                     {box(title = "4. Selecting the master digital voucher",
                                          status = "primary",
                                          width = 12,

                                          fluidRow(
                                            column(width = 12,
                                                   actionButton("selectDigitalVoucherBtn", "Select digital voucher", icon = icon("play"))

                                            )),


                                          br(),
                                          fluidRow(
                                            column(width = 12,
                                                   tabsetPanel(
                                                     tabPanel("Summary",
                                                              DT::dataTableOutput( 'SummaryDigitalVoucherContents'),
                                                              br(),
                                                              downloadButton("digitalVoucherDownload", "Download summary")),
                                                     tabPanel("Results",
                                                              DT::dataTableOutput("digitalVoucherContents"),
                                                              br(),
                                                              downloadButton("occdigitalVoucherDownload", "Download results"))
                                                   )))


                                          # fluidRow(
                                          #   column(width = 6,
                                          #          downloadButton("digitalVoucherDownload", "Download summary")),
                                          #   column(width = 6,
                                          #          downloadButton("occdigitalVoucherDownload", "Download results"))
                                          # )


                                          # column(width = 12,
                                          #        # downloadButton("issueSummaryDownload", "Download summary"),
                                          #        downloadButton("digitalVoucherDownload", "Download results")
                                          # ))
                                     )},

                                     # 5. Export of results
                                     {box(title = "5. Merge and export of results",
                                          status = "primary",
                                          width = 12,

                                          fluidRow(
                                            column(width = 12,
                                                   actionButton("exportBtn", " Merge and export", icon = icon("play"))

                                            )),


                                          br(),
                                          fluidRow(
                                            column(width = 12,

                                                   tabsetPanel(
                                                     tabPanel("Useable Data",
                                                              DT::dataTableOutput( 'useable_data_mergeContents'),
                                                              br(),
                                                              downloadButton("useable_data_mergeDownload", "Download useable data")),

                                                     tabPanel("Duplicates",
                                                              DT::dataTableOutput("duplicatesContents"),
                                                              br(),
                                                              downloadButton("duplicatesDownload", "Download duplicates")),

                                                     tabPanel("Useable Data",
                                                              DT::dataTableOutput( 'unusable_data_mergeContents'),
                                                              br(),
                                                              downloadButton("unusable_data_mergeDownload", "Download unusable data")),

                                                     tabPanel("All Data",
                                                              DT::dataTableOutput( 'all_dataContents'),
                                                              br(),
                                                              downloadButton("all_dataDownload", "Download all data"))



                                                   )))

                                     )},

                                     # 5. Summary of results
                                     {box(title = "6. Summary of results",
                                          status = "primary",
                                          width = 12,

                                          fluidRow(
                                            column(width = 12,
                                                   actionButton("summaryBtn", " Summary parseGBIF results", icon = icon("play"))

                                            )),

                                          br(),
                                          fluidRow(
                                            column(width = 12,
                                                   tabsetPanel(
                                                     tabPanel("General summary",
                                                              DT::dataTableOutput( 'parseGBIF_general_summaryContents'),
                                                              br(),
                                                              downloadButton("parseGBIF_general_summaryDownload", "Download general summary")),

                                                     tabPanel("Merge fields summary",
                                                              DT::dataTableOutput("parseGBIF_merge_fields_summaryContents"),
                                                              br(),
                                                              downloadButton("parseGBIF_merge_fields_summaryDownload", "Download merge fields summary")),

                                                     tabPanel("Merge fields summary - useable data",
                                                              DT::dataTableOutput( 'parseGBIF_merge_fields_summary_useable_dataContents'),
                                                              br(),
                                                              downloadButton("parseGBIF_merge_fields_summary_useable_dataDownload", "Download merge fields summary - useable data")),

                                                     tabPanel("Merge fields summary - unusable data",
                                                              DT::dataTableOutput( 'parseGBIF_merge_fields_summary_unusable_dataContents'),
                                                              br(),
                                                              downloadButton("parseGBIF_merge_fields_summary_unusable_dataDownload", "Download merge fields summary - useable data"))

                                                   )))

                                     )},


                                 ),

                                 # mais abas - analises, mapas, graficos...

                                 ##
                       )},

                       # maps
                       tabPanel(icon("location-dot"),
                                box(title = "2. Maps",
                                    status = "primary",
                                    width = 12,


                                    box(status = "primary", width = 12,
                                        title = 'parseGBIF result file',# background = 'navy', # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

                                        fileInput(inputId = "parseGBIFFile",
                                                  label = "Upload CSV parseGBIF result file",
                                                  multiple = FALSE),


                                        # verbatimTextOutput('parseGBIFText', placeholder = FALSE)


                                    ),


                                    box(status = "primary", width = 12,
                                        title = 'Lista de espécies',# background = 'navy', # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

                                        selectInput("sp_map", label = 'Selecione uma espécie:',
                                                    choices = spp_list,
                                                    selected = spp_list[1]),

                                        selectInput("sel_dataset_map", label = 'Tipo registro:',
                                                    choices = c('useable','duplicate','unusable'),
                                                    multiple = TRUE,
                                                    selected = c('useable','duplicate','unusable')),
                                        br(),
                                        br(),
                                        br(),
                                        br(),

                                        box(status = "primary", width = 12,
                                            title = 'Unique Collection Events',# background = 'navy', # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.


                                            navbarPage("Unique Collection Events",
                                                       tabPanel(icon("location-dot"),
                                                                box(title = "Maps",
                                                                    status = "primary",
                                                                    width = 12,

                                                                    selectInput("projetcion_map", label = 'Projeção:',
                                                                                choices = c('Mercator','Mollweide'),
                                                                                multiple = FALSE,
                                                                                selected = 'Mercator'),
                                                                    br(),

                                                                    leafletOutput("parseGBIFMap", width = "100%", height = "550px")

                                                                )),

                                                       tabPanel(icon("table"),
                                                                box(title = "Table",
                                                                    status = "primary",
                                                                    width = 12,

                                                                    uiOutput("picker"),


                                                                    DT::dataTableOutput('occ_parseGBIFContents')

                                                                )))
                                        ),



                                        DT::dataTableOutput('parseGBIFContents'),

                                    ),



                                )))

          ))}


    # Server
    server <- function(input, output, session)
    {

      # maps
      {

        output$picker <- renderUI({
          req(input$parseGBIFFile)
          pickerInput(inputId = 'pick',
                      label = 'Choose',
                      choices = colnames(occ),
                      options = list(`actions-box` = TRUE),
                      multiple = T,
                      selected = c('parseGBIF_dataset_result',
                                   'parseGBIF_sample_taxon_name_status',
                                   'parseGBIF_digital_voucher',
                                   'parseGBIF_merged',
                                   'Ctrl_key_family_recordedBy_recordNumber',
                                   'parseGBIF_wcvp_family',
                                   'parseGBIF_sample_taxon_name',
                                   'parseGBIF_useful_for_spatial_analysis',
                                   'parseGBIF_decimalLatitude',
                                   'parseGBIF_decimalLongitude',
                                   'parseGBIF_digital_voucher',
                                   'parseGBIF_duplicates',
                                   'parseGBIF_num_duplicates',
                                   'parseGBIF_duplicates_grouping_status',

                                   'Ctrl_eventDate',

                                   'parseGBIF_countryCode_ISO3',
                                   'parseGBIF_countryName_en',

                                   'Ctrl_stateProvince',
                                   'Ctrl_municipality',
                                   'Ctrl_county',
                                   'Ctrl_locality',

                                   'Ctrl_fieldNotes',
                                   'Ctrl_eventRemarks',

                                   'parseGBIF_coordinate_status',

                                   '.val',
                                   '.zer',
                                   '.sea',
                                   '.equ',
                                   '.cen',
                                   '.cap',
                                   '.urb',
                                   '.con',
                                   '.inst',
                                   '.dup',
                                   '.coordinates_outOfRange',
                                   'parseGBIF_GADM_centroids',
                                   'parseGBIF_GADM_centroids_level',

                                   'n_taxon_name_11_1_km',
                                   'n_unique_collection_event_11_1_km',
                                   'n_taxon_name_1_1_km',
                                   'n_unique_collection_event_1_1_km',
                                   'n_taxon_name_110m',
                                   'n_unique_collection_event_110m',
                                   'n_taxon_name_11m',
                                   'n_unique_collection_event_11m'

                                   ))



        })

        parsegbifLoad <- reactive({
          req(input$parseGBIFFile)
          tryCatch(
            {

              withProgress(message = 'Processing...', style = 'notification', value = 0.1,
                           {

                             occ <<- read.csv(file = input$parseGBIFFile$datapath, #'C:\\Dados\\Kew\\Bolivia\\dataGBIF\\parseGBIF_7_cc.csv',#input$parseGBIFFile$datapath, #'C:\\Users\\Pablo Hendrigo\\Downloads\\parseGBIF_5_occ_all_data.csv'
                                              fileEncoding = "UTF-8") %>%
                               dplyr::select(parseGBIF_dataset_result,
                                             Ctrl_key_family_recordedBy_recordNumber,

                                             parseGBIF_useful_for_spatial_analysis,
                                             parseGBIF_sample_taxon_name_status,
                                             parseGBIF_wcvp_family,
                                             parseGBIF_sample_taxon_name,

                                             parseGBIF_digital_voucher,
                                             parseGBIF_duplicates,
                                             parseGBIF_num_duplicates,
                                             parseGBIF_duplicates_grouping_status,
                                             parseGBIF_merged,

                                             Ctrl_eventDate,

                                             parseGBIF_countryCode_ISO3,
                                             parseGBIF_countryName_en,
                                             Ctrl_countryCode,
                                             Ctrl_stateProvince,
                                             Ctrl_municipality,
                                             Ctrl_county,
                                             Ctrl_locality,

                                             parseGBIF_decimalLatitude,
                                             parseGBIF_decimalLongitude,

                                             Ctrl_fieldNotes,
                                             Ctrl_eventRemarks,

                                             parseGBIF_GADM_centroids,
                                             parseGBIF_GADM_centroids_level,
                                             parseGBIF_coordinate_status,
                                             .coordinates_outOfRange,
                                             .val,.zer,.sea,.equ,.cen,.cap,.urb,.con,.inst,.dup,

                                             point_11_1_km,
                                             n_taxon_name_11_1_km,
                                             n_unique_collection_event_11_1_km,
                                             point_1_1_km,
                                             n_taxon_name_1_1_km,
                                             n_unique_collection_event_1_1_km,
                                             point_110m,
                                             n_taxon_name_110m,
                                             n_unique_collection_event_110m,
                                             point_11m,
                                             n_taxon_name_11m,
                                             n_unique_collection_event_11m,

                                             Ctrl_gbifID,
                                             Ctrl_bibliographicCitation,
                                             Ctrl_language,
                                             Ctrl_institutionCode,
                                             Ctrl_collectionCode,
                                             Ctrl_datasetName,
                                             Ctrl_basisOfRecord,
                                             Ctrl_catalogNumber,
                                             Ctrl_recordNumber,
                                             Ctrl_recordedBy,
                                             Ctrl_occurrenceStatus,

                                             Ctrl_year,
                                             Ctrl_month,
                                             Ctrl_day,
                                             Ctrl_habitat,

                                             Ctrl_level0Name,
                                             Ctrl_level1Name,
                                             Ctrl_level2Name,
                                             Ctrl_level3Name,
                                             Ctrl_identifiedBy,
                                             Ctrl_dateIdentified,
                                             Ctrl_scientificName,
                                             Ctrl_taxonRank,
                                             Ctrl_decimalLatitude,
                                             Ctrl_decimalLongitude,
                                             Ctrl_nameRecordedBy_Standard,
                                             Ctrl_recordNumber_Standard,

                                             Ctrl_geospatial_quality,
                                             Ctrl_verbatim_quality,
                                             Ctrl_moreInformativeRecord,
                                             Ctrl_coordinates_validated_by_gbif_issue,
                                             wcvp_plant_name_id,
                                             wcvp_taxon_rank,
                                             wcvp_taxon_status,
                                             wcvp_family,
                                             wcvp_taxon_name,
                                             wcvp_taxon_authors,
                                             wcvp_reviewed,
                                             wcvp_searchedName,
                                             wcvp_searchNotes,

                                             parseGBIF_number_taxon_names,
                                             parseGBIF_non_groupable_duplicates,
                                             parseGBIF_unidentified_sample,
                                             parseGBIF_wcvp_plant_name_id,
                                             parseGBIF_wcvp_taxon_rank,
                                             parseGBIF_wcvp_taxon_status,
                                             parseGBIF_wcvp_taxon_name,
                                             parseGBIF_wcvp_taxon_authors,
                                             parseGBIF_wcvp_reviewed,

                                             parseGBIF_freq_duplicate_or_missing_data,
                                             parseGBIF_duplicates_map,
                                             parseGBIF_merged_fields
                               )


                             base_url <- "https://www.gbif.org/occurrence/search?gbif_id=%s"
                             url <- sprintf(base_url,occ$Ctrl_gbifID)

                             # print(url)

                             # _blank
                             # occ$gbif_url <- paste0("<a href='", url, "' target='_blank'>", url,"</a>")

                             occ$gbif_url <- url

                             # occ$gbif_url <-  a(occ$Ctrl_key_family_recordedBy_recordNumbe, url, target="_blank")

                             # occ$gbif_url <- shiny::tags$a('GBIF | Global Biodiversity Information Facility', href = "'", url,"'")

                             # occ$gbif_url <- paste0("<a href='", url, "' target='_blank'>", occ$Ctrl_key_family_recordedBy_recordNumber,"</a>")
                             #
                             #                              link <- paste0('<div class="alert alert-primary" role="alert"> ',
                             #                                             "GBIF record: <a href='", url, "'target='_blank'> ", occurrenceID," </a></div>")


                             spp <<- occ %>%
                               dplyr::select(parseGBIF_wcvp_family, parseGBIF_sample_taxon_name) %>%
                               dplyr::distinct(parseGBIF_wcvp_family, parseGBIF_sample_taxon_name ) %>%
                               dplyr::arrange_all()

                             spp_list <<- as.character(spp$parseGBIF_sample_taxon_name)

                             updateSelectInput(session = session,
                                               inputId = "sp_map",
                                               choices = spp_list,
                                               selected = tail(spp_list, 1))

                             incProgress(1, detail = 'ok')
                           })

              showModal(modalDialog(
                title = "All ready!",
                paste0("Select a species to display 'Unique Collection Events' and their duplicates on the map and table."),
                easyClose = TRUE,
                footer = NULL
              ))

              return(occ)
            },
            error = function(e) {
              stop(safeError(e))
            }
          )
        })


        output$occ_parseGBIFContents <- DT::renderDataTable(options = list(
          columnDefs = list(list(className = 'dt-center', targets = 5)),
          pageLength = 5,
          lengthMenu = c(5, 15, 50, 100),
          scrollX = TRUE),
          {
            req(input$sp_map)

            occ %>%
              dplyr::filter(parseGBIF_sample_taxon_name == input$sp_map &
                              parseGBIF_dataset_result %in% c(input$sel_dataset_map)) %>%
              dplyr::select(input$pick) %>%
              dplyr::arrange_at(c('Ctrl_key_family_recordedBy_recordNumber', 'parseGBIF_dataset_result'))#desc('parseGBIF_dataset_result'))



          })


        output$parseGBIFContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                        {
                                                          req(input$parseGBIFFile)

                                                          occ <<- parsegbifLoad()

                                                          summ <- parseGBIF::parseGBIF_summary(parseGBIF_all_data = occ)

                                                          summ$parseGBIF_general_summary

                                                          # data()
                                                        })

        # output$parseGBIFText <- renderText({NRO})


        output$parseGBIFMap <- leaflet::renderLeaflet({
          req(input$sp_map)
          # req(input$parseGBIFFile)

          # print(input$sp_map)
          # print(input$sel_dataset_map)


          dt <- occ %>%
            dplyr::filter(parseGBIF_useful_for_spatial_analysis == TRUE &
                            parseGBIF_sample_taxon_name == input$sp_map &
                            parseGBIF_dataset_result %in% c(input$sel_dataset_map)) %>%
            dplyr::arrange_at('parseGBIF_dataset_result')


          if (input$projetcion_map == 'Mollweide')
          {
            m <- map_on_moll
          }else
          {
            m <- map_on

          }


          dt$cor <- rep('blue', NROW(dt))

          dt$cor <- ifelse(dt$parseGBIF_dataset_result=='duplicate', 'pink',dt$cor)
          dt$cor <- ifelse(dt$parseGBIF_dataset_result=='unusable', 'red',dt$cor)
          dt$cor <- ifelse(dt$parseGBIF_dataset_result=='useable', 'darkblue',dt$cor)


          # dt$cor <- ifelse(dt$.sea==FALSE, 'darkred',dt$cor)

          # index_geo_issue <- occ$.cen == FALSE |
          #   occ$.cap == FALSE |
          #   occ$.urb == FALSE |
          #   occ$.con == FALSE |
          #   occ$.inst == FALSE


          # icons <- awesomeIcons(icon = ifelse(dt$.summary_coordinate_check==TRUE,"home", "whatever"),
          icons <- awesomeIcons(icon = "whatever",
                                iconColor = "black",
                                library = "ion",
                                markerColor = dt$cor)

          # print('aqui1')

          # icon_geo_issue <- awesomeIcons(icon = "home",
          #                                iconColor = "black",
          #                                library = "ion",
          #                                markerColor = dt$cor)
          #
          # print('aqui2')
          #

          #   print(paste0('.val', '-', occ_cc %>% dplyr::filter(.val == FALSE) %>% NROW()))
          # print(paste0('.zer', '-', occ_cc %>% dplyr::filter(.zer == FALSE) %>% NROW()))
          # print(paste0('.cen', '-', occ_cc %>% dplyr::filter(.cen == FALSE) %>% NROW()))
          # print(paste0('.equ', '-', occ_cc %>% dplyr::filter(.equ == FALSE) %>% NROW()))
          # print(paste0('.cap', '-', occ_cc %>% dplyr::filter(.cap == FALSE) %>% NROW()))
          # print(paste0('.urb', '-', occ_cc %>% dplyr::filter(.urb == FALSE) %>% NROW()))
          # print(paste0('.con', '-', occ_cc %>% dplyr::filter(.con == FALSE) %>% NROW()))
          # print(paste0('.inst', '-', occ_cc %>% dplyr::filter(.inst == FALSE) %>% NROW()))
          # print(paste0('.dup', '-', occ_cc %>% dplyr::filter(.dup == FALSE) %>% NROW()))
          # print(paste0('.sea', '-', occ_cc %>% dplyr::filter(.sea == FALSE) %>% NROW()))


          label <- paste0(dt$parseGBIF_dataset_result, ' - ', dt$Ctrl_key_family_recordedBy_recordNumber,
                          ' - ', dt$parseGBIF_wcvp_family, ' - ', dt$parseGBIF_sample_taxon_name,
                          ' - ', dt$Ctrl_year,
                          ' - ', dt$Ctrl_county)

          # geo_issue <- dt$.val & dt$.equ & dt$.zer
          # geo_issue_urb <- dt$.cen & dt$.cap & dt$.urb & dt$.inst & dt$.con & dt$.sea

          geo_issue <- !(dt$.val==FALSE | dt$.equ==FALSE | dt$.zer==FALSE | dt$.coordinates_outOfRange==FALSE)
          geo_issue_urb <- !(dt$.cen==FALSE | dt$.cap==FALSE | dt$.urb==FALSE | dt$.inst==FALSE | dt$.con==FALSE | dt$.sea==FALSE | dt$parseGBIF_GADM_centroids==FALSE)

          # https://getbootstrap.com/docs/5.3/components/alerts/
          etiquetaTextoBtn <- paste0('Unique Collection Event: <b>', dt$Ctrl_key_family_recordedBy_recordNumber,'</b></br>',
                                     dt$parseGBIF_dataset_result,' - Duplicates ',' (',dt$parseGBIF_num_duplicates-1,') - ',dt$parseGBIF_duplicates_grouping_status,'</br>',
                                      dt$parseGBIF_wcvp_family, ' - <b>', dt$parseGBIF_sample_taxon_name,'</b></br>',
                                     'Year: ',dt$Ctrl_year,'</br>',
                                     'Locality: ',ifelse(is.na(dt$Ctrl_locality),'',dt$Ctrl_locality),' - ',
                                     ifelse(is.na(dt$Ctrl_municipality),'',dt$Ctrl_municipality), ' - ',ifelse(is.na(dt$Ctrl_stateProvince),'',dt$Ctrl_stateProvince), ' - ', ifelse(is.na(dt$Ctrl_county),'',dt$Ctrl_county),'</br>',
                                     ifelse(is.na(dt$Ctrl_level3Name),'',dt$Ctrl_level3Name), ' - ',ifelse(is.na(dt$Ctrl_level2Name),'',dt$Ctrl_level2Name), ' - ',ifelse(is.na(dt$Ctrl_level1Name),'',dt$Ctrl_level1Name), ' - ', ifelse(is.na(dt$Ctrl_level0Name),'',dt$Ctrl_level0Name),' (',
                                     ifelse(is.na(dt$parseGBIF_countryCode_ISO3),'',dt$parseGBIF_countryCode_ISO3), ' - ',ifelse(is.na(dt$parseGBIF_countryName_en),'',dt$parseGBIF_countryName_en),')</br>',

                                     "<a href='", dt$gbif_url, "' target='_blank'><b>", dt$gbif_url,"</b></a></br>",
                                     '</br>',

                                     '<div class=',ifelse(geo_issue==FALSE,'"alert alert-danger"',ifelse(geo_issue_urb==FALSE,'"alert alert-warning"','"alert alert-success"')),' role="alert"> Questions about geographic coordinates ',unique(dt$parseGBIF_coordinate_status) ,' </div>',
                                     # '<div class="alert alert-danger" role="alert"> Possibly Artificial or Attributed later </div>',
                                     'val: ',dt$.val,"  /  ", 'equ: ',dt$.equ,"  /  ",'zer: ',dt$.zer,"  /  ", 'con: ',dt$.con,"</br>",
                                     'sea: ',dt$.sea,"  /  ", 'cen: ',dt$.cen,"  /  ",'cap: ',dt$.cap,"  /  ", 'urb: ',dt$.urb,"</br>",
                                     'inst: ',dt$.inst, "  /  ", 'dup: ',dt$.dup, "</br>",
                                     '.coordinates_outOfRange: ', dt$.coordinates_outOfRange, '  /  ', 'parseGBIF_GADM_centroids: ', dt$parseGBIF_GADM_centroids)


          # label <- dt$parseGBIF_dataset_result

          m <- addAwesomeMarkers(m,

                                 lat = dt$parseGBIF_decimalLatitude,
                                 lng = dt$parseGBIF_decimalLongitude,


                                 # aqui voltar
                                 label = label,

                                 popup = etiquetaTextoBtn,

                                 # # labelOptions = labelOptions(style = list( "color" = ifelse(dt$occ_in_sel$verticeEOO==TRUE,"red","black"))),
                                 # labelOptions = labelOptions(style = list( "color" = cor_label)),
                                 # popup = etiquetaTextoBtn,
                                 # icon =   ifelse(index_geo_issue==TRUE, icon_geo_issue, icons),
                                 icon = icons,
                                 # group= ifelse(dt$occ_in_sel$verticeEOO==TRUE,'Vértice EOO','Pré-valiados BR'),
                                 # group = grupo_camadas,
                                 layerId=dt$Ctrl_gbifID)

          # %>%
          #   leafletOptions(crs= crs)

          m
        })




      }

      {

        withProgress(message = 'Processing...rWCVPdata::wcvp_names...', style = 'notification', value = 0.5, {
          # wcvp_names_t <<-  wcvp_get_data_v2.1(read_only_to_memory = TRUE,
          #                                      load_rda_data = TRUE)$wcvp_names


          # wcvp_names_t <<- rWCVPdata::wcvp_names  %>%
          #   data.frame(stringsAsFactors = FALSE)


          # wcvp_names_t <<- rWCVPdata::wcvp_names  %>%
          #     dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
          #                   TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .)) %>%
          #     data.frame(stringsAsFactors = FALSE)


          incProgress(100, detail = '100')
        })

      }

      occ <<- {}
      collectorsDictionary <<- {}
      collectorsDictionaryFromDataset <<- {}
      collectorsDictionary_summary <<- {}
      collectorsDictionary_new <<- {}

      occ_collectorsDictionary <<- {}
      summ_selectDigitalVoucher <<- {}

      wcvpSummary <<- {}
      wcvpOccurrence <<- {}
      # wcvp_names_t <<- {}

      wcvp_names <- {}

      issueGBIFOccurrence <<- {}
      issueGBIFSummary <<- {}

      geospatialVerificationSummary <<- {}
      geospatialVerificationOccurrence <<- {}

      occ_selectDigitalVoucher <<- {}

      all_data <<- {}
      parseGBIF_general_summary <<- {}

      # 6. summaryBtn
      {

        summary_ <- eventReactive(input$summaryBtn,
                                  {
                                    withProgress(message = 'Processing...', style = 'notification', value = 0.5, {

                                      incProgress(0.2, detail = 'parseGBIF_summary...')

                                      summ <- parseGBIF_summary(parseGBIF_all_data = all_data)

                                      incProgress(100, detail = '100')
                                    })

                                    return(list(parseGBIF_general_summary = summ$parseGBIF_general_summary,
                                                parseGBIF_merge_fields_summary = summ$parseGBIF_merge_fields_summary,
                                                parseGBIF_merge_fields_summary_useable_data = summ$parseGBIF_merge_fields_summary_useable_data,
                                                parseGBIF_merge_fields_summary_unusable_data = summ$parseGBIF_merge_fields_summary_unusable_data))

                                  })

        output$parseGBIF_general_summaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                        {
                                                                          summary_()$parseGBIF_general_summary
                                                                        })

        output$parseGBIF_general_summaryDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_6_general_summary - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(summary_()$parseGBIF_general_summary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })

        # "parseGBIF_general_summary" parseGBIF_general_summaryContents  parseGBIF_general_summaryDownload
        # "parseGBIF_merge_fields_summary" parseGBIF_merge_fields_summaryContents parseGBIF_merge_fields_summaryDownload

        output$parseGBIF_merge_fields_summaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                             {
                                                                               summary_()$parseGBIF_merge_fields_summary
                                                                             })

        output$parseGBIF_merge_fields_summaryDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_6_merge_fields_summary - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(summary_()$parseGBIF_merge_fields_summary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })

        # "parseGBIF_merge_fields_summary_useable_data" parseGBIF_merge_fields_summary_useable_dataContents parseGBIF_merge_fields_summary_useable_dataDownload

        output$parseGBIF_merge_fields_summary_useable_dataContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                                          {
                                                                                            summary_()$parseGBIF_merge_fields_summary_useable_data
                                                                                          })

        output$parseGBIF_merge_fields_summary_useable_dataDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_6_merge_fields_summary - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(summary_()$parseGBIF_merge_fields_summary_useable_data, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })

        # "parseGBIF_merge_fields_summary_unusable_data" parseGBIF_merge_fields_summary_unusable_dataContents parseGBIF_merge_fields_summary_unusable_dataDownload

        output$parseGBIF_merge_fields_summary_unusable_dataContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                                           {
                                                                                             summary_()$parseGBIF_merge_fields_summary_unusable_data
                                                                                           })

        output$parseGBIF_merge_fields_summary_unusable_dataDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_6_merge_fields_summary_unusable_data - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(summary_()$parseGBIF_merge_fields_summary_unusable_data, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })

      }


      # 5. Export of results
      {


        export <- eventReactive(input$exportBtn,
                                {
                                  withProgress(message = 'Processing...', style = 'notification', value = 0.5, {



                                    incProgress(0.5, detail = 'Selecting the master digital voucher...')

                                    results <- export_data_v2.3(occ_digital_voucher_file = '',
                                                                occ_digital_voucher = occ_selectDigitalVoucher,
                                                                merge_unusable_data = TRUE,
                                                                silence = FALSE)


                                    all_data <<- results$all_data


                                    incProgress(100, detail = '100')
                                  })

                                  return(list(useable_data_merge = results$useable_data_merge,
                                              unusable_data_merge = results$unusable_data_merge,
                                              duplicates = results$duplicates,
                                              all_data = results$all_data))

                                })

        output$useable_data_mergeContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                 {
                                                                   export()$useable_data_merge
                                                                 })

        output$useable_data_mergeDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_5_useable_data_merge - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(export()$useable_data_merge, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })


        output$duplicatesContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                         {
                                                           export()$duplicates
                                                         })

        output$duplicatesDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_5_duplicates - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(export()$duplicates, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })


        output$unusable_data_mergeContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                  {
                                                                    export()$unusable_data_merge
                                                                  })

        output$unusable_data_mergeDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_5_unusable_data_merge - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(export()$unusable_data_merge, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })


        output$all_dataContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                       {
                                                         all_data <<- export()$all_data
                                                       })

        output$all_dataDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_5_all_data - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(all_data, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })

      }


      # 4. Selecting the master digital voucher
      {

        selectDigitalVoucher <- eventReactive(input$selectDigitalVoucherBtn,
                                              {
                                                withProgress(message = 'Processing...', style = 'notification', value = 0.5, {



                                                  incProgress(0.5, detail = 'Selecting the master digital voucher...')

                                                  tmp <- select_digital_voucher_v2.2(occ = data(),
                                                                                     occ_gbif_issue = issueGBIFOccurrence,
                                                                                     occ_wcvp_check_name = wcvpOccurrence,
                                                                                     occ_collectorsDictionary = occ_collectorsDictionary,
                                                                                     silence = FALSE)

                                                  # {
                                                  #   # divide
                                                  #   key <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %>% unique()
                                                  #   n_k <- NROW(key)
                                                  #   i_key <- 1:n_k
                                                  #
                                                  #   ind_k1 <- i_key <= (n_k/3)
                                                  #   ind_k2 <- i_key > (n_k/3) & i_key <= ((n_k/3)*2)
                                                  #   ind_k3 <- i_key > (n_k/3)*2
                                                  #
                                                  #   n_k
                                                  #   sum(ind_k1==TRUE)
                                                  #   sum(ind_k2==TRUE)
                                                  #   sum(ind_k3==TRUE)
                                                  #   sum(ind_k1==TRUE) + sum(ind_k2==TRUE) + sum(ind_k3==TRUE)
                                                  #
                                                  #   i_k_1 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k1==TRUE]
                                                  #   i_k_2 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k2==TRUE]
                                                  #   i_k_3 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k3==TRUE]
                                                  #
                                                  #   occ_collectorsDictionary[i_k_1==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>%
                                                  #     unique() %>% NROW() +
                                                  #
                                                  #     occ_collectorsDictionary[i_k_2==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>%
                                                  #     unique() %>% NROW() +
                                                  #
                                                  #     occ_collectorsDictionary[i_k_3==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>%
                                                  #     unique() %>% NROW()
                                                  #
                                                  #   occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %>%
                                                  #     unique() %>% NROW()
                                                  #
                                                  #   occ_digital_voucher_t1 <- select_digital_voucher_v2.1(occ = data()[i_k_1==TRUE,],
                                                  #                                                         occ_gbif_issue = issueGBIFOccurrence[i_k_1==TRUE,],
                                                  #                                                         occ_wcvp_check_name = wcvpOccurrence[i_k_1==TRUE,],
                                                  #                                                         occ_collectorsDictionary = occ_collectorsDictionary[i_k_1==TRUE,],
                                                  #                                                         silence = FALSE)
                                                  #
                                                  #   occ_digital_voucher_t2 <- select_digital_voucher_v2.1(occ = data()[i_k_2==TRUE,],
                                                  #                                                         occ_gbif_issue = issueGBIFOccurrence[i_k_2==TRUE,],
                                                  #                                                         occ_wcvp_check_name = wcvpOccurrence[i_k_2==TRUE,],
                                                  #                                                         occ_collectorsDictionary = occ_collectorsDictionary[i_k_2==TRUE,],
                                                  #                                                         silence = FALSE)
                                                  #
                                                  #   occ_digital_voucher_t3 <- select_digital_voucher_v2.1(occ = data()[i_k_3==TRUE,],
                                                  #                                                         occ_gbif_issue = issueGBIFOccurrence[i_k_3==TRUE,],
                                                  #                                                         occ_wcvp_check_name = wcvpOccurrence[i_k_3==TRUE,],
                                                  #                                                         occ_collectorsDictionary = occ_collectorsDictionary[i_k_3==TRUE,],
                                                  #                                                         silence = FALSE)
                                                  #
                                                  #   digital_voucher <- list(all_data =  {} %>% data.frame(stringsAsFactors = FALSE),
                                                  #                           useable_data_raw = {},
                                                  #                           duplicates = {},
                                                  #                           unusable_data_raw = {},
                                                  #                           occ_digital_voucher = {},
                                                  #                           occ_results = {})
                                                  #
                                                  #   digital_voucher$all_data <- rbind(occ_digital_voucher_t1$all_data,
                                                  #                                     occ_digital_voucher_t2$all_data,
                                                  #                                     occ_digital_voucher_t3$all_data)
                                                  #
                                                  #
                                                  #   digital_voucher$useable_data_raw <- rbind(occ_digital_voucher_t1$useable_data_raw,
                                                  #                                             occ_digital_voucher_t2$useable_data_raw,
                                                  #                                             occ_digital_voucher_t3$useable_data_raw)
                                                  #
                                                  #
                                                  #   digital_voucher$unusable_data_raw <- rbind(occ_digital_voucher_t1$unusable_data_raw,
                                                  #                                              occ_digital_voucher_t2$unusable_data_raw,
                                                  #                                              occ_digital_voucher_t3$unusable_data_raw)
                                                  #
                                                  #   digital_voucher$occ_digital_voucher <- rbind(occ_digital_voucher_t1$occ_digital_voucher,
                                                  #                                                occ_digital_voucher_t2$occ_digital_voucher,
                                                  #                                                occ_digital_voucher_t3$occ_digital_voucher)
                                                  #
                                                  #   digital_voucher$occ_results <- rbind(occ_digital_voucher_t1$occ_results,
                                                  #                                        occ_digital_voucher_t2$occ_results,
                                                  #                                        occ_digital_voucher_t3$occ_results)
                                                  #
                                                  # }

                                                  occ_selectDigitalVoucher <<- tmp$occ_digital_voucher

                                                  summ_selectDigitalVoucher <<- tmp$occ_results

                                                  incProgress(100, detail = '100')
                                                })

                                                return(list(occ_digital_voucher = occ_selectDigitalVoucher,
                                                            results = summ_selectDigitalVoucher))


                                              })

        output$SummaryDigitalVoucherContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                    {
                                                                      summ_selectDigitalVoucher <<- selectDigitalVoucher()$results
                                                                    })

        output$digitalVoucherContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                             {
                                                               occ_selectDigitalVoucher <<- selectDigitalVoucher()$occ_digital_voucher #results
                                                             })


        output$digitalVoucherDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_4_select_digital_voucher_summary - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(summ_selectDigitalVoucher, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })


        output$occdigitalVoucherDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_4_select_digital_voucher - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(occ_selectDigitalVoucher, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })

      }


      # 3. Collector's Dictionary
      {

        # load
        {

          output$collectorsDictionaryDownload <- downloadHandler(
            filename = function() {
              paste("collectorsDictionary - ", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {

              # aqui
              # write.csv(collectorsDictionary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
              # write.csv(collectorsDictionary, file, row.names = FALSE,  fileEncoding = 'UCS-2LE', na = "")

              write.csv(collectorsDictionary, file, row.names = FALSE,  fileEncoding = 'UTF-8', na = "")



            })


          collectorsDictionaryLoad <- reactive({
            req(input$collectorsDictionaryFile)

            # library(googlesheets4)
            #
            # gs4_deauth()
            #
            # sheet_url <- "https://docs.google.com/spreadsheets/d/1QjpRZPNOOL0pfRO6IVT5WiafnyNdahsch1A03iHdv7s/"
            # sheet_id <- as_sheets_id(sheet_url)
            #
            # states <- read_sheet(sheet_id, 1)
            # mammals <- read_sheet(sheet_id, 2)

            tryCatch(
              {
                withProgress(message = 'Processing...', style = 'notification', value = 0.5,
                             {


                               files_tmp <- input$collectorsDictionaryFile$datapath

                               occ_tmp <- readr::read_csv(files_tmp,
                                                          locale = readr::locale(encoding = "UTF-8"),
                                                          show_col_types = FALSE) %>%
                                 data.frame() %>%
                                 dplyr::mutate(Ctrl_notes = Ctrl_notes %>% as.character(),
                                               Ctrl_update = Ctrl_update %>% as.character(),
                                               Ctrl_nameRecordedBy_Standard = Ctrl_nameRecordedBy_Standard %>% as.character(),
                                               Ctrl_recordedBy = Ctrl_recordedBy %>% as.character(),
                                               collectorName = collectorName %>% as.character(),
                                               Ctrl_fullName = Ctrl_fullName %>% as.character(),
                                               Ctrl_fullNameII = Ctrl_fullNameII %>% as.character(),
                                               CVStarrVirtualHerbarium_PersonDetails = CVStarrVirtualHerbarium_PersonDetails %>% as.character())

                               collectorsDictionary <<- occ_tmp

                               incProgress(100, detail = '100')
                             })


                return(collectorsDictionary)
              },
              error = function(e) {
                stop(safeError(e))
              }

            )

          })



          collectorsDictionaryFromDatasetLoad <- eventReactive(input$collectorsDictionaryFromDatasetBtn,
                                                               {
                                                                 req(input$collectorsDictionaryFromDatasetFile)
                                                                 tryCatch(
                                                                   {
                                                                     withProgress(message = 'Processing...', style = 'notification', value = 0.5,
                                                                                  {


                                                                                    files_tmp <- input$collectorsDictionaryFromDatasetFile$datapath

                                                                                    if(is.na(file_tmp))
                                                                                    {
                                                                                      files_tmp <- 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary.csv'

                                                                                    }

                                                                                    # occ_tmp <- readr::read_delim(file = files_tmp,
                                                                                    #                   delim = ',',
                                                                                    #                   locale = readr::locale(encoding = "UTF-8"),
                                                                                    #                   show_col_types = FALSE) %>%
                                                                                    #   data.frame()


                                                                                    collectorsDictionary.dataset <- collectors_prepare_dictionary_v2(collectorDictionary_file = files_tmp,
                                                                                                                                                     occ = data())

                                                                                    collectorsDictionaryFromDataset <<- collectorsDictionary.dataset

                                                                                    incProgress(100, detail = '100')
                                                                                  })


                                                                     return(collectorsDictionaryFromDataset)
                                                                   },
                                                                   error = function(e) {
                                                                     stop(safeError(e))
                                                                   }
                                                                 )

                                                               })

          # DT editável
          {
            output$collectorsDictionaryContents = render_dt(collectorsDictionary <<- collectorsDictionaryLoad(), 'cell')

            # edit a single cell
            proxy5 = dataTableProxy('collectorsDictionaryContents')
            observeEvent(input$collectorsDictionaryContents_cell_edit, {
              info = input$collectorsDictionaryContents_cell_edit
              str(info)  # check what info looks like (a data frame of 3 columns)
              collectorsDictionary <<- editData(collectorsDictionary, info)
              replaceData(proxy5, collectorsDictionary, resetPaging = FALSE)  # important
              # the above steps can be merged into a single editData() call; see examples below
            })
          }
        }



        # get Main Collector Last Name
        {

          getCollectorsDictionaryFromDataset <- eventReactive(input$getCollectorsDictionaryFromDatasetBtn,
                                                              {

                                                                if(is.null(input$collectorsDictionaryFromDatasetFile$datapath)!=TRUE)
                                                                {
                                                                  withProgress(message = 'Processing...', style = 'notification', value = 0.5,
                                                                               {
                                                                                 files_tmp <- input$collectorsDictionaryFromDatasetFile$datapath

                                                                                 occ_tmp <- readr::read_delim(file = files_tmp,
                                                                                                              delim = ',',
                                                                                                              locale = readr::locale(encoding = "UTF-8"),
                                                                                                              show_col_types = FALSE) %>%
                                                                                   data.frame()

                                                                                 collectorsDictionaryFromDataset <<- occ_tmp

                                                                                 incProgress(100, detail = '100')
                                                                               })


                                                                }else
                                                                {
                                                                  withProgress(message = 'Processing...', style = 'notification', value = 0.5, {

                                                                    # collectorsDictionaryFromDataset <<- prepere_lastNameRecordedBy_v3(occ_tmp=occ,
                                                                    #                                    coletoresDB=collectorsDictionary)

                                                                    collectorsDictionaryFromDataset <<- collectors_prepare_dictionary(occ)

                                                                    incProgress(100, detail = '100')
                                                                  })
                                                                }

                                                                return(collectorsDictionaryFromDataset)

                                                              })



          # DT editável
          {
            # output$collectorsDictionaryFromDatasetContents = render_dt(collectorsDictionaryFromDataset <<- getCollectorsDictionaryFromDataset(), 'cell')


            # output$collectorsDictionaryFromDatasetContents = render_dt(
            #   ifelse(is.null(input$collectorsDictionaryFromDatasetFile$datapath)!=TRUE,
            #     collectorsDictionaryFromDataset <<- collectorsDictionaryFromDatasetLoad(),
            #     collectorsDictionaryFromDataset <<- getCollectorsDictionaryFromDataset()), 'cell')


            # output$collectorsDictionaryFromDatasetContents = render_dt(ifelse(is.null(input$collectorsDictionaryFromDatasetFile$datapath)!=TRUE,
            #                                                                   # collectorsDictionaryFromDataset <<- collectorsDictionaryFromDatasetLoad(),
            #                                                                   # collectorsDictionaryFromDataset <<- getCollectorsDictionaryFromDataset()),
            #                                                                   collectorsDictionaryFromDatasetLoad(),
            #                                                                   getCollectorsDictionaryFromDataset()),
            #
            #                                                            'cell')


            output$collectorsDictionaryFromDatasetContents = render_dt(collectorsDictionaryFromDataset <<- getCollectorsDictionaryFromDataset(),
                                                                       'cell')

            # edit a single cell
            proxy5 = dataTableProxy('collectorsDictionaryFromDatasetContents')
            observeEvent(input$collectorsDictionaryFromDatasetContents_cell_edit, {
              info = input$collectorsDictionaryFromDatasetContents_cell_edit
              # str(info)  # check what info looks like (a data frame of 3 columns)
              collectorsDictionaryFromDataset <<- editData(collectorsDictionaryFromDataset, info)
              replaceData(proxy5, collectorsDictionaryFromDataset, resetPaging = FALSE)  # important
              # the above steps can be merged into a single editData() call; see examples below
            })
          }

        }


        # apply Collector's Dictionary
        {
          applyCollectorsDictionary <- eventReactive(input$applyCollectorsDictionaryBtn,
                                                     {
                                                       withProgress(message = 'Processing...', style = 'notification', value = 0.5, {
                                                         mainCollectorLastName_tmp <- generate_collection_event_key(occ=data(),
                                                                                                                    collectorDictionary_checked = collectorsDictionaryFromDataset)

                                                         # mainCollectorLastName_tmp <- generate_collection_event_key(occ=data(),
                                                         #                                                            collectorDictionary_checked = dataTableProxy('collectorsDictionaryFromDatasetContents'))



                                                         collectorsDictionary_new <<- mainCollectorLastName_tmp[['collectorsDictionary_add']]

                                                         occ_collectorsDictionary <<- mainCollectorLastName_tmp[['occ_collectorsDictionary']]

                                                         collectorsDictionary_summary <<- mainCollectorLastName_tmp[['summary']]


                                                         incProgress(100, detail = '100')
                                                       })

                                                       return(list(collectorsDictionary_summary = collectorsDictionary_summary,
                                                                   occ_collectorsDictionary = occ_collectorsDictionary,
                                                                   collectorsDictionary_new = collectorsDictionary_new))

                                                     })

          output$collectorsDictionarySummaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                            {
                                                                              collectorsDictionary_summary <- applyCollectorsDictionary()$collectorsDictionary_summary
                                                                            })

          output$collectorsDictionaryNewContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                        {
                                                                          collectorsDictionary_new <<- applyCollectorsDictionary()$collectorsDictionary_new
                                                                        })
          output$collectorsDictionaryOccurrenceContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                               {
                                                                                 occ_collectorsDictionary <<- applyCollectorsDictionary()$occ_collectorsDictionary
                                                                               })





          output$collectorsDictionaryFromDatasetDownload <- downloadHandler(
            filename = function() {
              paste("parseGBIF_3_collectorsDictionaryFromDataset - ", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(collectorsDictionaryFromDataset %>%
                          dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy %>% toupper(),
                                        Ctrl_nameRecordedBy_Standard = Ctrl_nameRecordedBy_Standard %>% toupper()) %>% data.frame(),
                        file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
            })


          output$summary_collectorsDictionaryDownload <- downloadHandler(
            filename = function() {
              paste("parseGBIF_3_unique_collection_events_summary - ", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(collectorsDictionary_summary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
            })

          output$results_collectorsDictionaryDownload <- downloadHandler(
            filename = function() {
              paste("parseGBIF_3_unique_collection_events - ", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(occ, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
            })

          output$add_collectorsDictionaryDownload <- downloadHandler(
            filename = function() {
              paste("parseGBIF_3_add_collectors_in_dictionary - ", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(collectorsDictionary_new, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
            })

        }


      }

      # 2. WCVP
      {

        loadWCVP <- reactive({
          req(input$loadWCVPFile)

          tryCatch(
            {

              withProgress(message = 'Processing...', style = 'notification', value = 0.1,
                           {

                             files_tmp <- input$loadWCVPFile$datapath

                             wcvp_names <<- utils::read.table(files_tmp, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") %>%
                               data.frame(stringsAsFactors = F) %>%
                               dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
                                             TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .))

                             incProgress(1, detail = 'ok')
                           })

              return(wcvp_names)
            },
            error = function(e) {
              stop(safeError(e))
            }
          )
        })


        output$WCPVSummary_text <- renderText({
          req(NROW(data())>0)

          paste0(
            ' Number of records: ', NROW(loadWCVP()),
            ' / Number of selected columns: ', NCOL(loadWCVP()),
            ' / Families: ', length(loadWCVP()$wcvp_family %>% unique()),
            ' / Scientific Names: ', length(data()$wcvp_taxon_name %>% unique())
          )

        })

        # ok 1 wcvp_get_data_v2.1 e wcvp_check_name_batch
        applyWCVP <- eventReactive(input$applyWCVPBtn,
                                   {
                                     withProgress(message = 'Processing...1', style = 'notification', value = 0.5, {

                                       incProgress(0.25, detail = 'wcvp get data...')

                                       # wcvp_names <-  rWCVPdata::wcvp_names %>%
                                       #   data.frame(stringsAsFactors = FALSE) %>%
                                       #   dplyr::select('plant_name_id',
                                       #                 'taxon_rank',
                                       #                 'taxon_status',
                                       #                 'family',
                                       #                 'taxon_name',
                                       #                 'taxon_authors',
                                       #                 'accepted_plant_name_id',
                                       #                 'reviewed')
                                       #
                                       # wcvp_names$TAXON_NAME_U <- wcvp_names$taxon_name %>% toupper()
                                       # wcvp_names$TAXON_AUTHORS_U <- wcvp_names$taxon_authors %>% toupper() %>% gsub ("\\s+", "", .)


                                       if(NROW(wcvp_names)==0)
                                       {
                                         wcvp_names <<-  wcvp_get_data_v2.1(read_only_to_memory = TRUE,
                                                                            load_rda_data = TRUE)$wcvp_names

                                       }

                                       # wcvp_names_t <<- rWCVPdata::wcvp_names  %>%
                                       #   data.frame(stringsAsFactors = FALSE)
                                       #
                                       # wcvp_names_t <<- wcvp_names_t  %>%
                                       #   dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
                                       #                 TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .)) %>%
                                       #   data.frame(stringsAsFactors = FALSE)

                                       # # if(NROW(wcvp_names)==0)
                                       # # {
                                       #   wcvp_names <<- loadWCVP()
                                       # # }

                                       # wcvp_names_tmp <- rWCVPdata::wcvp_names  %>%
                                       #     dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
                                       #                   TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .)) %>%
                                       #     data.frame(stringsAsFactors = FALSE)

                                       # wcvp_names_tmp <- wcvp_names  %>%
                                       #   dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
                                       #                 TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .)) %>%
                                       #   data.frame(stringsAsFactors = FALSE)


                                       incProgress(0.5, detail = 'wcvp_check_name_batch...')

                                       names.checked <- wcvp_check_name_batch(occ = data(),
                                                                              wcvp_names = wcvp_names,                                                                            if_author_fails_try_without_combinations = TRUE,
                                                                              wcvp_selected_fields = 'standard',
                                                                              silence = FALSE)


                                       incProgress(0.75, detail = '...')

                                       wcvpSummary <<- names.checked$summary
                                       wcvpOccurrence <<- names.checked$occ_wcvp_check_name

                                       incProgress(100, detail = 'done!')
                                     })

                                     return(list(wcvpSummary=wcvpSummary,
                                                 wcvpOccurrence=wcvpOccurrence))


                                   })

        output$wcvpSummaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                          {
                                                            wcvpSummary <<- applyWCVP()$wcvpSummary
                                                          })


        output$wcvpOccurrenceContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                             {
                                                               wcvpOccurrence <<- applyWCVP()$wcvpOccurrence
                                                             })


        output$wcvpOccurrenceDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_2_wcvp_check_name - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(wcvpOccurrence, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })


        output$wcvpSummaryDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_2_wcvp_check_name_summary - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(wcvpSummary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })
      }

      # OK 1
      # 1.3 GBIF's Issue
      {


        extractIssue <- eventReactive(input$extractIssueBtn,
                                      {
                                        withProgress(message = 'Processing...', style = 'notification', value = 0.5, {


                                          incProgress(0.25, detail = 'EnumOccurrenceIssue')


                                          incProgress(0.5, detail = 'Issue key')

                                          occ_gbif_issue <- extract_gbif_issue(occ = data())



                                          incProgress(100, detail = '100')
                                        })

                                        issueGBIFSummary <<- occ_gbif_issue$summary
                                        issueGBIFOccurrence <<- occ_gbif_issue$occ_gbif_issue

                                        return(list(issueGBIFSummary=issueGBIFSummary,
                                                    issueGBIFOccurrence=issueGBIFOccurrence))


                                      })

        output$issueSummaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                           {
                                                             issueGBIFSummary <<- extractIssue()$issueGBIFSummary
                                                             # issueGBIFSummary
                                                           })


        output$issueOccurrenceContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                              {
                                                                issueGBIFOccurrence <<- extractIssue()$issueGBIFOccurrence
                                                                # issueGBIFOccurrence
                                                              })


        output$issueOccurrenceDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_1.3_extract_gbif_issue - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(issueGBIFOccurrence, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })


        output$issueSummaryDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_1.3_extract_gbif_issue_summary - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(issueGBIFSummary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })

      }

      # 1.2 Load GBIF
      {

        data <- reactive({
          if(NROW(occ)==0)
          {
            gbifLoad()
          }else
          {
            occ
          }
        })


        output$occGBIFDownload <- downloadHandler(
          filename = function() {
            paste("parseGBIF_1.2_prepare_gbif_occurrence - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {

            # write.csv(data(), file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
            write.csv(data(), file, row.names = FALSE, fileEncoding = "UTF-8", na = "")

          })

        output$DataSummary_text <- renderText({
          req(NROW(data())>0)

          paste0(


            ' Number of records: ', NROW(data()),
            ' / Number of selected columns: ', NCOL(data()),
            ' / Families: ', length(data()$Ctrl_family %>% unique()),
            # ' / Genus: ', length(data()$Ctrl_genus %>% unique()),
            ' / Scientific Names: ', length(data()$Ctrl_scientificName %>% unique())


          )


        })

        gbifLoad <- reactive({
          req(input$gbifFile)

          tryCatch(
            {

              withProgress(message = 'Processing...', style = 'notification', value = 0.1,
                           {

                             # files_tmp <- 'C:\\Users\\Pablo Hendrigo\\Downloads\\ALTO SAO FRANCISCO\\occurrence.txt'
                             # files_tmp_zip <- 'C:\\Dados\\Kew\\data\\raw_gbif_data_www\\Urticaceae\\occurrence.zip'

                             files_tmp_zip <- input$gbifFile$datapath

                             if(str_sub(files_tmp_zip[1],nchar(files_tmp_zip[1])-2,nchar(files_tmp_zip[1]))=='zip')
                             {
                               path_results <- tempdir()
                               utils::unzip(files_tmp_zip, exdir = path_results) # descompactar e salvar dentro subpasta "ipt" na pasta principal
                               incProgress(0.5, detail = 'unzip')

                               files_tmp <- list.files(path =  path_results, full.names = TRUE)

                               if(any(grepl('occurrence.txt', files_tmp)))
                               {
                                 files_tmp <- files_tmp[grepl('occurrence.txt', files_tmp)]
                               }else
                               {
                                 files_tmp <- files_tmp[grepl('.txt|.csv', files_tmp)]
                               }


                             }


                             if(str_sub(files_tmp_zip[1],nchar(files_tmp_zip[1])-2,nchar(files_tmp_zip[1])) %in% c('txt','csv'))
                             {
                               files_tmp <- files_tmp_zip
                             }


                             # files_tmp <- input$gbifFile$datapath
                             nf <- length(files_tmp)
                             occ_tmp <- data.frame({})
                             if(nf>0)
                             {
                               i <- 1
                               for(i in 1:nf)
                               {

                                 if(str_sub(files_tmp[i],nchar(files_tmp[i])-2,nchar(files_tmp[i])) == 'txt')
                                 {

                                   # occ_tmp_1 <- readr::read_delim(file = files_tmp[i],
                                   #                              delim = '\t',
                                   #                              locale = readr::locale(encoding = "UTF-8"),
                                   #                              show_col_types = FALSE)

                                   occ_tmp_1 <- prepare_gbif_occurrence_data(files_tmp[i])

                                 }

                                 if(str_sub(files_tmp[i],nchar(files_tmp[i])-2,nchar(files_tmp[i])) == 'csv')
                                 {

                                   occ_tmp_1 <- readr::read_delim(file = files_tmp[i],
                                                                  delim = ',',
                                                                  locale = readr::locale(encoding = "UTF-8"),
                                                                  show_col_types = FALSE)
                                 }


                                 occ_tmp <- rbind.data.frame(occ_tmp, occ_tmp_1)
                               }
                             }

                             incProgress(0.75, detail = 'load')

                             # if(NROW(occ_tmp)>0 & colnames(occ_tmp)[1]=='bibliographicCitation')
                             # {
                             #   colnames(occ_tmp) <- paste0('Ctrl_',colnames(occ_tmp))
                             # }

                             if(str_sub(files_tmp_zip[1],nchar(files_tmp_zip[1])-2,nchar(files_tmp_zip[1]))=='zip')
                             {
                               file.remove(files_tmp)
                               file.remove(files_tmp_zip)
                             }

                             incProgress(1, detail = 'ok')
                           })

              return(occ_tmp)
            },
            error = function(e) {
              stop(safeError(e))
            }
          )
        })

        # occ <<- gbifLoad()
        output$gbifContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                   {
                                                     occ <<- data()
                                                     # data()
                                                   })




      }

    }


  }

  shinyApp(ui = ui, server = server)

}
# parseGBIF_app()
