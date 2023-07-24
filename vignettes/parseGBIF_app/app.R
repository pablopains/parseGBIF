
rm(list = ls())

online <- FALSE

if(online==FALSE)
{
  if (!dir.exists("c:/R_temp")){dir.create("c:/R_temp")}
  tempdir <- function() "c:/R_temp"
  unlockBinding("tempdir", baseenv())
  assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
  assign("tempdir", tempdir, baseenv())
  lockBinding("tempdir", baseenv())
  tempdir()
  
  setwd('C:\\parseGBIF - github.com\\parseGBIF')
  
}

path_data <- getwd()


# plumber::plumb(file='C:/Dados/Kew/checkName_WCVP/plumber.R')$run()
# Running plumber API at http://127.0.0.1:5750

#' @section 0 - Preparar ambiente R
{
  #' @details carregar funcões para mensurar tempos de processamento
  {
    tempo_processo <- data.frame(nome_processo=NA,
                                 tempo_processamento=NA)[-1,]
    
    inicia_tempo_processamento <- function(nome_processo='',
                                           tempo_processo=NA)
    {
      return(list(ini.time = Sys.time(),
                  nome_processo=nome_processo,
                  tempo_processo=tempo_processo))
    }  
    
    get_tempo_processamento <- function(tempo_processo_tmp)
    {
      
      tempo_processamento <- difftime(Sys.time(), tempo_processo_tmp$ini.time , units = 'min')
      tempo_processo <- rbind(tempo_processo_tmp$tempo_processo,
                              data.frame(nome_processo=tempo_processo_tmp$nome_processo,
                                         tempo_processamento=tempo_processamento))
      print(tempo_processo)
      return(tempo_processo)
    }  
  }
  
  #' @details inicar tempo de processamento
  tempo_processo_tmp <- inicia_tempo_processamento('Preparação do ambiente de trabalho em R',
                                                   tempo_processo)
  #' @details carregar pacotes básicos
  {
    
    
    # library('geosphere')

    # install.packages('plyr', dependencies = TRUE)
    library(plyr) 
    
    # install.packages('readxl', dependencies = TRUE)
    library(readxl) 
    
    # install.packages('dplyr', dependencies = TRUE)
    library(dplyr)
    
    # install.packages('tidyr', dependencies = TRUE)
    library(tidyr)
    
    # # install.packages('biogeo', dependencies = TRUE)
    # library(biogeo)
    
    # install.packages('readr', dependencies = TRUE)
    library(readr)
    
    # install.packages('stringr', dependencies = TRUE)
    library(stringr)
    
    # install.packages('devtools', dependencies = TRUE)
    library(devtools)
    
    # devtools::install_github("ropensci/CoordinateCleaner")
    # library(CoordinateCleaner)
    
    # install.packages('textclean', dependencies = TRUE)
    library(textclean)
    
    # install.packages('googledrive', dependencies = TRUE)
    library(googledrive)
    
    # install.packages('rvest', dependencies = TRUE)
    library(rvest)
    
    # # install.packages('flora', dependencies = TRUE)
    # library(flora)
    
    # install.packages('raster', dependencies = TRUE)
    library(raster)
    
    # install.packages('sp', dependencies = TRUE)
    library(sp)
    
    # install.packages('lubridate', dependencies = TRUE)
    library(lubridate)
    
    # install.packages('rnaturalearthdata', dependencies = TRUE)
    library(rnaturalearthdata)
    
    # # install.packages('geobr', dependencies = TRUE)
    # library(geobr) 
    # 
    # # install.packages('monographaR', dependencies = TRUE)
    # library(monographaR) 
    
    # install.packages('jsonlite', dependencies = TRUE)
    library(jsonlite)
    
    # install.packages('sqldf', dependencies = TRUE)
    library(sqldf) 
    
    # install.packages('shiny', dependencies = TRUE)
    library(shiny) 
    
    
    library(shinydashboardPlus)
    
    # install.packages('shinydashboard', dependencies = TRUE)
    library(shinydashboard)
    
    # install.packages('mapview', dependencies = TRUE)
    library(mapview)
    
    # install.packages('DT', dependencies = TRUE)
    library(DT)
    
    # install.packages('rhandsontable', dependencies = TRUE)
    library(rhandsontable) # tabela editavel
    
    # install.packages('shinyWidgets', dependencies = TRUE)
    library(shinyWidgets) # botoes
    
    # install.packages('measurements', dependencies = TRUE)
    library(measurements)
    
    # install.packages('downloader', dependencies = TRUE)
    library(downloader)
    

    
    options(shiny.maxRequestSize=10000*1024^2) 
    
  }
  
  library(parseGBIF)
}

#' @details Select GBIF fields
{
  
  col_sel <- parseGBIF::select_gbif_fields()
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
  
  #' @section Tela APP--
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
          navbarPage("Global strategy for plant exploration - Framework for filling in the gaps in our knowledge of plant diversity", #"Global strategy for plant exploration",
                     ###
                     tabPanel(icon("upload"), 
                              box(title = "parseGBIF workflow",
                                  status = "primary",
                                  width = 12,
                                  
                              box(title = "1. Upload occurrence file(s)",
                                  status = "primary",
                                  width = 12,
                                  
                                  fluidRow(
                                    column(width = 12,
                                           shiny::tags$a('GBIF | Global Biodiversity Information Facility', href = 'https://www.gbif.org/occurrence/search?occurrence_status=present&q=')
                                    )),
                                  br(),
                                  fluidRow(
                                    column(width = 12,
                                           fileInput(inputId = "gbifFile", 
                                                     label = "Upload TXT occurrence file(s) from GBIF, in Darwin Core Standard (DwC)",
                                                     multiple = TRUE))),

                                  box(title = "Data summary",
                                      status = "primary",
                                      width = 12,
                                      fluidRow(
                                        column(width = 12,
                                               # textOutput("DataSummary_text"))
                                               verbatimTextOutput("DataSummary_text"))
                                      )),

                                  box(title = "Table of occurrences",
                                      status = "primary",
                                      width = 12,
                                      fluidRow(
                                        column(width = 12,
                                               downloadButton("occGBIFDownload", "Download"),
                                               DT::dataTableOutput('gbifContents'))
                                      )),
                              ),
                              
                                  box(title = "2. Prepare the collector's dictionary to group duplicates",
                                      status = "primary",
                                      width = 12,
                                      
                                      fluidRow(
                                        column(width = 12,
                                               actionButton("getCollectorsDictionaryFromDatasetBtn", "Extract the primary collector's last name", icon = icon("play"))
                                        )), 
                                      
                                      
                                      box(title = "Data summary",
                                          status = "primary",
                                          width = 12,
                                          
                                          fluidRow(
                                            column(width = 12,
                                                   # DT editável,
                                                   dt_output('collectorsDictionaryFromDatasetContents')
                                            )),
                                          
                                          fluidRow(
                                            column(width = 12,
                                                   downloadButton("collectorsDictionaryFromDatasetDownload", "Download")
                                            )),
                                          
                                          
                                          fluidRow(
                                            column(width = 12,
                                                   'dddd',
                                                   # verbatimTextOutput("DataSummary_text"))
                                          ))),
                                      # 
                                      # box(title = "Table of occurrences",
                                      #     status = "primary",
                                      #     width = 12,
                                      #     fluidRow(
                                      #       column(width = 12,
                                      #              downloadButton("occGBIFDownload", "Download"),
                                      #              DT::dataTableOutput('gbifContents'))
                                      #     ))
                                      
                                      box(title = "2.3. Apply the collector dictionary to the dataset",
                                          status = "primary",
                                          width = 12,
                                          
                                          fluidRow(
                                            column(width = 12,
                                                   actionButton("applyCollectorsDictionaryBtn", "Aplly", icon = icon("play"))
                                                   
                                            )), 
                                          
                                          
                                          fluidRow(
                                            column(width = 12,
                                                   # DT::dataTableOutput('applyCollectorsDictionaryContents')
                                                   
                                                   helpText("Results"),
                                                   tabsetPanel(
                                                     tabPanel("Summary", DT::dataTableOutput( 'collectorsDictionarySummaryContents')), #'applyCollectorsDictionaryContents')),
                                                     tabPanel("Collectors Dictionary add", DT::dataTableOutput("collectorsDictionaryNewContents")),
                                                     tabPanel("Occurrences", DT::dataTableOutput("collectorsDictionaryOccurrenceContents"))
                                                   ))),
                                          
                                          fluidRow(
                                            column(width = 12,
                                                   downloadButton("collectorsDictionarySummaryDownload", "Download collecor's dictionary summary"),
                                                   downloadButton("collectorsDictionaryNewDownload", "Download collecor's dictionary new"),
                                                   downloadButton("collectorsDictionaryOccurrenceDownload", "Download Occurrences")
                                                   
                                            ))
                                      )
                                  )),
                              
                              
                     ),
                     
                     #' @details collector's dictionary
                     
                     tabPanel(icon("user"), 
                              box(title = "2. Prepare the collector's dictionary to group duplicates",
                                  status = "primary",
                                  width = 12,
                                  
                              box(title = "2.1. Load Collector's Dictionary",
                                  status = "primary",
                                  width = 12,
                                  
                                  fluidRow(
                                    column(width = 12,
                                           shiny::tags$a("Collector's Dictionary", href ="https://docs.google.com/spreadsheets/d/10X_mv6wdB4dCIUph6h8I4_63QPxKacjN714qYEdk02Q/edit#gid=1443813274" ),#'https://drive.google.com/file/d/1LvCHxE3F808YiLxtTKxOWTguOhUaWWmF/view?usp=share_link')
                                    )),
                                  
                                  br(),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           fileInput(inputId = "collectorsDictionaryFile", 
                                                     label = "Upload CSV file with collector's dictionary",
                                                     multiple = TRUE)
                                    )),

                                  fluidRow(
                                    column(width = 12,
                                           helpText("Data summary"),
                                           verbatimTextOutput("collectorsDictionary_text")
                                    )),
                                  

                                  fluidRow(
                                    column(width = 12,
                                           # DT editável,
                                           dt_output('collectorsDictionaryContents')
                                           )),
                                  
                                  
                                  fluidRow(
                                    column(width = 12,
                                           downloadButton("collectorsDictionaryDownload", "Download")
                                           
                                    ))
                                  
                                  ),
                              
                              
                              box(title = "2.2. Extract the primary collector's last name from dataset from step 1 or upload CSV collector's dictionary from dataset file",
                                  status = "primary",
                                  width = 12,

                                  fluidRow(
                                    column(width = 12,
                                           actionButton("getCollectorsDictionaryFromDatasetBtn", "Extract the primary collector's last name", icon = icon("play"))
                                    
                                    )), 
                                  
                                  br(),
                                  fluidRow(
                                    column(width = 8,
                                           fileInput(inputId = "collectorsDictionaryFromDatasetFile", 
                                                     label = "or Upload CSV collector's dictionary from dataset file",
                                                     multiple = TRUE)),
                                  #   ),
                                  # fluidRow(
                                    br(),br(),
                                    column(width = 4,
                                           
                                           actionButton("collectorsDictionaryFromDatasetBtn", "Load CSV collector's dictionary from dataset file", onclick = 'Shiny.onInputChange(\"getCollectorsDictionaryFromDatasetBtn\",  Math.random())',
                                                        icon = icon("play"))
                                  
                                           # actionButton("collectorsDictionaryFromDatasetBtn", "Load CSV collector's dictionary from dataset file", icon = icon("play"))
                                           ),
                                    
                                    
                                    ),
                                  
                                  
                                  fluidRow(
                                    column(width = 12,
                                           # DT editável,
                                           dt_output('collectorsDictionaryFromDatasetContents')
                                           )),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           downloadButton("collectorsDictionaryFromDatasetDownload", "Download")
                                           ))
                              ),
                              
                              
                              box(title = "2.3. Apply the collector dictionary to the dataset",
                                  status = "primary",
                                  width = 12,
                                  
                                  fluidRow(
                                    column(width = 12,
                                           actionButton("applyCollectorsDictionaryBtn", "Aplly", icon = icon("play"))
                                           
                                    )), 
                                  
                                  
                                  fluidRow(
                                    column(width = 12,
                                           # DT::dataTableOutput('applyCollectorsDictionaryContents')
                                           
                                                  helpText("Results"),
                                                  tabsetPanel(
                                                    tabPanel("Summary", DT::dataTableOutput( 'collectorsDictionarySummaryContents')), #'applyCollectorsDictionaryContents')),
                                                    tabPanel("Collectors Dictionary add", DT::dataTableOutput("collectorsDictionaryNewContents")),
                                                    tabPanel("Occurrences", DT::dataTableOutput("collectorsDictionaryOccurrenceContents"))
                                           ))),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           downloadButton("collectorsDictionarySummaryDownload", "Download collecor's dictionary summary"),
                                           downloadButton("collectorsDictionaryNewDownload", "Download collecor's dictionary new"),
                                           downloadButton("collectorsDictionaryOccurrenceDownload", "Download Occurrences")
                                           
                                    ))
                              ))
                     ),
                     
                     #' @details WCVP
                     tabPanel(icon("sitemap"), 
                              box(title = "3. Check species names",
                                  status = "primary",
                                  width = 12,
                                  
                                  box(title = "3.1. Get WCVP database",
                                      status = "primary",
                                      width = 12,
                                      
                                      fluidRow(
                                        column(width = 12,
                                               
                                               shiny::tags$a("The World Checklist of Vascular Plants (WCVP) - KEW - wcvp.zip", href ="http://sftp.kew.org/pub/data-repositories/WCVP/" )
                                               # shiny::tags$a("The World Checklist of Vascular Plants (WCVP) - GBIF", href ="https://www.gbif.org/pt/dataset/f382f0ce-323a-4091-bb9f-add557f3a9a2" ),
                                        )),
                                      
                                      br(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               fileInput(inputId = "loadWCVPFile",
                                                         label = "Upload CSV file with World Checklist of Vascular Plants (WCVP) database",
                                                         multiple = TRUE)
                                        )),
                                      
                                      
                                      fluidRow(
                                        column(width = 12,
                                               helpText("Data summary"),
                                               verbatimTextOutput("WCVP_text")
                                        )),
                                      

                                      fluidRow(
                                        column(width = 12,
                                               DT::dataTableOutput("WCVPContents")
                                        )),
                                      
                                    ),
                                  
                                  box(title = "3.2. Apply WCVP",
                                      status = "primary",
                                      width = 12,

                                      fluidRow(
                                        column(width = 12,
                                               actionButton("applyWCVPBtn", "Aplly", icon = icon("play"))

                                        )),


                                      fluidRow(
                                        column(width = 12,
                                               # DT::dataTableOutput('applyCollectorsDictionaryContents')

                                               helpText("Results"),
                                               tabsetPanel(
                                                 tabPanel("Summary", DT::dataTableOutput( 'wcvpSummaryContents')), #'applyCollectorsDictionaryContents')),
                                                 # tabPanel("Collectors Dictionary add", DT::dataTableOutput("collectorsDictionaryNewContents")),
                                                 tabPanel("Occurrences", DT::dataTableOutput("wcvpOccurrenceContents"))
                                               ))),

                                      fluidRow(
                                        column(width = 12,
                                               downloadButton("wcvpSummaryDownload", "Download check species names summary"),
                                               # downloadButton("collectorsDictionaryNewDownload", "Download collecor's dictionary new"),
                                               downloadButton("wcvpOccurrenceDownload", "Download Occurrences")

                                        ))
                                  )
                                )
                     ),
                     
                     #' @details GBIF's issues
                     
                     tabPanel(icon('share-from-square'), 
                              box(title = "4. Extract GBIF's issues",
                                  status = "primary",
                                  width = 12,

                                  box(title = "4.1. Extract GBIF's issues",
                                      status = "primary",
                                      width = 12,
                                      
                                      fluidRow(
                                        column(width = 12,
                                               actionButton("extractIssueBtn", "Extract", icon = icon("play"))
                                               
                                        )),
                                      
                                      
                                      fluidRow(
                                        column(width = 12,
                                               # DT::dataTableOutput('applyCollectorsDictionaryContents')
                                               
                                               helpText("Results"),
                                               tabsetPanel(
                                                 tabPanel("Summary", DT::dataTableOutput( 'issueSummaryContents')),
                                                 tabPanel("Occurrences", DT::dataTableOutput("issueOccurrenceContents"))
                                               ))),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               downloadButton("issueSummaryDownload", "Download issue summary"),
                                               downloadButton("issueOccurrenceDownload", "Download occurrences")
                                               
                                        ))
                                  )
                              )
                     ),
                     
                     
                     #' @section Geospatial verification - coordinateCleaner and bdc
                     
                     tabPanel(icon('globe'), 
                              box(title = "5. Geospatial verification",
                                  status = "primary",
                                  width = 12,
                                  
                                  box(title = "5.1. Geospatial verification",
                                      status = "primary",
                                      width = 12,
                                      
                                      fluidRow(
                                        column(width = 12,
                                               actionButton("geospatialVerificationBtn", "Aplly", icon = icon("play"))
                                               
                                        )),
                                      
                                      
                                      fluidRow(
                                        column(width = 12,
                                               # DT::dataTableOutput('applyCollectorsDictionaryContents')
                                               
                                               helpText("Results"),
                                               tabsetPanel(
                                                 tabPanel("Summary", DT::dataTableOutput( 'geospatialVerificationSummaryContents')),
                                                 tabPanel("Occurrences", DT::dataTableOutput("geospatialVerificationOccurrenceContents"))
                                               ))),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               downloadButton("geospatialVerificationSummaryDownload", "Download geospatial verification summary"),
                                               downloadButton("geospatialVerificationOccurrenceDownload", "Download occurrences")
                                               
                                        ))
                                  )
                              )
                     )
                     
                     
          ))

      )}
  
  
  #' @section Server
  server <- function(input, output, session)
  {
    
    {
      
      withProgress(message = 'Processing...', style = 'notification', value = 0.5, {
        
        # load("wcvp_names_s.RData")
        
      # files_tmp_zip <- 'wcvp_names.zip'
      # 
      # path_results <- tempdir()
      # 
      # # path_results <- getwd()
      # 
      # utils::unzip(files_tmp_zip, exdir = path_results) # descompactar e salvar dentro subpasta "ipt" na pasta principal
      # 
      # files_tmp <- list.files(path =  path_results, full.names = TRUE)
      # 
      # files_tmp <- files_tmp[grepl('wcvp_names.csv', files_tmp)]
      # 
      # wcvp_names_tmp <<- read.table(files_tmp, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") %>% 
      #   data.frame(stringsAsFactors = F) 
      # 
      # file.remove(files_tmp)
      # 
      # wcvp_names_tmp <- wcvp_names_tmp  %>%
      #   dplyr::filter(taxon_rank %in% c('Species','Subspecies','Variety','Form' ))
      # 
      # wcvp_names <<- wcvp_names_tmp  %>%
      #   dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
      #                 TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .))
      # 
        # wcvp_names <<- wcvp_names %>%
        #   dplyr::select(plant_name_id,
        #               taxon_rank,
        #               taxon_status,
        #               family,
        #               # nomenclatural_remarks,
        #               taxon_name,
        #               taxon_authors,
        #               accepted_plant_name_id,
        #               # replaced_synonym_author,
        #               reviewed,
        #               TAXON_NAME_U,
        #               TAXON_AUTHORS_U)
        # 
      # rm(wcvp_names_tmp)
      # rm(files_tmp_zip)
      # rm(path_results)
        # save.image("C:/Dados/Kew/app/wcvp_names_s.RData")

      incProgress(100, detail = '100')
      })
      
    }
    
    occ <<- {}
    collectorsDictionary <<- {}
    collectorsDictionaryFromDataset <<- {}
    collectorsDictionary_summary <<- {}
    collectorsDictionary_new <<- {}
    
    wcvpSummary <<- {}
    wcvpOccurrence <<- {}
    
    issueGBIFOccurrence <<- {}
    issueGBIFSummary <<- {}
    
    geospatialVerificationSummary <<- {}
    geospatialVerificationOccurrence <<- {}
    

    #' @details Geospatial Verification
    {
      
      
      geospatialVerification <- eventReactive(input$geospatialVerificationBtn,
                                    {
                                      withProgress(message = 'Processing...', style = 'notification', value = 0.1, {
                                        
                                      #' @section Gerar centroides
                                      {
                                        # Get SpatialPolygonsDataFrame object example
                                        
                                        file.centroids <- "centroids.csv"
                                        
                                        if(!file.exists(file.centroids))
                                        {
                                          centroids <- data.frame(countryCode_ISO3=NA,
                                                                  name0=NA,
                                                                  name1=NA,
                                                                  name2=NA,
                                                                  level=NA,
                                                                  lon=NA,
                                                                  lat=NA)[-1,]
                                          
                                          i=1   
                                          for (i in 1:NROW(countryCode_ISO3))
                                          {   
                                            polygons <- getData('GADM', country = countryCode_ISO3[i], level = 0)
                                            
                                            # Get polygons centroids
                                            centroids_tmp <- as.data.frame(centroid(polygons))
                                            colnames(centroids_tmp) <- c("lon", "lat") 
                                            
                                            centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
                                                                                     name0=polygons$NAME_0,
                                                                                     name1=rep(NA, NROW(centroids_tmp)),
                                                                                     name2=rep(NA, NROW(centroids_tmp)),
                                                                                     level=rep(0, NROW(centroids_tmp)),
                                                                                     lon=centroids_tmp$lon,
                                                                                     lat=centroids_tmp$lat))
                                            
                                            print(countryCode_ISO3[i])
                                          }
                                          
                                          i=1   
                                          for (i in 1:NROW(countryCode_ISO3))
                                          {   
                                            polygons <- NULL
                                            try(polygons <- getData('GADM', country = countryCode_ISO3[i], level = 1))
                                            if(is.null(polygons)){next}
                                            
                                            
                                            # Get polygons centroids
                                            centroids_tmp <- as.data.frame(centroid(polygons))
                                            colnames(centroids_tmp) <- c("lon", "lat") 
                                            
                                            centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
                                                                                     name0=polygons$NAME_0,
                                                                                     name1=polygons$NAME_1,
                                                                                     name2=rep(NA, NROW(centroids_tmp)),
                                                                                     level=rep(1, NROW(centroids_tmp)),
                                                                                     lon=centroids_tmp$lon,
                                                                                     lat=centroids_tmp$lat))
                                            
                                            print(countryCode_ISO3[i])
                                          }
                                          
                                          i=1   
                                          for (i in 1:NROW(countryCode_ISO3))
                                          {   
                                            polygons <- NULL
                                            try(polygons <- getData('GADM', country = countryCode_ISO3[i], level = 2))
                                            if(is.null(polygons)){next}
                                            
                                            
                                            # Get polygons centroids
                                            centroids_tmp <- as.data.frame(centroid(polygons))
                                            colnames(centroids_tmp) <- c("lon", "lat") 
                                            
                                            centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
                                                                                     name0=polygons$NAME_0,
                                                                                     name1=polygons$NAME_1,
                                                                                     name2=polygons$NAME_2,
                                                                                     level=rep(2, NROW(centroids_tmp)),
                                                                                     lon=centroids_tmp$lon,
                                                                                     lat=centroids_tmp$lat))
                                            
                                            print(countryCode_ISO3[i])
                                          }
                                          
                                          # file.result <- paste0(path_results,"\\centroids")
                                          # write.csv(centroids, file.result, fileEncoding = "UTF-8", na = "", row.names = FALSE)
                                          
                                        }else
                                        {
                                          
                                          centroids <- readr::read_csv(file.centroids,
                                                                       locale = locale(encoding = "UTF-8"),
                                                                       show_col_types = FALSE)
                                          
                                        }
                                        
                                      }
                                        
                                        
                                        n_dec_round <- 3
                                        numero_registos_por_centroide <- 3
                                        
                                        occ_pto <- occ %>%
                                          dplyr::mutate(Ctrl_decimalLongitude = round(as.numeric(Ctrl_decimalLongitude),n_dec_round))  %>%
                                          dplyr::mutate(Ctrl_decimalLatitude = round(as.numeric(Ctrl_decimalLatitude),n_dec_round))  %>%
                                          dplyr::mutate(Ctrl_decimalLongitude = as.character(sprintf("%.5f", Ctrl_decimalLongitude))) %>%
                                          dplyr::mutate(Ctrl_decimalLatitude = as.character(sprintf("%.5f", Ctrl_decimalLatitude))) %>%
                                          dplyr::mutate(point_cent = paste0(Ctrl_decimalLongitude, ' _ ', Ctrl_decimalLatitude)) %>% 
                                          dplyr::mutate(ID_Count = 1:NROW(occ))
                                        
                                          centroids_tmp <- centroids %>%
                                          dplyr::mutate(lon = round(as.numeric(lon),n_dec_round))  %>%
                                          dplyr::mutate(lat = round(as.numeric(lat),n_dec_round))  %>%
                                          dplyr::mutate(lon = as.character(sprintf("%.5f", lon))) %>%
                                          dplyr::mutate(lat = as.character(sprintf("%.5f", lat))) %>%
                                          dplyr::mutate(point_cent = paste0(lon, ' _ ', lat))
                                          
                                        # .val <- (!issueGBIFOccurrence$COORDINATE_OUT_OF_RANGE) &
                                        #   (! issueGBIFOccurrence$COORDINATE_INVALID) &
                                        #   (! issueGBIFOccurrence$ZERO_COORDINATE) &
                                        #   (! issueGBIFOccurrence$COUNTRY_COORDINATE_MISMATCH)
                                        
                                        
                                        hot_centroids <- left_join(occ_pto,
                                                                   centroids_tmp,
                                                                   by = 'point_cent')
                                      
                                        
                                        hot_centroids <- hot_centroids[!is.na(hot_centroids$level), ]   

                                      point_to_check_hot_centroids <- sqldf::sqldf("SELECT DISTINCT point_cent, name0, name1, name2, level, COUNT(ID_Count)
                        FROM hot_centroids
                        GROUP BY point_cent, level
                        ORDER BY COUNT(ID_Count) DESC") 
                                        
                                      

                                      point_to_check_hot_centroids <- point_to_check_hot_centroids %>%
                                        dplyr::rename(centroidOccurenceCount=`COUNT(ID_Count)`)

                                      centroidOccurenceCount_result <- data.frame(point_cent = occ_pto$point_cent)#,
                                      # Ctrl_year = occ_pto$Ctrl_year)
                                      
                                      centroidOccurenceCount_result <- left_join(centroidOccurenceCount_result,
                                                                                   point_to_check_hot_centroids,
                                                                                   by = 'point_cent') 
                                      
                                      centroidOccurenceCount_result <- centroidOccurenceCount_result %>%
                                        dplyr::select(centroidOccurenceCount,
                                                      point_cent)
                                      

                                        incProgress(100, detail = 'ok')
                                      })
                                      
                                      
                                      return(list(geospatialVerificationSummary=point_to_check_hot_centroids,
                                                  geospatialVerificationOccurrence=centroidOccurenceCount_result))
                                      
                                      
                                    })
      
      output$geospatialVerificationSummaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                         {
                                                           geospatialVerificationSummary <<- geospatialVerification()$geospatialVerificationSummary
                                                         })
      
      
      output$geospatialVerificationOccurrenceContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                            {
                                                              geospatialVerificationOccurrence <<- geospatialVerification()$geospatialVerificationOccurrence
                                                            })
      
      
      output$geospatialVerificationOccurrenceDownload <- downloadHandler(
        filename = function() {
          paste("5_geospatialVerificationOccurrence - ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(geospatialVerificationOccurrence, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
        })
      
      
      output$geospatialVerificationSummaryDownload <- downloadHandler(
        filename = function() {
          paste("5_geospatialVerificationSummary - ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(geospatialVerificationSummary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
        })
      
    }
    
    
    
    #' @details GBIF's Issue
    {
      
      
      extractIssue <- eventReactive(input$extractIssueBtn,
                                 {
                                   withProgress(message = 'Processing...', style = 'notification', value = 0.5, {

                                     #' @details criar estrutura de dados a partir do modelo
                                     {
                                       file.csv.open <- 'EnumOccurrenceIssue.csv'
                                       EnumOccurrenceIssue <- readr::read_csv(file.csv.open, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
                                       
                                       issue_table <- data.frame(t(EnumOccurrenceIssue$Constant)) 
                                       colnames(issue_table) <- EnumOccurrenceIssue$Constant
                                       
                                       issue_key <- colnames(issue_table)
                                       issue_table[1:NROW(occ),issue_key] <- rep(FALSE, NROW(occ))
                                     }
                                     
                                     incProgress(0.25, detail = 'EnumOccurrenceIssue')

                                     
                                     ic <- 1
                                     for(ic in 1:length(issue_key))
                                     {
                                       x_issue <- grepl(issue_key[ic], occ$Ctrl_issue)
                                       # any(x_issue==TRUE)
                                       # occ$Ctrl_issue[x_issue==TRUE]
                                       issue_table[,ic] <- x_issue
                                       
                                     }  
                                     
                                     incProgress(0.5, detail = 'Issue key')
                                     
                                     issue_result <- data.frame(issue = issue_key,
                                                                n_occ = rep(0,length(issue_key)))
                                     i=1
                                     for(i in 1:length(issue_key))
                                     {
                                       n_occ <- issue_table[,issue_key[i]] %>% sum()
                                       # print(paste0(issue_key[i], ' - ',  n_occ))
                                       issue_result$n_occ[i] <- issue_table[,issue_key[i]] %>% sum()
                                     }
                                     
                                     incProgress(100, detail = '100')
                                   })
                                   
                                   issueGBIFSummary <<- issue_result
                                   issueGBIFOccurrence <<- issue_table
                                   
                                   return(list(issueGBIFSummary=issue_result,
                                               issueGBIFOccurrence=issue_table))
                                   
                                   
                                 })
      
      output$issueSummaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                        {
                                                          issueGBIFSummary <<- extractIssue()$issueGBIFSummary
                                                        })
      
      
      output$issueOccurrenceContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                           {
                                                             issueGBIFOccurrence <<- extractIssue()$issueGBIFOccurrence
                                                           })
      
      
      output$issueOccurrenceDownload <- downloadHandler(
        filename = function() {
          paste("4_issueGBIFOccurrence - ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(issueGBIFOccurrence, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
        })
      
      
      output$issueSummaryDownload <- downloadHandler(
        filename = function() {
          paste("4_issueGBIFSummary - ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(issueGBIFSummary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
        })
      
    }
    
    
    #' @details Get WCVP
    {
      
      
      applyWCVP <- eventReactive(input$applyWCVPBtn,
                                                 {
                                                   withProgress(message = 'Processing...1', style = 'notification', value = 0.5, {
                                                     
                                                     # index <- occ$Ctrl_taxonRank %>% toupper() %in%
                                                     #   toupper(c('SPECIES',
                                                     #             'VARIETY',
                                                     #             'SUBSPECIES',
                                                     #             'FORM'))
                                                     # 
                                                     # wcvp_na <- data.frame(wcvp_plant_name_id  = NA,
                                                     #                       # wcvp_ipni_id = NA,
                                                     #                       wcvp_taxon_rank = NA,
                                                     #                       wcvp_taxon_status = NA,
                                                     #                       wcvp_family = NA,
                                                     #                       # wcvp_genus_hybrid = NA,
                                                     #                       # wcvp_genus = NA,
                                                     #                       # wcvp_species_hybrid = NA,         
                                                     #                       # wcvp_species = NA,
                                                     #                       # wcvp_infraspecific_rank = NA,
                                                     #                       # wcvp_infraspecies = NA,
                                                     #                       # wcvp_parenthetical_author = NA,
                                                     #                       # wcvp_primary_author = NA,
                                                     #                       # wcvp_publication_author = NA,  
                                                     #                       # wcvp_place_of_publication = NA,
                                                     #                       # wcvp_volume_and_page = NA,        
                                                     #                       # wcvp_first_published = NA,
                                                     #                       # wcvp_nomenclatural_remarks = NA,
                                                     #                       # wcvp_geographic_area = NA, 
                                                     #                       # wcvp_lifeform_description = NA,   
                                                     #                       # wcvp_climate_description = NA,
                                                     #                       wcvp_taxon_name = NA,  
                                                     #                       wcvp_taxon_authors = NA,
                                                     #                       wcvp_accepted_plant_name_id = NA,
                                                     #                       # wcvp_basionym_plant_name_id = NA,
                                                     #                       # wcvp_replaced_synonym_author = NA,
                                                     #                       # wcvp_homotypic_synonym = NA,
                                                     #                       # wcvp_parent_plant_name_id = NA,   
                                                     #                       # wcvp_powo_id = NA,
                                                     #                       # wcvp_hybrid_formula = NA,
                                                     #                       wcvp_reviewed = NA,
                                                     #                       # # wcvp_TAXON_NAME_U = NA,
                                                     #                       wcvp_searchedName = NA,
                                                     #                       wcvp_taxon_status_of_searchedName = NA,
                                                     #                       wcvp_plant_name_id_of_searchedName = NA,
                                                     #                       wcvp_taxon_authors_of_searchedName = NA,
                                                     #                       wcvp_verified_author = NA,
                                                     #                       wcvp_verified_speciesName = NA,
                                                     #                       wcvp_searchNotes = NA)
                                                     # 
                                                     # colunas_wcvp_sel <- colnames(wcvp_na)
                                                     # 
                                                     # occ_all <- cbind(occ, wcvp_na) %>%
                                                     #   dplyr::mutate(wcvp_searchedName = Ctrl_scientificName) %>%
                                                     #   dplyr::select(all_of(colunas_wcvp_sel))
                                                     # 
                                                     # name_search_wcvp <- occ_all[index==TRUE,]$wcvp_searchedName %>% unique() %>% as.character()
                                                     # 
                                                     # # https://powo.science.kew.org/about-wcvp#unplacednames
                                                     # 
                                                     # # Pourouma cecropiaefolia
                                                     # 
                                                     # x <- {}
                                                     # i <- 1
                                                     # # i <- 938
                                                     # 
                                                     # i=1
                                                     # 
                                                     # tot_rec <- NROW(name_search_wcvp)
                                                     # 
                                                     #   try(
                                                     #     {
                                                     #       for(i in 1:tot_rec)
                                                     #       {
                                                     #         sp_tmp <- name_search_wcvp[i]
                                                     #         
                                                     #         print( paste0( i, '-',tot_rec ,' ',  sp_tmp))
                                                     #         x_tmp <- checkName_WCVP_v3(searchedName = sp_tmp,
                                                     #                                    wcvp_names = wcvp_names,
                                                     #                                    if_author_fails_try_without_combinations = TRUE)
                                                     #         
                                                     #         x <- rbind(x,
                                                     #                    cbind(x_tmp[,
                                                     #                                all_of(colunas_wcvp_sel)]))
                                                     #         index <- occ_all$wcvp_searchedName %in% sp_tmp #name_search_wcvp[i]  # wcvp_searchedName == Ctrl_scientificName
                                                     #         occ_all[index==TRUE, all_of(colunas_wcvp_sel)] <- x_tmp[, all_of(colunas_wcvp_sel)]
                                                     #       }
                                                     #       
                                                     #     })
                                                     
                                                     # wcvp_names <- wcvp_get_data(read_only_to_memory = TRUE)$wcvp_names
                                                     
                                                     names.checked <- wcvp_check_name_batch(occ = occ,
                                                                                            wcvp_names =  wcvp_names,
                                                                                            if_author_fails_try_without_combinations = TRUE,
                                                                                            wcvp_selected_fields = 'standard')
                                                     
                                                     # names(names.checked)


                                                     wcvpSummary <<- names.checked$summary
                                                     # wcvpOccurrence <<- occ_all[,all_of(colunas_wcvp_sel)]
                                                     
                                                     
                                                     wcvpOccurrence <<- names.checked$occ_wcvp_check_name
                                                     print('done!')

                                                     incProgress(100, detail = '100')
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
      
      
      
      output$WCVPContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                 {
                                                   
                                                   wcvp_names <<- loadWCVP()
                                                   
                                                   # if(is.null(wcvp_names)==TRUE)
                                                   # {wcvp_names <<- loadWCVP()}
                                                   # else
                                                   # {wcvp_names}
                                                   
                                                   # wcvp_names
                                                   
                                                 })
      
      
      
      output$wcvpOccurrenceDownload <- downloadHandler(
        filename = function() {
          paste("wcvpOccurrence - ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(wcvpOccurrence, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
        })
      
      
      output$wcvpSummaryDownload <- downloadHandler(
        filename = function() {
          paste("wcvpSummary - ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(wcvpSummary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
        })
      
      
      
      
      loadWCVP <- reactive({
        req(input$loadWCVPFile)
        
        tryCatch(
          {
            withProgress(message = 'Processing...', style = 'notification', value = 0.1, 
                         {
                           
                           
                           files_tmp_zip <- input$loadWCVPFile$datapath

                           if(str_sub(files_tmp_zip[1],nchar(files_tmp_zip[1])-2,nchar(files_tmp_zip[1]))=='zip')
                           {
                             path_results <- tempdir()
                             utils::unzip(files_tmp_zip, exdir = path_results) # descompactar e salvar dentro subpasta "ipt" na pasta principal
                             incProgress(0.5, detail = 'unzip')
                             
                             files_tmp <- list.files(path =  path_results, full.names = TRUE)
                             
                             files_tmp <- files_tmp[grepl('wcvp_names.csv', files_tmp)]
                           }
                           
                           
                           if(str_sub(files_tmp_zip[1],nchar(files_tmp_zip[1])-2,nchar(files_tmp_zip[1])) == 'csv')
                           {
                             
                             # files_tmp_zip <- 'C:\\Dados\\Kew\\data\\WCVP\\wcvp_names.CSV'
                             files_tmp <- files_tmp_zip
                           }
                           
                           incProgress(0.5, detail = '')
                           
                           wcvp_names_tmp <- read.table(files_tmp, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") %>% 
                             data.frame(stringsAsFactors = F) 
                           
                           
                           # wcvp_names_tmp$taxon_rank %>% unique()
                           # 
                           # wcvp_names_tmp  <- wcvp_names_tmp  %>%
                           #   dplyr::filter(taxon_rank %in% c('Species','Subspecies','Variety','Form' ))
                           
                           
                           incProgress(0.75, detail = '')
                           
                           
                           wcvp_names <<- wcvp_names_tmp  %>%
                             dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
                                           TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .))
                           
                           if(str_sub(files_tmp_zip[1],nchar(files_tmp_zip[1])-2,nchar(files_tmp_zip[1]))=='zip')
                           {
                             file.remove(files_tmp)
                             file.remove(files_tmp_zip)
                           }

                           incProgress(100, detail = '')
                         })
            
            return(wcvp_names)
          },
          error = function(e) {
            stop(safeError(e))
          }
          
        )
        
      })
      
      
      #' getWCVP <- eventReactive(input$getWCVPBtn,
      #'                          {
      #'                            withProgress(message = 'Processing...', style = 'notification', value = 0.1, {
      #'                              
      #'                              # tempo_processo_tmp <<- inicia_tempo_processamento('Get WCVP', tempo_processo)
      #'                              
      #'                              wcvp_tmp <- get_wcvp(url_source = 'http://sftp.kew.org/pub/data-repositories/WCVP/',
      #'                                               path_results = path_data,
      #'                                               update = FALSE)
      #'                              
      #'                              incProgress(50, detail = '50')
      #'                              
      #'                              wcvp_tmp$wcvp_names <- wcvp_tmp$wcvp_names %>%
      #'                                dplyr::filter(taxon_rank %in% c('Species','Subspecies','Variety','Form' ))
      #'                              
      #'                              #' @details taxon_rank
      #'                              # "Species"     "nothosubsp." "Subspecies"  "Form"        "Variety"     "microgene"   "Genus"       ""           
      #'                              # "proles"      "nothof."     "Subvariety"  "nothovar."   "Subform"     "lusus"       "monstr."     "[**]"       
      #'                              # "[*]"         "sublusus"    "Convariety"  "psp."        "subspecioid" "group"       "grex"        "stirps"     
      #'                              # "mut."        "subproles"   "nid"         "provar."     "positio"     "micromorphe" "modif."      "ecas."      
      #'                              # "microf."     "agamosp."
      #'                              
      #'                              # tempo_processo <<- get_tempo_processamento(tempo_processo_tmp)                                   
      #'                              incProgress(100, detail = '100')
      #'                            })
      #'                            
      #' 
      #'                            wcvp <<- wcvp_tmp
      #'                            
      #'                            return(wcvp)
      #'                            
      #'                          })
      
      
      output$DataSummary_text <- renderText({ 
        req(input$getWCVPBtn)

        paste0(
          
          
          ' Number of records: ', NROW(wcvp_names),
          ' / Number of selected columns: ', NCOL(wcvp_names),
          ' / Families: ', length(wcvp_names$family %>% unique()),
          # ' / Genus: ', length(data()$Ctrl_genus %>% unique()),
          ' / Scientific Names: ', length(wcvp_names$taxon_name %>% unique())
          
          
        )
      })

    }

    
    #' @details  Collector's Dictionary
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
                           
                           
                           occ_tmp <- readr::read_delim(file = files_tmp,
                                             delim = ',',
                                             locale = readr::locale(encoding = "UTF-8"),
                                             show_col_types = FALSE) %>%
                             data.frame()
                           
                           
                           # %>%
                           #   data.frame() %>%
                           #   dplyr::mutate(Ctrl_notes = Ctrl_notes %>% as.character(),
                           #                 Ctrl_update = Ctrl_update %>% as.character(),
                           #                 Ctrl_nameRecordedBy_Standard = Ctrl_nameRecordedBy_Standard %>% as.character(),
                           #                 Ctrl_recordedBy = Ctrl_recordedBy %>% as.character(),
                           #                 collectorName = collectorName %>% as.character(),
                           #                 Ctrl_fullName = Ctrl_fullName %>% as.character(),
                           #                 Ctrl_fullNameII = Ctrl_fullNameII %>% as.character(),
                           #                 CVStarrVirtualHerbarium_PersonDetails = CVStarrVirtualHerbarium_PersonDetails %>% as.character())
                           
                           collectorsDictionaryFromDataset <<- occ_tmp
                           
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
                                                                
                                                                collectorsDictionaryFromDataset <<- parseGBIF::collectors_prepare_dictionary(occ)
                                                                
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
          

          output$collectorsDictionaryFromDatasetContents = render_dt(getCollectorsDictionaryFromDataset(),
                                                                     'cell')
          
          # edit a single cell
          proxy5 = dataTableProxy('collectorsDictionaryFromDatasetContents')
          observeEvent(input$collectorsDictionaryFromDatasetContents_cell_edit, {
            info = input$collectorsDictionaryFromDatasetContents_cell_edit
            str(info)  # check what info looks like (a data frame of 3 columns)
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
                                                               

                                                               # mainCollectorLastName_tmp <- update_lastNameRecordedBy_v5(occ_tmp=occ,
                                                               #                                                           recordedBy_ajusted=collectorsDictionaryFromDataset,
                                                               #                                                           coletoresDB=collectorsDictionary)
                                                               
                                                               
                                                               # mainCollectorLastName_tmp <- parseGBIF::generate_collection_event_key(occ=occ,
                                                               #                                                                       collectorDictionary = collectorsDictionaryFromDataset)
                                                               mainCollectorLastName_tmp <- generate_collection_event_key(occ=occ,
                                                                                                                          collectorDictionary_checked = collectorsDictionaryFromDataset)
                                                               
                                                               collectorsDictionary_new <<- mainCollectorLastName_tmp[['collectorsDictionary_add']]
                                                               
                                                               occ <<- mainCollectorLastName_tmp[['occ_collectorsDictionary']]
                                                               
                                                               collectorsDictionary_summary <<- mainCollectorLastName_tmp[['summary']]
                                                               
                                                               
                                                               incProgress(100, detail = '100')
                                                             })
                                                             
                                                             collectorsDictionary_summary

                                                           })
        
        output$collectorsDictionarySummaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                         {
                                                                           collectorsDictionary_summary <- applyCollectorsDictionary()
                                                                         })
        
        output$collectorsDictionaryNewContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                        {
                                                                          collectorsDictionary_new
                                                                        })
        output$collectorsDictionaryOccurrenceContents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                        {
                                                                          occ
                                                                        })
        
        
        
        
        
        output$collectorsDictionaryFromDatasetDownload <- downloadHandler(
          filename = function() {
            paste("collectorsDictionaryFromDataset - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(collectorsDictionaryFromDataset, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })
        
        
        output$collectorsDictionarySummaryDownload <- downloadHandler(
          filename = function() {
            paste("collectorsDictionary_summary - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(collectorsDictionary_summary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })
        
        
        output$collectorsDictionaryNewDownload <- downloadHandler(
          filename = function() {
            paste("collectorsDictionary_new - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(collectorsDictionary_new, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })
        
        
        output$collectorsDictionaryOccurrenceDownload <- downloadHandler(
          filename = function() {
            paste("Occurrence - ", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(occ, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })
        
      }
      
      
    }
    
    
    #' @details Load GBIF
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
          paste("OccurrenceGBIF - ", Sys.Date(), ".csv", sep="")
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
                             
                             files_tmp <- files_tmp[grepl('.txt|.csv', files_tmp)]
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
                               
                                 occ_tmp_1 <- parseGBIF::prepare_gbif_occurrence_data(files_tmp[i])
                                 
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
  
  
  #' @section Run the application 
  shinyApp(ui = ui, server = server)
}


# seleção
{
  # abrir
  {
    
    path.result <- 'C:\\Users\\Pablo Hendrigo\\Downloads\\kew\\urticaceae'
  
  file.csv <- paste0(path.result,'\\Occurrence - 2023-04-23.csv') 
  occ <- readr::read_csv(file.csv,
                         locale = locale(encoding = "UTF-8"),
                         show_col_types = FALSE)
  

  file.csv <- paste0(path.result,'\\4_issueGBIFOccurrence - 2023-04-23.csv') 
  occ_issue <- readr::read_csv(file.csv,
                         locale = locale(encoding = "UTF-8"),
                         show_col_types = FALSE)

  
  file.csv <- paste0(path.result,'\\wcvpOccurrence - 2023-04-23.csv') 
  occ_wcvo <- readr::read_csv(file.csv,
                         locale = locale(encoding = "UTF-8"),
                         show_col_types = FALSE)

  occ <- cbind(occ_issue, occ, occ_wcvo)

  occ$Ctrl_taxonRank %>% unique()
  
  # "FAMILY"
  # "GENUS"
  # "SPECIES" 
  # "VARIETY" 
  # "FORM"    
  
  
  occ$wcvp_taxon_rank %>% unique()
  occ$wcvp_taxon_rank <- ifelse(is.na(occ$wcvp_taxon_rank),'',occ$wcvp_taxon_rank)
  
  occ$wcvp_taxon_status %>% unique()
  occ$wcvp_taxon_status <- ifelse(is.na(occ$wcvp_taxon_status),'',occ$wcvp_taxon_status)
  
  
  file.csv.open <- 'EnumOccurrenceIssue.csv'
  EnumOccurrenceIssue <- readr::read_csv(file.csv.open, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  # View(EnumOccurrenceIssue)
  
  occ_issue <- colnames(occ)
  
  # geospatial_quality
  index_tmp1 <- EnumOccurrenceIssue$score == 1 & EnumOccurrenceIssue$type == 'geospatial' %>%
    ifelse(is.na(.), FALSE,.)
  index_tmp2 <- EnumOccurrenceIssue$score == 2 & EnumOccurrenceIssue$type == 'geospatial'%>%
    ifelse(is.na(.), FALSE,.)
  index_tmp3 <- EnumOccurrenceIssue$score == 3 & EnumOccurrenceIssue$type == 'geospatial'%>%
    ifelse(is.na(.), FALSE,.)
  }
  
  # verbatim_quality
  {
    occ <- occ %>%
      dplyr::mutate(temAnoColeta =  ifelse( is.na(Ctrl_year) | Ctrl_year == ""  | Ctrl_year == 0 | Ctrl_year <= 10,
                                            FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temCodigoInstituicao = ifelse( is.na(Ctrl_institutionCode) | Ctrl_institutionCode=="",
                                                   FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temNumeroCatalogo = ifelse( is.na(Ctrl_catalogNumber) | Ctrl_catalogNumber=="",
                                                FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temColetor = ifelse( is.na(Ctrl_recordedBy) | Ctrl_recordedBy=="",
                                         FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temNumeroColeta = ifelse( is.na(Ctrl_recordNumber) | Ctrl_recordNumber=="",
                                              FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    # COUNTRY_MISMATCH
                    
                    temPais = ifelse( COUNTRY_INVALID==TRUE,
                                      FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temUF = ifelse( is.na(Ctrl_stateProvince) | Ctrl_stateProvince=="",
                                    FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temMunicipio = ifelse( is.na(Ctrl_municipality) | Ctrl_municipality=="",
                                           FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temLocalidade = ifelse( is.na(Ctrl_locality) | Ctrl_locality=="",
                                            FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temIdentificador = ifelse( is.na(Ctrl_identifiedBy) | Ctrl_identifiedBy=="",
                                               FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    
                    temDataIdentificacao = ifelse( is.na(Ctrl_dateIdentified) | Ctrl_dateIdentified=="",
                                                   FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),
                    
                    temCitacaoBibliografica = ifelse( is.na(Ctrl_bibliographicCitation) | Ctrl_bibliographicCitation=="",
                                                      FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.)
                    
      )
    
    # occ[,EnumOccurrenceIssue$Constant[index_tmp1 == TRUE]]
    }
  
  # for
  {

    occ <- occ %>% 
      dplyr::mutate(geospatial_quality = 0,
                    verbatim_quality = 0,
                    Ctrl_moreInformativeRecord = 0,
                    Ctrl_selectedMoreInformativeRecord = FALSE,
                    Ctrl_thereAreDuplicates = FALSE,
                    Ctrl_unmatched = FALSE,
                    Ctrl_unidentifiedSample = FALSE,
                    
                    # sample taxon name
                    Ctrl_sampleTaxonName = '',
                    # match status between duplicates
                    Ctrl_matchStatusDuplicates = '',
                    # sample identification status
                    Ctrl_sampleIdentificationStatus = '',
                    # number of taxon names for the sample
                    Ctrl_numberTaxonNamesSample = 0)
    
    occ <- occ %>%
      dplyr::mutate(geospatial_quality = ifelse(rowSums(occ[,EnumOccurrenceIssue$Constant[index_tmp3 == TRUE]])>0, -9,
                                                ifelse(rowSums(occ[,EnumOccurrenceIssue$Constant[index_tmp2 == TRUE]])>0, -3, 
                                                       ifelse(rowSums(occ[,EnumOccurrenceIssue$Constant[index_tmp1 == TRUE]])>0, -1, 0)))) 
    
    
    occ <- occ %>%
      dplyr::mutate(verbatim_quality = (  temColetor +
                                          temNumeroColeta +
                                          temAnoColeta +
                                          temCodigoInstituicao +
                                          temNumeroCatalogo +
                                          temLocalidade +
                                          temMunicipio +
                                          temUF + 
                                          temCitacaoBibliografica))
    
    occ <- occ %>%
      dplyr::mutate(Ctrl_moreInformativeRecord  = ( geospatial_quality + verbatim_quality))

    
    occ <- occ %>%
      dplyr::select(Ctrl_key_family_recordedBy_recordNumber,
                    wcvp_taxon_name,
                    wcvp_taxon_status,
                    wcvp_searchNotes,
                    # Ctrl_taxonRank,
                    geospatial_quality,
                    verbatim_quality,
                    Ctrl_moreInformativeRecord,
                    Ctrl_selectedMoreInformativeRecord,
                    Ctrl_thereAreDuplicates,
                    Ctrl_unmatched,
                    Ctrl_unidentifiedSample,
                    Ctrl_sampleTaxonName,
                    Ctrl_matchStatusDuplicates,
                    Ctrl_sampleIdentificationStatus,
                    Ctrl_numberTaxonNamesSample)
    
    index <- str_sub(occ$Ctrl_key_family_recordedBy_recordNumber, str_count(occ$Ctrl_key_family_recordedBy_recordNumber)-2, str_count(occ$Ctrl_key_family_recordedBy_recordNumber)) %in% '_NA'
    occ$Ctrl_key_family_recordedBy_recordNumber[index==TRUE] <- str_sub(occ$Ctrl_key_family_recordedBy_recordNumber[index==TRUE], 1, str_count(occ$Ctrl_key_family_recordedBy_recordNumber[index==TRUE])-2)


    recordedBy_unique <- occ$Ctrl_key_family_recordedBy_recordNumber %>% unique()
    
    # r <- recordedBy_unique[999]
    # r <- recordedBy_unique[2]
    # r <- recordedBy_unique[982]
    # r <- recordedBy_unique[1]
    # r <- 'ACHATOCARPACEAE_CAROL_'
    # r <- 'ACHATOCARPACEAE_CABRERA_'
    # r <- 'ACHATOCARPACEAE_CRISTOBAL_1254'
    # r <- 'ACHATOCARPACEAE__7862'
    r <- 'ACHATOCARPACEAE_ZARDINI_38377'
    r <- 'ACHATOCARPACEAE_AGUILAR-CANO_2271'
    r <- 'URTICACEAE_AHERN_619'
    r <- 'URTICACEAE_ACEVEDO-RODRIGUEZ_4423'
    r <- 'URTICACEAE_AMORIM_4433'
    
    r <-"URTICACEAE_CROAT_16297"
    r <- "URTICACEAE_BOZEMAN_45126"
    r <- "URTICACEAE_LONGBOTTOM_14525"
    
    r <- 'URTICACEAE_BANG_127'
    
    r <- 'URTICACEAE_BORDEN_1186'
    
    r <- 'URTICACEAE_SCHUNKE_7643'
    
    r <- 'URTICACEAE_WUNDERLIN_'
    
    r <- 'URTICACEAE_SEM-COLETOR_'
    # r 
    
    japrocessado <<- rep(FALSE,length(recordedBy_unique))
    
    tot <- NROW(recordedBy_unique)
    s <- 0

    for (r in recordedBy_unique)
    {
      s <- s+1
      
      if (s%%100==0){print(paste0(s, ' de ',tot))}
      
      if(japrocessado[s]==TRUE){next}
      
      # print(paste0(r, ' ',s, ' de ',tot))
      
      FAMILY__ <-  FAMILY__recordNumber <- FAMILY_recordedBy_ <- FALSE
      sp_name <- ''
      
      index_occ <- (occ$Ctrl_key_family_recordedBy_recordNumber %in% r) %>% ifelse(is.na(.), FALSE,.)

      num_records <- NROW(occ[index_occ==TRUE,])
      
      if (num_records == 0) 
      {
        print(r)
        print('table')
        break
      } 
      
      japrocessado[s] <- TRUE
      
      fam <-str_sub(r,1, str_locate(r, '_')[1]-1) %>% ifelse(is.na(.), "",.)
      
      if(str_sub(r, str_count(r), str_count(r)) == '_' | 
         grepl('__', r) |
         grepl('SEM-COLETOR',r))
      {
        
        FAMILY__ <- grepl('__', r) & str_locate(r, '__')[2] == str_count(r) %>% ifelse(is.na(.), FALSE,.)
        
        if(FAMILY__==FALSE)
        {
          
          # FAMILY_recordedBy_ <- (str_sub(r, str_count(r), str_count(r)) == '_' &
          #                          !str_sub(r, str_count(r)-1, str_count(r)-1) == '_') |
          #   grepl('SEM-COLETOR',r) %>% ifelse(is.na(.), FALSE,.)
          # 
          # if (FAMILY_recordedBy_==FALSE)
          # {
          #   FAMILY__recordNumber <- (grepl('__', r) &
          #                              str_locate(r, '__')[2] != str_count(r)) %>% ifelse(is.na(.), FALSE,.)
          # }
          
          
          FAMILY_recordedBy_ <- (grepl('__', r) &
                                   str_locate(r, '__')[2] != str_count(r)) |
            grepl('SEM-COLETOR',r) %>% ifelse(is.na(.), FALSE,.)
          
          if (FAMILY_recordedBy_==FALSE)
          {
            
            FAMILY__recordNumber <- (str_sub(r, str_count(r), str_count(r)) == '_' &
                                       !str_sub(r, str_count(r)-1, str_count(r)-1) == '_')  %>% ifelse(is.na(.), FALSE,.)
            
          }
          
        }
      }
      
      
      # unmatched
      if(FAMILY__ == TRUE | FAMILY__recordNumber == TRUE | FAMILY_recordedBy_== TRUE )
      {
        # incluir filtro espacial
        
        # nomes
        sp_name <- ifelse(occ[index_occ==TRUE, ]$wcvp_taxon_status == 'Accepted',
                          occ[index_occ==TRUE, ]$wcvp_taxon_name %>% as.character(),
                          '')
        
        occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
          dplyr::mutate(Ctrl_selectedMoreInformativeRecord = TRUE,
                        Ctrl_unmatched = TRUE,
                        Ctrl_thereAreDuplicates = FALSE,
                        Ctrl_sampleTaxonName = sp_name,
                        Ctrl_unidentifiedSample = ifelse(sp_name %in% '', TRUE,FALSE),
                        Ctrl_matchStatusDuplicates = ifelse(FAMILY__==TRUE,
                                                             'unmatched: no recordedBy and no recordNumber', 
                                                             ifelse(FAMILY__recordNumber==TRUE,
                                                                    'unmatched: no recordNumber ',
                                                                    ifelse(FAMILY_recordedBy_==TRUE,
                                                                           'unmatched: no recordedBy', 'unmatched'))),
                        Ctrl_numberTaxonNamesSample = ifelse(sp_name %in% '',
                                                              0,
                                                              1),
                        Ctrl_sampleIdentificationStatus = ifelse(sp_name %in% '', 
                                                                  'unidentified',
                                                                 # paste0('unidentified: ', 
                                                                 #        ifelse(occ[index_occ==TRUE, ]$Ctrl_taxonRank %in%  c("SPECIES","VARIETY", "FORM", "SUBSPECIES"),
                                                                 #               occ[index_occ==TRUE, ]$wcvp_searchNotes,
                                                                 #               occ[index_occ==TRUE, ]$Ctrl_taxonRank)),
                                                                  'identified')
                        
                        )
        
        # print('1 - Unmatched samples')
        next
      }
      
      occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
        dplyr::mutate(Ctrl_matchStatusDuplicates = 'matched',
                      Ctrl_thereAreDuplicates = num_records > 1)
      
      # flaq para inidicar amostra não identificada
      
      if(!any(is.na(occ[index_occ==TRUE, ]$wcvp_taxon_name) == FALSE)) # any ???
      {
        
        # nomes
        sp_name <- rep('',num_records)
        
        occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
          dplyr::mutate(Ctrl_matchStatusDuplicates = 'matched',
                        Ctrl_thereAreDuplicates = num_records > 1,
                        Ctrl_numberTaxonNamesSample = 0,
                        Ctrl_sampleTaxonName = sp_name,
                        # Ctrl_sampleIdentificationStatus = paste0('unidentified: ', 
                        #                                          ifelse(occ[index_occ==TRUE, ]$Ctrl_taxonRank %in%  c("SPECIES","VARIETY", "FORM", "SUBSPECIES"),
                        #                                                 occ[index_occ==TRUE, ]$wcvp_searchNotes,
                        #                                                 occ[index_occ==TRUE, ]$Ctrl_taxonRank)))
                        Ctrl_sampleIdentificationStatus = 'unidentified')

        # print('2 - Unidentified sample')

      } else
      {
        
        taxon_name_sample <- table(occ[index_occ==TRUE, ]$wcvp_taxon_name,
                                   # occ[index_occ==TRUE, ]$wcvp_searchNotes,
                                   occ[index_occ==TRUE, ]$wcvp_taxon_status,
                                   # occ[index_occ==TRUE, ]$Ctrl_taxonRank,
                                   exclude = NA) %>% 
          data.frame() %>%
          dplyr::filter(Freq > 0) %>%
          dplyr::arrange(desc(Freq),Var1)
        
        num_taxon_name <- NROW(taxon_name_sample)

        if(num_taxon_name==0)
        {
          
          print(occ[index_occ==TRUE, ]$wcvp_taxon_name) 
          
          print('0 - Error')
          
          break
        }
        
        
        if(num_taxon_name==1 & taxon_name_sample$Var2[1] %in% c('Accepted'))#,'Updated'))
        {
          sp_name <- taxon_name_sample$Var1[1] %>% as.character()
          
          occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
            dplyr::mutate(Ctrl_matchStatusDuplicates = 'matched',
                          Ctrl_thereAreDuplicates = num_records > 1,
                          Ctrl_numberTaxonNamesSample = num_taxon_name,
                          Ctrl_sampleTaxonName = sp_name,
                          Ctrl_sampleIdentificationStatus = 'identified')

          # print('3 - Identified sample 100 %')

        }
        
        
        if(num_taxon_name>1)
        {
          ii=1
          for(ii in 1:NROW(taxon_name_sample))
          {
            
            if(taxon_name_sample$Var2[ii] %in% c('Accepted'))#,'Updated'))
            {
              sp_name <- taxon_name_sample$Var1[ii] %>% as.character()
              
              occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
                dplyr::mutate(Ctrl_matchStatusDuplicates = 'matched',
                              Ctrl_thereAreDuplicates = num_records > 1,
                              Ctrl_numberTaxonNamesSample = num_taxon_name,
                              Ctrl_sampleTaxonName = sp_name,
                              Ctrl_sampleIdentificationStatus = 'divergent identifications')
              
              
              # print(paste0('4 - Identified sample ', 100/num_taxon_name,' %'))
              
              break  
            }
            
            
          }
          
        }
        
      }
      
      occ[index_occ==TRUE, ]$Ctrl_selectedMoreInformativeRecord <-
        (occ[index_occ==TRUE, ]$Ctrl_moreInformativeRecord ==
           max(occ[index_occ==TRUE, ]$Ctrl_moreInformativeRecord) )
      
      if (sum(occ[index_occ==TRUE, ]$Ctrl_selectedMoreInformativeRecord)>1)
      {
        
        index_end <- occ[index_occ==TRUE, ]$Ctrl_selectedMoreInformativeRecord == TRUE
        
        n_tmp <- NROW(occ[index_occ==TRUE, ]$Ctrl_selectedMoreInformativeRecord[index_end==TRUE])
        
        if (n_tmp==1)
        {
          occ[index_occ==TRUE, ]$Ctrl_selectedMoreInformativeRecord[index_end==FALSE] <- FALSE
        } else
        {
          occ[index_occ==TRUE, ]$Ctrl_selectedMoreInformativeRecord[index_end==FALSE] <- FALSE
          occ[index_occ==TRUE, ]$Ctrl_selectedMoreInformativeRecord[index_end==TRUE][2:n_tmp] <- FALSE
        }   
        
        # print(paste0('6 - Selection of the more informative record ', 100/sum(index_end),' %'))
        
      }
      # else
      # {
      #   print(paste0('5 - Selection of the more informative record 100 %'))
      # }
    }
    
  }
  
  # salvar
  
  file.result <- paste0("C:\\Users\\Pablo Hendrigo\\Downloads\\kew\\urticaceae\\",fam,'_selectedMoreInformativeRecord_.csv')
  write.csv(occ, file.result, fileEncoding = "UTF-8", na = "", row.names = FALSE)
  
  occ  %>%
    # dplyr::filter(Ctrl_matchStatusDuplicates != '') %>%
    dplyr::arrange(Ctrl_key_family_recordedBy_recordNumber) %>%
    View()

  
  # occ %>% 
    # dplyr::select(Ctrl_key_family_recordedBy_recordNumber, 
    #               # Ctrl_eventDate,
    #               # Ctrl_level0Name,
    #               # Ctrl_level1Name,
    #               # Ctrl_level2Name,
    #               # # Ctrl_municipality,
    #               # # Ctrl_county,
    #               wcvp_taxon_name,
    #               wcvp_taxon_status,
    #               wcvp_searchNotes,
    #               wcvp_taxon_rank,
    #               # wcvp_reviewed,
    #               # Ctrl_taxonRank,
    #               geospatial_quality,
    #               verbatim_quality,                                 
    #               Ctrl_moreInformativeRecord,
    #               Ctrl_selectedMoreInformativeRecord,               
    #               Ctrl_thereAreDuplicates,
    #               Ctrl_unmatched,                                   
    #               Ctrl_unidentifiedSample,
    #               Ctrl_sampleTaxonName,                             
    #               Ctrl_matchStatusDuplicates,                        
    #               Ctrl_sampleIdentificationStatus,
    #               Ctrl_numberTaxonNamesSample) %>%
    # dplyr::filter(Ctrl_matchStatusDuplicates != '') %>%
    # dplyr::arrange(Ctrl_key_family_recordedBy_recordNumber) %>%
    # View()
  
      
}


# familias
{
  
  files_tmp <- 'C:\\Dados\\Kew\\data\\WCVP\\wcvp_names.CSV'
  
  wcvp_names <- read.table(files_tmp, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") %>% 
    data.frame(stringsAsFactors = F) 
  
  wcvp_names$taxon_rank %>% unique()
  
  wcvp_names$taxon_status %>% unique()
  
  wcvp_names  <- wcvp_names  %>%
    dplyr::filter(taxon_rank %in% c('Species','Subspecies','Variety','Form' ) &
                    taxon_status == 'Accepted')
  
  wcvp_names <<- wcvp_names  %>%
    dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
                  TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .))
  
  colnames(wcvp_names)
  
  # wcvp_family <- sqldf::sqldf("SELECT family, count(taxon_name)"
                              
  wcvp_family <- sqldf::sqldf("SELECT family, count(taxon_name) as NumberSpecies
                        FROM wcvp_names
                        WHERE taxon_status = 'Accepted' 
                        AND taxon_rank = 'Species'
                        GROUP BY family
                        ORDER BY NumberSpecies DESC") 

  
  # wcvp_family <- sqldf::sqldf("SELECT DISTINCT family, taxon_name, taxon_status
  wcvp_family <- sqldf::sqldf("SELECT DISTINCT taxon_name
                              FROM wcvp_names
                        WHERE taxon_status = 'Accepted' AND family = 'Asteraceae'
                              ") 
  
  View(wcvp_family)
  
  file.result <- paste0("C:\\Dados\\Kew\\data\\download_family_control_sp.csv")
  write.csv(wcvp_family, file.result, fileEncoding = "UTF-8", na = "", row.names = FALSE)
  
}



#' @details Geospatial Verification
{
#'   
#'   
#'   geospatialVerification <- eventReactive(input$geospatialVerificationBtn,
#'                                           {
#'                                             
#'                                             
#'                                             if(is.null(issueGBIFOccurrence))
#'                                             {
#'                                               return(null)  
#'                                             }
#'                                             
#'                                             
#'                                             withProgress(message = 'Processing...', style = 'notification', value = 0.5, {
#'                                               
#'                                               
#'                                               
#'                                               #' @details carregar mapas de fundo
#'                                               {
#'                                                 
#'                                                 #' @details map  
#'                                                 world <- raster::getData("countries", path=path_data, download = TRUE)
#'                                                 
#'                                                 # Renaming the column in the mask to use in coordinateCleaner
#'                                                 names(world)[names(world) == "ISO"] <- "iso_a3_eh"
#'                                                 
#'                                               }
#'                                               
#'                                               incProgress(0.1, detail = 'Load maps')
#'                                               
#'                                               occ_cc <- occ %>%
#'                                                 dplyr::select(Ctrl_scientificName,
#'                                                               Ctrl_decimalLongitude,
#'                                                               Ctrl_decimalLatitude,
#'                                                               Ctrl_countryCode) %>%
#'                                                 dplyr::rename(species = Ctrl_scientificName,
#'                                                               decimallongitude  = Ctrl_decimalLongitude,
#'                                                               decimallatitude = Ctrl_decimalLatitude) %>%
#'                                                 dplyr::mutate(.val = (!issueGBIFOccurrence$COORDINATE_OUT_OF_RANGE) &
#'                                                                 (! issueGBIFOccurrence$COORDINATE_INVALID), #.val = occ$Ctrl_hasCoordinate, # hasCoordinate
#'                                                               
#'                                                               .zer = (! issueGBIFOccurrence$ZERO_COORDINATE), #.zer = TRUE,
#'                                                               .sea = TRUE,
#'                                                               .equ = TRUE,
#'                                                               .cen = TRUE,
#'                                                               .cap = TRUE,
#'                                                               .urb = TRUE,
#'                                                               .con = (! issueGBIFOccurrence$COUNTRY_COORDINATE_MISMATCH),  #.con = TRUE,
#'                                                               .inst = TRUE,                         
#'                                                               .dup = TRUE,
#'                                                               geospatialVerification = TRUE,
#'                                                               decimallongitude = as.numeric(decimallongitude),
#'                                                               decimallatitude = as.numeric(decimallatitude)) %>%
#'                                                 dplyr::select(species,
#'                                                               decimallongitude,
#'                                                               decimallatitude,
#'                                                               Ctrl_countryCode,
#'                                                               .val,
#'                                                               .zer,
#'                                                               .sea,
#'                                                               .equ,
#'                                                               .cen,
#'                                                               .cap,
#'                                                               .urb,
#'                                                               .con,
#'                                                               .inst,
#'                                                               # .dup,
#'                                                               geospatialVerification)
#'                                               
#'                                               index <- rep(TRUE, NROW(occ_cc))
#'                                               
#'                                               # get from GBIF
#'                                               {  
#'                                                 
#'                                                 # occ_cc <- bdc_coordinates_outOfRange(
#'                                                 #   data = occ_cc[index==TRUE,],
#'                                                 #   lat = "decimallatitude",
#'                                                 #   lon = "decimallongitude")
#'                                                 
#'                                                 
#'                                                 # index <- index & occ_cc$.coordinates_outOfRange
#'                                                 # occ_cc$.val[index==TRUE] <- CoordinateCleaner::cc_val(x=occ_cc[index==TRUE, ],
#'                                                 #                                                       value = 'flagged')
#'                                               }
#'                                               
#'                                               index <- index & occ_cc$.val
#'                                               # get from GBIF
#'                                               {
#'                                                 # occ_cc$.zer[index==TRUE] <- CoordinateCleaner::cc_zero(x=occ_cc[index==TRUE,],
#'                                                 #                                                      value = 'flagged')
#'                                               }
#'                                               
#'                                               incProgress(0.2, detail = '.val')
#'                                               
#'                                               index <- index & occ_cc$.zer
#'                                               occ_cc$.sea[index==TRUE] <- !CoordinateCleaner::cc_sea(x=occ_cc[index==TRUE,],
#'                                                                                                      scale = 110, #10, 50, 110
#'                                                                                                      value = 'flagged')
#'                                               
#'                                               incProgress(0.3, detail = '.sea')
#'                                               
#'                                               index <- index & !occ_cc$.sea
#'                                               occ_cc$.equ[index==TRUE] <- CoordinateCleaner::cc_equ(x=occ_cc[index==TRUE,],
#'                                                                                                     value = 'flagged')
#'                                               
#'                                               incProgress(0.4, detail = '.equ')
#'                                               
#'                                               index <- index & occ_cc$.equ
#'                                               occ_cc$.cen[index==TRUE] <- CoordinateCleaner::cc_cen(x=occ_cc[index==TRUE,],
#'                                                                                                     value = 'flagged')
#'                                               
#'                                               incProgress(0.5, detail = '.cap')
#'                                               
#'                                               occ_cc$.cap[index==TRUE] <- CoordinateCleaner::cc_cap(x=occ_cc[index==TRUE,],
#'                                                                                                     value = 'flagged')
#'                                               
#'                                               incProgress(0.6, detail = '.urb')
#'                                               
#'                                               occ_cc$.urb[index==TRUE] <- CoordinateCleaner::cc_urb(x=occ_cc[index==TRUE,],
#'                                                                                                     value = 'flagged')
#'                                               
#'                                               incProgress(0.7, detail = '.inst')
#'                                               
#'                                               occ_cc$.inst[index==TRUE] <- CoordinateCleaner::cc_inst(x=occ_cc[index==TRUE,],
#'                                                                                                       value = 'flagged')
#'                                               
#'                                               incProgress(0.8, detail = '')
#'                                               
#'                                               # get from GBIF
#'                                               {
#'                                                 # occ_cc$.con[index==TRUE] <- CoordinateCleaner::cc_coun(x = occ_cc[index==TRUE,],
#'                                                 #                                                        lon = "decimallongitude",
#'                                                 #                                                        lat = "decimallatitude",
#'                                                 #                                                        iso3 = "Ctrl_countryCode",
#'                                                 #                                                        value = "flagged",
#'                                                 #                                                        ref = world,
#'                                                 #                                                        ref_col = "iso_a3_eh")
#'                                               }
#'                                               
#'                                               # no usefull
#'                                               # occ_cc$.dup[index==TRUE] <- CoordinateCleaner::cc_dupl(x=occ_cc[index==TRUE,],
#'                                               #                                                        value = 'flagged')
#'                                               
#'                                               incProgress(0.9, detail = '')
#'                                               
#'                                               # sumário
#'                                               # occ_cc$geospatialVerification <- occ_cc$.val & occ_cc$.zer & (!occ_cc$.sea) & occ_cc$.cen & occ_cc$.coordinates_outOfRange
#'                                               
#'                                               occ_cc$geospatialVerification <- occ_cc$.val & occ_cc$.zer & (!occ_cc$.sea) & occ_cc$.cen
#'                                               
#'                                               
#'                                               geospatialVerificationSummary <<- {}
#'                                               geospatialVerificationOccurrence <<- occ_cc %>%
#'                                                 dplyr::select(.sea,
#'                                                               .equ,
#'                                                               .cen,
#'                                                               .cap,
#'                                                               .urb,
#'                                                               .con,
#'                                                               .inst,
#'                                                               .dup,
#'                                                               geospatialVerification
#'                                                 )
#'                                               
#'                                               incProgress(100, detail = 'ok')
#'                                             })
#'                                             
#'                                             
#'                                             return(list(issueGBIFSummary=issue_result,
#'                                                         issueGBIFOccurrence=issue_table))
#'                                             
#'                                             
#'                                           })
#'   
#'   output$geospatialVerificationSummaryContents <- DT::renderDataTable(options = list(scrollX = TRUE),
#'                                                                       {
#'                                                                         geospatialVerificationSummary <<- geospatialVerification()$geospatialVerificationSummary
#'                                                                       })
#'   
#'   
#'   output$geospatialVerificationOccurrenceContents <- DT::renderDataTable(options = list(scrollX = TRUE),
#'                                                                          {
#'                                                                            geospatialVerificationOccurrence <<- geospatialVerification()$geospatialVerificationOccurrence
#'                                                                          })
#'   
#'   
#'   output$geospatialVerificationOccurrenceDownload <- downloadHandler(
#'     filename = function() {
#'       paste("5_geospatialVerificationOccurrence - ", Sys.Date(), ".csv", sep="")
#'     },
#'     content = function(file) {
#'       write.csv(issueGBIFOccurrence, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
#'     })
#'   
#'   
#'   output$geospatialVerificationSummaryDownload <- downloadHandler(
#'     filename = function() {
#'       paste("5_geospatialVerificationSummary - ", Sys.Date(), ".csv", sep="")
#'     },
#'     content = function(file) {
#'       write.csv(issueGBIFSummary, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
#'     })
#'   
}







