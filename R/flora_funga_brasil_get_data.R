#' @title Download Flora e Funga do Brazil Database
#' @name flora_funga_brasil_get_data
#'
#' @description
#' Downloads and processes the Flora e Funga do Brazil database from the JBRJ IPT.
#' This database contains taxonomic information for Brazilian plant and fungal species,
#' including accepted names, synonyms, and taxonomic hierarchy.
#'
#' @param url_source
#' Character. URL source for the Flora e Funga do Brazil database.
#' Default is "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil".
#'
#' @param path_results
#' Character. Directory path where the database files will be downloaded and stored.
#'
#' @param update
#' Logical. If `TRUE`, re-downloads the database even if it exists locally.
#' Default is `FALSE`.
#'
#' @param load_distribution
#' Logical. If `TRUE`, loads distribution data (currently not implemented).
#' Default is `FALSE`.
#'
#' @details
#' ## Data Source:
#' The Flora e Funga do Brazil database is maintained by the Jardim Botânico do Rio de Janeiro (JBRJ)
#' and provides comprehensive taxonomic information for Brazilian flora and funga.
#'
#' ## Processing Steps:
#' 1. Downloads compressed database from JBRJ IPT
#' 2. Extracts and reads the taxon.txt file
#' 3. Standardizes scientific names and taxonomic ranks
#' 4. Creates uppercase versions for case-insensitive matching
#' 5. Returns processed taxonomic data
#'
#' ## Taxonomic Ranks Included:
#' - FAMILIA (Family)
#' - GENERO (Genus)
#' - ESPECIE (Species)
#' - SUB_ESPECIE (Subspecies)
#' - VARIEDADE (Variety)
#' - FORMA (Form)
#'
#' @return
#' A data frame containing the processed Flora e Funga do Brazil taxonomic data
#' with the following key columns:
#' - Taxonomic identifiers and hierarchy
#' - Scientific names with and without authors
#' - Taxonomic status and rank
#' - Uppercase versions for matching
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`flora_funga_brasil_check_name()`] for checking individual names against the database,
#' [`flora_funga_brasil_check_name_batch()`] for batch name checking
#'
#' @examples
#' \donttest{
#' # Load package
#' library(parseGBIF)
#'
#' # Download database to temporary directory
#' path_data <- tempdir()
#'
#' flora_funga_brasil <- flora_funga_brasil_get_data(
#'   path_results = path_data,
#'   update = FALSE
#' )
#'
#' # View database structure
#' colnames(flora_funga_brasil)
#' head(flora_funga_brasil)
#' }
#'
#' @importFrom dplyr select
#' @importFrom readr read_delim
#' @importFrom downloader download
#' @importFrom utils unzip
#' @importFrom stringr str_split
#' @export
flora_funga_brasil_get_data <- function(url_source = "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil",
                                      path_results = '')

  {

    # require(dplyr)
    # require(downloader)
    # require(stringr)

    # criar pasta para salvar raultados do dataset
    path_results <- paste0(path_results,'/FloraFungaBrasil')
    if (!dir.exists(path_results)){dir.create(path_results)}

    destfile <- paste0(path_results,"/IPT_FloraFungaBrasil_.zip")


    # ultima versao
    # destfile <- paste0(path_results,"/",Sys.Date(),'.zip')
    downloader::download(url = url_source, destfile = destfile, mode = "wb")
    utils::unzip(destfile, exdir = path_results) # descompactar e salvar dentro subpasta "ipt" na pasta principal


    taxon.file <- paste0(path_results,"/taxon.txt")

    # taxon.file <- paste0("C:\\Dados\\APP_GBOT\\data\\FloraFungaBrasil\\taxon.txt")



    # taxon
    fb2020_taxon  <- readr::read_delim(taxon.file, delim = "\t", quote = "") %>%
      dplyr::select(-id)

    ### familia
    # index = fb2020_taxon$taxonRank %in% c("ESPECIE",
    #                                       "SUB_ESPECIE",
    #                                       "VARIEDADE",
    #                                       "FORMA")

    index = fb2020_taxon$taxonRank %in% c("ESPECIE",
                                          "SUB_ESPECIE",
                                          "VARIEDADE",
                                          "FORMA",
                                          "FAMILIA",
                                          "GENERO")
    ###

    fb2020_taxon  <- fb2020_taxon[index==TRUE,]


    scientificName_tmp <- fb2020_taxon$scientificName %>% stringr::str_split(.,pattern = ' ', simplify = TRUE)


    # carregando especie sem autor
    scientificName <- rep('',nrow(fb2020_taxon))

    # scientificName[index==TRUE] <- scientificName_tmp[index==TRUE,1] %>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("ESPECIE")

    scientificName[index==TRUE] <-  paste0(scientificName_tmp[index==TRUE,1], ' ', scientificName_tmp[index==TRUE,2]) #%>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("VARIEDADE")
    scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' var. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("SUB_ESPECIE")
    scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' subsp. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("FORMA")
    scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' form. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')

    fb2020_taxon$scientificNamewithoutAuthorship <- scientificName
    fb2020_taxon$scientificNamewithoutAuthorship_U <- toupper(scientificName)

    fb2020_taxon$scientificNameAuthorship_U <- toupper(fb2020_taxon$scientificNameAuthorship)

    ### reconhecer genero e familia

    fb2020_taxon$genus_U <- toupper(fb2020_taxon$genus)

    fb2020_taxon$family_U <- toupper(fb2020_taxon$family)

    ###

    return(fb2020_taxon)

  }
