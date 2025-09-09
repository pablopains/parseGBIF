#' @title Get Flora e Funga  do Brazil database
#'
#' @name flora_funga_brasil_get_data
#'
#' @description Download Flora e Funga  do Brazil database
#'
#' @param url_source = "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
#' @param path_results download destination folder
#'
#' @details http://sftp.kew.org/pub/data-repositories/WCVP/ This is the public SFTP (Secure File Transfer Protocol) site of the Royal Botanic Gardens, Kew. This space contains data resources publicly accessible to the user `anonymous'.  No password required for access. Use of data made available via this site may be subject to legal and licensing restrictions. The README in the top-level directory for each data resource provides specific information about its terms of use.
#'
#' @return list with two data frames: wcvp_names and wcvp_distribution
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{wcvp_check_name}}, \code{\link[parseGBIF]{wcvp_check_name_batch}}
#'
#' @examples
#' \donttest{
#' # load package
#' library(parseGBIF)
#'
#' help(flora_funga_brasil_get_data)
#'
#' # Download database to local disk
#' path_data <- tempdir() # you can change this folder
#'
#' flora_funga_brasil <- flora_funga_brasil_get_data(path_results = path_data,
#'                  update = FALSE,
#'                  load_distribution = TRUE)
#' }
#'
#' @import dplyr
#' @import downloader
#' @import stringr
#'
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
