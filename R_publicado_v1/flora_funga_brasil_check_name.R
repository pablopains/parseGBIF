#' @title Check species names against Funga  do Brazil database
#'
#' @name flora_funga_brasil_get_data
#'
#' @description Check species names against Flora e Funga do Brazil database
#'
#' @param searchedName = searched name
#' @param flora_funga_brasil Flora e Funga  do Brazil database
#'
#' @details ....
#'
#' @return Flora e Funga  do Brazil database
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
flora_funga_brasil_check_name <- function(searchedName = 'Gardnerina angustata',
                                       flora_funga_brasil="",
                                       if_author_fails_try_without_combinations=TRUE,
                                       silence=TRUE)
{
  if(silence==FALSE){print(searchedName)}
  # https://powo.science.kew.org/about-wcvp#unplacednames

  x <- {}
  # sp_fb <- standardize_scientificName_v2(searchedName)
  sp_fb <- standardize_scientificName(searchedName)

  if(sp_fb$taxonAuthors != "")
  {

    index_author <- 100

    index <- flora_funga_brasil$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName) &
      flora_funga_brasil$scientificNameAuthorship_U %in% toupper(gsub ("\\s+", "", sp_fb$taxonAuthors ))
    ntaxa <- NROW(flora_funga_brasil[index==TRUE,])

    if(ntaxa == 0 & if_author_fails_try_without_combinations == TRUE)
    {
      index_author <- 50
      index <- flora_funga_brasil$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName) &
        flora_funga_brasil$scientificNameAuthorship_U %in% toupper(gsub ("\\s+", "", sp_fb$taxonAuthors_last ))
      ntaxa <- NROW(flora_funga_brasil[index==TRUE,])
    }


    if(ntaxa == 0)
    {
      index_author <- 0
      index <- flora_funga_brasil$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName)
      ntaxa <- NROW(flora_funga_brasil[index==TRUE,])
    }

  }else
  {
    index_author <- 0
    index <- flora_funga_brasil$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName)
    ntaxa <- NROW(flora_funga_brasil[index==TRUE,])
  }

  if(ntaxa == 0 | sp_fb$standardizeName=="")
  {
    x <- flora_funga_brasil[index==TRUE,] %>%
      dplyr::add_row()  %>%
      dplyr::mutate(searchedName=searchedName,
                    taxon_status_of_searchedName = "",
                    plant_name_id_of_searchedName = "",
                    taxon_authors_of_searchedName = "",
                    verified_author = index_author,
                    verified_speciesName = 0,
                    searchNotes='Not found')
  }

  if(ntaxa == 1)
  {
    verified_speciesName <- 100

    id_accept <- ifelse(is.na(flora_funga_brasil$acceptedNameUsageID[index==TRUE]),'', flora_funga_brasil$acceptedNameUsageID[index==TRUE])

    if((!is.na(flora_funga_brasil$acceptedNameUsageID[index==TRUE])) &
       (flora_funga_brasil$taxonID[index==TRUE] != id_accept ))
    {

      x <- flora_funga_brasil[index==TRUE,]

      taxon_status_of_searchedName <- flora_funga_brasil[index==TRUE,]$taxonomicStatus
      plant_name_id_of_searchedName <- flora_funga_brasil[index==TRUE,]$taxonID
      taxon_authors_of_searchedName <- flora_funga_brasil[index==TRUE,]$scientificNamewithoutAuthorship

      index_synonym <- flora_funga_brasil$taxonID %in% x$acceptedNameUsageID

      if(sum(index_synonym==TRUE)==1)
      {
        x <- flora_funga_brasil[index_synonym==TRUE,] %>%
          dplyr::mutate(searchedName=searchedName,
                        taxon_status_of_searchedName = taxon_status_of_searchedName,
                        plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                        taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                        verified_author = index_author,
                        verified_speciesName = verified_speciesName,
                        searchNotes= 'Updated')

      }else
      {
        x <- flora_funga_brasil[index==TRUE,] %>%
          dplyr::mutate(searchedName=searchedName,
                        taxon_status_of_searchedName = taxon_status_of_searchedName,
                        plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                        taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                        verified_author = index_author,
                        verified_speciesName = verified_speciesName,
                        searchNotes= 'Does not occur in Brazil')
      }

    }else
    {
      x <- flora_funga_brasil[index==TRUE,] %>%
        # dplyr::add_row()  %>%
        dplyr::mutate(searchedName=searchedName,
                      taxon_status_of_searchedName = "",
                      plant_name_id_of_searchedName = "",
                      taxon_authors_of_searchedName = "",
                      verified_author = index_author,
                      verified_speciesName = verified_speciesName,
                      searchNotes=ifelse(is.na(taxonomicStatus),'',taxonomicStatus))
    }

  }

  if(ntaxa > 1)
  {

    taxon_status_of_searchedName <- paste(flora_funga_brasil[index==TRUE,]$taxonomicStatus, collapse = '|')
    plant_name_id_of_searchedName <- paste(flora_funga_brasil[index==TRUE,]$taxonID, collapse = '|')
    # taxon_authors_of_searchedName <- paste(paste0(flora_funga_brasil[index==TRUE,]$taxon_name, ' ',flora_funga_brasil[index==TRUE,]$taxon_authors), collapse = '|')
    taxon_authors_of_searchedName <- paste(flora_funga_brasil[index==TRUE,]$scientificNameAuthorship, collapse = '|')


    # Accepted or Homonyms
    {
      index_status <- flora_funga_brasil$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName) &
        flora_funga_brasil$taxonomicStatus %in% c( "NOME_ACEITO")

      ntaxa_status <- NROW(flora_funga_brasil[index_status==TRUE,])

      if(ntaxa_status == 1)
      {

        x <- flora_funga_brasil[index_status==TRUE,] %>%
          dplyr::mutate(searchedName=searchedName,
                        taxon_status_of_searchedName = taxon_status_of_searchedName,
                        plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                        taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                        verified_author = index_author,
                        verified_speciesName = 100/ntaxa,
                        searchNotes=taxonomicStatus)
      }
      else
      {


        x <- flora_funga_brasil[1==2,] %>%
          dplyr::add_row()  %>%
          dplyr::mutate(searchedName=searchedName,
                        taxon_status_of_searchedName = taxon_status_of_searchedName,
                        plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                        taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                        verified_author = index_author,
                        verified_speciesName = 0,
                        searchNotes='Homonyms')

      }

    }

  }

  # 'Homonyms' ajustar família

  if(x$searchNotes == 'Not found' & is.na(word(sp_fb$standardizeName,2)))
  {
    ### reconhecer genero e familia
    # x <-{}
    w1 <- toupper(word(sp_fb$standardizeName))

    index <- flora_funga_brasil$genus_U %in% toupper(w1) & flora_funga_brasil$taxonRank == 'GENERO' #& !is.na(flora_funga_brasil$acceptedNameUsageID)
    ntaxa <- NROW(flora_funga_brasil[index==TRUE,])

    g_f <- 'g'

    if(ntaxa == 0 )
    {
      index <- flora_funga_brasil$family_U %in% toupper(w1) & flora_funga_brasil$taxonRank == 'FAMILIA' #& !is.na(flora_funga_brasil$acceptedNameUsageID)
      ntaxa <- NROW(flora_funga_brasil[index==TRUE,])
      g_f <- 'f'
    }

    if(ntaxa == 1)
    {
      verified_speciesName <- 100

      id_accept <- ifelse(is.na(flora_funga_brasil$acceptedNameUsageID[index==TRUE]),'', flora_funga_brasil$acceptedNameUsageID[index==TRUE])

      if((!is.na(flora_funga_brasil$acceptedNameUsageID[index==TRUE])) &
         (flora_funga_brasil$taxonID[index==TRUE] != id_accept ))
      {

        x <- flora_funga_brasil[index==TRUE,]

        taxon_status_of_searchedName <- flora_funga_brasil[index==TRUE,]$taxonomicStatus
        plant_name_id_of_searchedName <- flora_funga_brasil[index==TRUE,]$taxonID
        taxon_authors_of_searchedName <- flora_funga_brasil[index==TRUE,]$scientificNamewithoutAuthorship

        index_synonym <- flora_funga_brasil$taxonID %in% x$acceptedNameUsageID

        if(sum(index_synonym==TRUE)==1)
        {
          x <- flora_funga_brasil[index_synonym==TRUE,] %>%
            dplyr::mutate(searchedName=searchedName,
                          taxon_status_of_searchedName = taxon_status_of_searchedName,
                          plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                          taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                          verified_author = index_author,
                          verified_speciesName = verified_speciesName,
                          searchNotes=  ifelse(g_f=='g', 'Updated_genus', 'Updated_family') )

        }else
        {
          x <- flora_funga_brasil[index==TRUE,] %>%
            dplyr::mutate(searchedName=searchedName,
                          taxon_status_of_searchedName = taxon_status_of_searchedName,
                          plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                          taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                          verified_author = index_author,
                          verified_speciesName = verified_speciesName,
                          searchNotes= 'Does not occur in Brazil')
        }

      }else
      {
        x <- flora_funga_brasil[index==TRUE,] %>%
          # dplyr::add_row()  %>%
          dplyr::mutate(searchedName=searchedName,
                        taxon_status_of_searchedName = "",
                        plant_name_id_of_searchedName = "",
                        taxon_authors_of_searchedName = "",
                        verified_author = index_author,
                        verified_speciesName = verified_speciesName,
                        searchNotes=taxonomicStatus)
      }

    }


    if(ntaxa >1)
    {

      x <- flora_funga_brasil[index==TRUE,][1,] %>%
        # dplyr::add_row()  %>%
        dplyr::mutate(taxonID = '',
                      acceptedNameUsageID = '',
                      parentNameUsageID = '',
                      originalNameUsageID = '',

                      # scientificName = ifelse(g_f=='g', genus, family),
                      scientificName = '',

                      acceptedNameUsage  = '',
                      parentNameUsage = '',
                      namePublishedIn = '',
                      namePublishedInYear = '',
                      higherClassification = '',
                      # kingdom
                      # phylum
                      # class
                      # order
                      # family
                      # genus
                      specificEpithet = '',
                      infraspecificEpithet = '',

                      # taxonRank = ifelse(g_f=='g',"GENERO", "FAMILIA"),
                      taxonRank = '',

                      scientificNameAuthorship = '',
                      taxonomicStatus = '',
                      nomenclaturalStatus = '',
                      modified = '',
                      bibliographicCitation = '',
                      references = '',
                      scientificNamewithoutAuthorship = ifelse(g_f=='g', genus, family),
                      scientificNamewithoutAuthorship_U = ifelse(g_f=='g', genus_U, family_U),
                      scientificNameAuthorship_U = '',
                      genus_U,
                      family_U) %>%
        dplyr::mutate(searchedName=searchedName,
                      taxon_status_of_searchedName = "",
                      plant_name_id_of_searchedName = "",
                      taxon_authors_of_searchedName = "",
                      verified_author = "",
                      verified_speciesName = "",
                      searchNotes=taxonRank)
    }
    ###

  }

  colnames(x) <- str_c('fb2020_',colnames(x))
  return(x)

}


