#' @title Check species names against World Checklist of Vascular Plants (WCVP) database
#'
#' @name wcvp_check_name
#'
#' @description Use the [World Checklist of Vascular Plants WCVP](https://powo.science.kew.org//)
#' [database](http://sftp.kew.org/pub/data-repositories/WCVP/) to check accepted names and update synonyms.
#' 
#' The World Checklist of Vascular Plants (WCVP) database is available from the 
#' [Royal Botanic Gardens, Kew](https://powo.science.kew.org/about-wcvp). 
#' It can be downloaded to a folder of the user’s choice or into memory using the get_wcvp function. The output has 33 columns.
#'
#' @param searchedName scientific name, with or without author
#' @param wcvp_names WCVP table, wcvp_names.csv file from http://sftp.kew.org/pub/data-repositories/WCVP/ If NA, automatically load the latest version of the database by the function parseGBIF::wcvp_get_data(read_only_to_memory = TRUE)$wcvp_names.
#' @param if_author_fails_try_without_combinations option for partial verification of the authorship of the species. Remove the authors of combinations, in parentheses
#'
#' @details About the World Checklist of Vascular Plants https://powo.science.kew.org/about-wcvp
#' searchNotes values:
#'
#' * Accepted - When only one authorless scientific name is present in the list of TAXON_name with
#' and TAXON_STATUS equal to "Accepted",
#' verified_speciesName = 100.
#' * Accepted among homonyms - When more than one authorless scientific name is present in the
#' TAXON_name list, but only one of the homonyms displays TAXON_STATUS equal to "Accepted",
#' verified_speciesName = number of matches/100.
#' * Homonyms - When more than one authorless scientific name is present in the TAXON_name list
#' and more than one, or none among the homonyms, display TAXON_STATUS equal to "Accepted",
#' verified_speciesName = number of matches/100.
#' Before searching for homonyms, there was a failure in trying to find the matching match between
#' authorless scientific name in TAXON_name and author in TAXON_AUTHORS, in these cases
#' verified_author equal to 0 (zero),
#' * Not Found: When the authorless scientific name is not present in the TAXON_NAME LIST
#' * Unplaced: o	When only one authorless scientific name is present in the list of TAXON_name with and TAXON_STATUS = "Unplaced"
#' * Updated: When only one authorless scientific name is present in the list of TAXON_name and ACCEPTED_PLANT_NAME_ID
#' are not empty (and ACCEPTED_PLANT_NAME_ID is different from the ID of the species consulted) taxon_status_of_searchedName, plant_name_id_of_searchedName and taxon_authors_of_searchedName values:
#'
#'    * When searchNotes equals "Updated" – The fields record the information of the scientific name originally consulted.
#'    * When searchNotes equals "Homonyms" - Fields record the information of homonymous synonyms separated by "|".
#'
#' * verified_author values:
#'
#'    * When value equal to 100 – when there is matched match between authorless scientific name in TAXON_name and author in TAXON_AUTHORS.
#'    * When value equal to 50 – when there is combined correspondence between authorless scientific name in TAXON_name and author, without (combination), in TAXON_AUTHORS.
#'    * When value equal to 0 – regardless of the correspondence between authorless scientific name in TAXON_name, author is not present in TAXON_AUTHORS.
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{wcvp_check_name_batch}}, \code{\link[parseGBIF]{wcvp_get_data}}
#'
#' @return Data frame with WCVP fields
#'
#' @import dplyr
#' @import stringr
#'
#' @examples
#' # These examples take >10 seconds to run and require 'parseGBIF::wcvp_get_data()'
#' \donttest{
#' library(parseGBIF)
#'
#' help(wcvp_check_name)
#'
#' wcvp_names <- wcvp_get_data(read_only_to_memory = TRUE)$wcvp_names
#'
#' # 1) Updated
#' wcvp_check_name(searchedName = 'Hemistylus brasiliensis Wedd.',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 2) Accepted
#' wcvp_check_name(searchedName = 'Hemistylus boehmerioides Wedd. ex Warm.',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 3) Unplaced - taxon_status = Unplaced
#' wcvp_check_name(searchedName = 'Leucosyke australis Unruh',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 4) Accepted among homonyms - When author is not informed. In this case, one of the homonyms, taxon_status is accepted
#' wcvp_check_name(searchedName = 'Parietaria cretica',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Parietaria cretica L.',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Parietaria cretica Moris',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 5) Homonyms - When author is not informed. In this case, none of the homonyms, taxon_status is Accepted
#' wcvp_check_name(searchedName = 'Laportea peltata',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Laportea peltata Gaudich. & Decne.',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Laportea peltata (Blume) Gaudich.',
#'                wcvp_names = wcvp_names,
#'                if_author_fails_try_without_combinations = TRUE)
#' }
#'
#' @import dplyr
#' @import stringr
#'
#' @export
wcvp_check_name <- function(searchedName = 'Hemistylus brasiliensis Wedd.',
                              wcvp_names =  '',
                              if_author_fails_try_without_combinations = TRUE)
{

  if(class(wcvp_names)!='data.frame')
  {
    stop("wcvp_names:  Inform wcvp_names data frame!")
  }

  x <- {}
  taxon_status <- ''
  sp_wcvp <- standardize_scientificName(searchedName)

  if(sp_wcvp$taxonAuthors != "")
  {

    index_author <- 100
    index <- wcvp_names$TAXON_NAME_U %in% toupper(sp_wcvp$standardizeName) &
      wcvp_names$TAXON_AUTHORS_U %in% toupper(gsub ("\\s+", "", sp_wcvp$taxonAuthors ))
    ntaxa <- NROW(wcvp_names[index==TRUE,])

    if(ntaxa == 0 & if_author_fails_try_without_combinations == TRUE)
    {
      index_author <- 50
      index <- wcvp_names$TAXON_NAME_U %in% toupper(sp_wcvp$standardizeName) &
        wcvp_names$TAXON_AUTHORS_U %in% toupper(gsub ("\\s+", "", sp_wcvp$taxonAuthors_last ))
      ntaxa <- NROW(wcvp_names[index==TRUE,])
    }


    if(ntaxa == 0)
    {
      index_author <- 0
      index <- wcvp_names$TAXON_NAME_U %in% toupper(sp_wcvp$standardizeName)
      ntaxa <- NROW(wcvp_names[index==TRUE,])
    }

  }else
  {
    index_author <- 0
    index <- wcvp_names$TAXON_NAME_U %in% toupper(sp_wcvp$standardizeName)
    ntaxa <- NROW(wcvp_names[index==TRUE,])
  }


  # Not found
  if(ntaxa == 0)
  {
    x <- wcvp_names[index==TRUE,] %>%
      dplyr::add_row()  %>%
      dplyr::mutate(searchedName=searchedName,
                    taxon_status_of_searchedName = NA,
                    plant_name_id_of_searchedName = NA,
                    taxon_authors_of_searchedName = NA,
                    verified_author = index_author,
                    verified_speciesName = 0,
                    searchNotes='Not found')


  }

  if(ntaxa == 1)
  {
    verified_speciesName <- 100

    teste_plant_name_id <- is.na(wcvp_names$accepted_plant_name_id[index==TRUE])

    id_accept <- ifelse(teste_plant_name_id==TRUE,'', wcvp_names$accepted_plant_name_id[index==TRUE])

    if((!teste_plant_name_id) &
       (wcvp_names$plant_name_id[index==TRUE] != id_accept ))
    {

      x <- wcvp_names[index==TRUE,]

      taxon_status_of_searchedName <- wcvp_names[index==TRUE,]$taxon_status
      plant_name_id_of_searchedName <- wcvp_names[index==TRUE,]$plant_name_id
      taxon_authors_of_searchedName <- wcvp_names[index==TRUE,]$taxon_authors

      index_synonym <- wcvp_names$plant_name_id %in% x$accepted_plant_name_id

      x <- wcvp_names[index_synonym==TRUE,] %>%
        dplyr::mutate(searchedName=searchedName,
                      taxon_status_of_searchedName = taxon_status_of_searchedName,
                      plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                      taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                      verified_author = index_author,
                      verified_speciesName = verified_speciesName,
                      searchNotes= 'Updated')
    }else
    {
      x <- wcvp_names[index==TRUE,] %>%
        # dplyr::add_row()  %>%
        dplyr::mutate(searchedName=searchedName,
                      taxon_status_of_searchedName = NA,
                      plant_name_id_of_searchedName = NA,
                      taxon_authors_of_searchedName = NA,
                      verified_author = index_author,
                      verified_speciesName = verified_speciesName,
                      searchNotes=taxon_status)
    }

  }

  if(ntaxa > 1)
  {

    taxon_status_of_searchedName <- paste(wcvp_names[index==TRUE,]$taxon_status, collapse = '|')
    plant_name_id_of_searchedName <- paste(wcvp_names[index==TRUE,]$plant_name_id, collapse = '|')
    # taxon_authors_of_searchedName <- paste(paste0(wcvp_names[index==TRUE,]$taxon_name, ' ',wcvp_names[index==TRUE,]$taxon_authors), collapse = '|')
    taxon_authors_of_searchedName <- paste(wcvp_names[index==TRUE,]$taxon_authors, collapse = '|')


    # Accepted or Homonyms
    {
      index_status <- wcvp_names$TAXON_NAME_U %in% toupper(sp_wcvp$standardizeName) &
        wcvp_names$taxon_status %in% c( "Accepted")

      ntaxa_status <- NROW(wcvp_names[index_status==TRUE,])

      if(ntaxa_status == 1)
      {

        x <- wcvp_names[index_status==TRUE,] %>%
          dplyr::mutate(searchedName=searchedName,
                        taxon_status_of_searchedName = taxon_status_of_searchedName,
                        plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                        taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                        verified_author = index_author,
                        verified_speciesName = 100/ntaxa,
                        searchNotes = 'Accepted among homonyms')#taxon_status)
      }
      else
      {


        x <- wcvp_names[1==2,] %>%
          dplyr::add_row()  %>%
          dplyr::mutate(searchedName=searchedName,
                        taxon_status_of_searchedName = taxon_status_of_searchedName,
                        plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                        taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                        verified_author = index_author,
                        verified_speciesName = 100/ntaxa,#0,
                        searchNotes='Homonyms')

      }

    }

  }

  colnames(x) <- str_c('wcvp_',colnames(x))
  return(x)

}
