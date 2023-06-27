#' @title In batch, use the WCVP database to check accepted names and update synonyms
#'
#' @name wcvp_check_name_batch
#'
#' @description In batch, use the [World Checklist of Vascular Plants](https://powo.science.kew.org//)
#' [database](http://sftp.kew.org/pub/data-repositories/WCVP/)
#' [(about WCVP)](https://powo.science.kew.org/about-wcvp) to check accepted names and update synonyms
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param wcvp_names get data frame in parseGBIF::wcvp_get_data(read_only_to_memory = TRUE)$wcvp_names
#' or configure function to save a copy on local disk to optimize loading, see details in help(wcvp_get_data)
#' @param if_author_fails_try_without_combinations option for partial verification of the authorship of the species.
#' Remove the authors of combinations, in parentheses.
#' @param wcvp_selected_fields WCVP fields selected as return, 'standard' basic columns, 'all' all available columns.
#' The default is 'standard'
#'
#' @details See help(checkName_wcvp) and [about WCVP database](http://sftp.kew.org/pub/data-repositories/WCVP/)
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{wcvp_get_data}}, \code{\link[parseGBIF]{wcvp_check_name}}
#'
#' @return list with two data frames: summary, species list and occ_wcvp_check_name, with WCVP fields
#'
#' @import dplyr
#' @import stringr
#' @import tidyselect
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' # These examples take >10 minutes to run and require 'parseGBIF::wcvp_get_data()'
#' \donttest{
#'
#' library(parseGBIF)
#'
#' help(wcvp_check_name_batch)
#'
#' occ_file <- 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt'
#'
#' occ <- prepare_gbif_occurrence_data(gbif_occurrece_file = occ_file,
#'                                     columns = 'standard')
#'
#' # wcvp_names <- wcvp_get_data(read_only_to_memory = TRUE)$wcvp_names
#' data(wcvp_names_Achatocarpaceae)
#'
#' head(wcvp_names)
#'
#' res_wcvp_check_name_batch <- wcvp_check_name_batch(occ = occ,
#'                                                  wcvp_names =  wcvp_names,
#'                                                  if_author_fails_try_without_combinations = TRUE,
#'                                                  wcvp_selected_fields = 'standard',
#'                                                  show_process = TRUE)
#'
#' names(res_wcvp_check_name_batch)
#'
#' head(res_wcvp_check_name_batch$summary)
#'
#' head(res_wcvp_check_name_batch$occ_wcvp_check_name)
#'
#' }
#'
#' @import dplyr
#' @import stringr
#' @import tidyselect
#'
#' @export
wcvp_check_name_batch <- function(occ = NA,
                                 wcvp_names = '',
                                 if_author_fails_try_without_combinations = TRUE,
                                 wcvp_selected_fields = 'standard')
{

  if(class(wcvp_names)!='data.frame')
  {
    stop("wcvp_names: Inform wcvp_names data frame!")
  }

  if(!wcvp_selected_fields %in% c('standard','all'))
  {
    stop("wcvp_selected_fields: standard or all!")
  }

  if (wcvp_selected_fields == 'standard')
  {
    wcvp_na <- data.frame(wcvp_plant_name_id  = NA,
                          # wcvp_ipni_id = NA,
                          wcvp_taxon_rank = NA,
                          wcvp_taxon_status = NA,
                          wcvp_family = NA,
                          # wcvp_genus_hybrid = NA,
                          # wcvp_genus = NA,
                          # wcvp_species_hybrid = NA,
                          # wcvp_species = NA,
                          # wcvp_infraspecific_rank = NA,
                          # wcvp_infraspecies = NA,
                          # wcvp_parenthetical_author = NA,
                          # wcvp_primary_author = NA,
                          # wcvp_publication_author = NA,
                          # wcvp_place_of_publication = NA,
                          # wcvp_volume_and_page = NA,
                          # wcvp_first_published = NA,
                          # wcvp_nomenclatural_remarks = NA,
                          # wcvp_geographic_area = NA,
                          # wcvp_lifeform_description = NA,
                          # wcvp_climate_description = NA,
                          wcvp_taxon_name = NA,
                          wcvp_taxon_authors = NA,
                          wcvp_accepted_plant_name_id = NA,
                          # wcvp_basionym_plant_name_id = NA,
                          # wcvp_replaced_synonym_author = NA,
                          # wcvp_homotypic_synonym = NA,
                          # wcvp_parent_plant_name_id = NA,
                          # wcvp_powo_id = NA,
                          # wcvp_hybrid_formula = NA,
                          wcvp_reviewed = NA,
                          # # wcvp_TAXON_NAME_U = NA,
                          wcvp_searchedName = NA,
                          wcvp_taxon_status_of_searchedName = NA,
                          wcvp_plant_name_id_of_searchedName = NA,
                          wcvp_taxon_authors_of_searchedName = NA,
                          wcvp_verified_author = NA,
                          wcvp_verified_speciesName = NA,
                          wcvp_searchNotes = NA)
  }

  if (wcvp_selected_fields == 'all')
  {
    wcvp_na <- data.frame(wcvp_plant_name_id  = NA,
                          wcvp_ipni_id = NA,
                          wcvp_taxon_rank = NA,
                          wcvp_taxon_status = NA,
                          wcvp_family = NA,
                          wcvp_genus_hybrid = NA,
                          wcvp_genus = NA,
                          wcvp_species_hybrid = NA,
                          wcvp_species = NA,
                          wcvp_infraspecific_rank = NA,
                          wcvp_infraspecies = NA,
                          wcvp_parenthetical_author = NA,
                          wcvp_primary_author = NA,
                          wcvp_publication_author = NA,
                          wcvp_place_of_publication = NA,
                          wcvp_volume_and_page = NA,
                          wcvp_first_published = NA,
                          wcvp_nomenclatural_remarks = NA,
                          wcvp_geographic_area = NA,
                          wcvp_lifeform_description = NA,
                          wcvp_climate_description = NA,
                          wcvp_taxon_name = NA,
                          wcvp_taxon_authors = NA,
                          wcvp_accepted_plant_name_id = NA,
                          wcvp_basionym_plant_name_id = NA,
                          wcvp_replaced_synonym_author = NA,
                          wcvp_homotypic_synonym = NA,
                          wcvp_parent_plant_name_id = NA,
                          wcvp_powo_id = NA,
                          wcvp_hybrid_formula = NA,
                          wcvp_reviewed = NA,
                          # wcvp_TAXON_NAME_U = NA,
                          wcvp_searchedName = NA,
                          wcvp_taxon_status_of_searchedName = NA,
                          wcvp_plant_name_id_of_searchedName = NA,
                          wcvp_taxon_authors_of_searchedName = NA,
                          wcvp_verified_author = NA,
                          wcvp_verified_speciesName = NA,
                          wcvp_searchNotes = NA)
  }

  index <- toupper(occ$Ctrl_taxonRank) %in%
    toupper(c('SPECIES',
              'VARIETY',
              'SUBSPECIES',
              'FORM'))

  colunas_wcvp_sel <- colnames(wcvp_na)

  occ_all <- cbind(occ, wcvp_na) %>%
    dplyr::mutate(wcvp_searchedName = Ctrl_scientificName) %>%
    dplyr::select(tidyselect::all_of(colunas_wcvp_sel))

  name_search_wcvp <- occ_all[index==TRUE,]$wcvp_searchedName %>% unique() %>% as.character()

  x <- {}
  i <- 1
  tot_rec <- NROW(name_search_wcvp)

  for(i in 1:tot_rec)
  {
    sp_tmp <- name_search_wcvp[i]

    print( paste0( i, '-',tot_rec ,' ',  sp_tmp))

    x_tmp <- wcvp_check_name(searchedName = sp_tmp,
                            wcvp_names = wcvp_names,
                            if_author_fails_try_without_combinations = TRUE)

    x <- rbind(x,
               cbind(x_tmp[,
                           tidyselect::all_of(colunas_wcvp_sel)]))


    index <- occ_all$wcvp_searchedName %in% sp_tmp
    occ_all[index==TRUE, tidyselect::all_of(colunas_wcvp_sel)] <- x_tmp[, tidyselect::all_of(colunas_wcvp_sel)]

  }

  return(list(occ_wcvp_check_name=occ_all[,tidyselect::all_of(colunas_wcvp_sel)],
              summary=x))
}
