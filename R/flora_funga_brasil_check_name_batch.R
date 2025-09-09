#' @title In batch, use the WCVP database to check accepted names and update synonyms
#'
#' @name flora_funga_brasil_check_name_batch
#'
#' @description Species’ names can be checked against WCVP database one by one, or in a batch mode. 
#' To verify individual names, the function wcvp_check_name is used.
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param wcvp_names get data frame in parseGBIF::wcvp_get_data(read_only_to_memory = TRUE)$wcvp_names
#' or configure function to save a copy on local disk to optimize loading, see details in help(wcvp_get_data)
#' @param if_author_fails_try_without_combinations option for partial verification of the authorship of the species.
#' Remove the authors of combinations, in parentheses.
#' @param wcvp_selected_fields WCVP fields selected as return, 'standard' basic columns, 'all' all available columns.
#' The default is 'standard'
#' @param silence if TRUE does not display progress messages
#'
#' @details See help(checkName_wcvp) 
#' * [about WCVP database](http://sftp.kew.org/pub/data-repositories/WCVP/)
#' * [World Checklist of Vascular Plants](https://powo.science.kew.org//)
#' * [WCVP database](http://sftp.kew.org/pub/data-repositories/WCVP/)
#' * [(about WCVP)](https://powo.science.kew.org/about-wcvp)

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
flora_funga_brasil_check_name_batch <- function(occ = NA,
                                                field_search = 'Ctrl_scientificName',
                                                flora_funga_brasil = NA,
                                                silence = TRUE,
                                                join_result = FALSE)
{
  
  colunas_fb2020_sel <<- c("fb2020_taxonID",
                           "fb2020_acceptedNameUsageID",
                           "fb2020_parentNameUsageID",
                           "fb2020_originalNameUsageID",
                           "fb2020_scientificName",
                           # "fb2020_acceptedNameUsage",
                           # "fb2020_parentNameUsage",
                           "fb2020_namePublishedIn",                  
                           "fb2020_namePublishedInYear",
                           "fb2020_higherClassification",             
                           # "fb2020_kingdom",
                           # "fb2020_phylum",                           
                           # "fb2020_class",
                           # "fb2020_order",                            
                           "fb2020_family",
                           # "fb2020_genus",                            
                           "fb2020_specificEpithet",
                           "fb2020_infraspecificEpithet",             
                           "fb2020_taxonRank",
                           "fb2020_scientificNameAuthorship",
                           "fb2020_taxonomicStatus",
                           "fb2020_nomenclaturalStatus",              
                           "fb2020_modified",
                           "fb2020_bibliographicCitation",
                           "fb2020_references",
                           "fb2020_scientificNamewithoutAuthorship",  
                           "fb2020_scientificNamewithoutAuthorship_U",
                           "fb2020_searchNotes",
                           "fb2020_searchedName")
  
  occ <- occ %>%
    dplyr::mutate(fb2020_taxonID = 0,
                  fb2020_acceptedNameUsageID = 0,
                  fb2020_parentNameUsageID = 0,
                  fb2020_originalNameUsageID = 0,
                  fb2020_scientificName = "",
                  # fb2020_acceptedNameUsage = "",
                  # fb2020_parentNameUsage = "",
                  fb2020_namePublishedIn = "",
                  fb2020_namePublishedInYear = 0,
                  fb2020_higherClassification = "",             
                  # fb2020_kingdom = "",
                  # fb2020_phylum = "",                     
                  # fb2020_class = "",
                  # fb2020_order = "",                       
                  fb2020_family = "",
                  # fb2020_genus = "",                      
                  fb2020_specificEpithet = "",
                  fb2020_infraspecificEpithet = "",             
                  fb2020_taxonRank = "",
                  fb2020_scientificNameAuthorship = "",
                  fb2020_taxonomicStatus = "",
                  fb2020_nomenclaturalStatus = "",             
                  fb2020_modified = lubridate::as_datetime("2021-10-31 21:13:33.77"),
                  fb2020_bibliographicCitation = "",
                  fb2020_references = "",
                  fb2020_scientificNamewithoutAuthorship = "",
                  fb2020_scientificNamewithoutAuthorship_U = "",
                  fb2020_searchNotes = "",
                  fb2020_searchedName = "")
  
  x <- {}
  i <- 1
  # i <- 938
  
  ok <- FALSE
  
  name_search <- occ[,field_search] %>% unique() 
  colnames(name_search) <- 'name_search'
  
  
  japrocessado <<- rep(FALSE,NROW(name_search))
  
  while (i<=NROW(name_search) & ok != TRUE)
  {
    try(
      {
        
        for(i in 1:NROW(name_search))
        {
          if(japrocessado[i]==TRUE){next} # 03-05-2022 para evitar tentar baixar a mesma espécies 2 vezes caso ocorra erro
          
          sp_tmp <- name_search$name_search[i]
          
          
            x_tmp_fb2020 <- flora_funga_brasil_check_name(searchedName = sp_tmp,
                                                          flora_funga_brasil = flora_funga_brasil,
                                                          silence = silence)
          
          # x <- rbind(x, cbind(x_tmp_fb2020[,colunas_fb2020_sel],
          #                     occ))
          
          index <- occ$Ctrl_scientificName %in% sp_tmp
          occ[index==TRUE,colunas_fb2020_sel] <- x_tmp_fb2020[,colunas_fb2020_sel]

          
          if(silence==FALSE)
          {
            print( paste0( i, ':',NROW(name_search), ' - FB2020: ',  x_tmp_fb2020$fb2020_scientificName, ', rec.up.',sum(index)))
          }
          
          # occ_all[index==TRUE,]$scientificNamewithoutAuthorship
          
          # occ_all[index==TRUE,
          #         c(colunas_wcvp_sel, colunas_fb2020_sel)] <- cbind(x_tmp[,
          #                                                                 colunas_wcvp_sel],
          #                                                           x_tmp_fb2020[,
          #                                                                        colunas_fb2020_sel])
          # 
          
          # occ_all[index==TRUE,
          #         colunas_fb2020_sel] <- x_tmp_fb2020[,colunas_fb2020_sel]
          
          
          japrocessado[i] <- TRUE
        }
        
        ok <- TRUE
      })
    
    print('reconectar em 2 segundos...')
    Sys.sleep(2)
  }
  
  if (join_result == FALSE)
  {
    return(occ %>% dplyr::select(colunas_fb2020_sel))
  }else
  {return(occ) }
  
}

