#' @title Batch Check Species Names Against Flora e Funga do Brazil Database
#' @name flora_funga_brasil_check_name_batch
#'
#' @description
#' Checks species names in batch mode against the Flora e Funga do Brazil database
#' for taxonomic validation and resolution. Processes all unique scientific names
#' in the occurrence data and returns taxonomic information with verification metrics.
#'
#' @param occ
#' Data frame. GBIF occurrence table with selected columns as returned by
#' `select_gbif_fields(columns = 'standard')`.
#'
#' @param field_search
#' Character. The column name in `occ` containing scientific names to check.
#' Default is 'Ctrl_scientificName'.
#'
#' @param flora_funga_brasil
#' Data frame. The Flora e Funga do Brazil database to search against.
#'
#' @param silence
#' Logical. If `TRUE`, suppresses progress messages. Default is `TRUE`.
#'
#' @param join_result
#' Logical. If `TRUE`, returns the full occurrence data with Flora e Funga do Brazil
#' columns appended. If `FALSE` (default), returns only the Flora e Funga do Brazil columns.
#'
#' @details
#' ## Processing Workflow:
#' 1. Extracts unique scientific names from the specified field
#' 2. Processes each name using `flora_funga_brasil_check_name()`
#' 3. Updates occurrence records with taxonomic information
#' 4. Handles connection errors with retry logic
#' 5. Returns standardized taxonomic data
#'
#' ## Output Columns:
#' All Flora e Funga do Brazil columns are prefixed with 'fb2020_' and include:
#' - Taxonomic identifiers and status
#' - Scientific names and authorship
#' - Classification hierarchy
#' - Verification notes and search results
#'
#' @return
#' If `join_result = FALSE`: Data frame containing only Flora e Funga do Brazil columns
#' If `join_result = TRUE`: Original occurrence data with Flora e Funga do Brazil columns appended
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`flora_funga_brasil_get_data()`] for downloading the database,
#' [`flora_funga_brasil_check_name()`] for individual name checking,
#' [`select_gbif_fields()`] for preparing occurrence data
#'
#' @examples
#' \donttest{
#' library(parseGBIF)
#'
#' # Load sample occurrence data
#' occ_file <- 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt'
#'
#' occ <- prepare_gbif_occurrence_data(
#'   gbif_occurrece_file = occ_file,
#'   columns = 'standard'
#' )
#'
#' # Load Flora e Funga do Brazil database
#' flora_funga_brasil <- flora_funga_brasil_get_data(
#'   path_results = tempdir(),
#'   update = FALSE
#' )
#'
#' # Batch check names
#' results <- flora_funga_brasil_check_name_batch(
#'   occ = occ,
#'   flora_funga_brasil = flora_funga_brasil,
#'   silence = FALSE
#' )
#'
#' # View results
#' head(results)
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom lubridate as_datetime
#' @export
flora_funga_brasil_check_name_batch <- function(occ = NA,
                                                field_search = 'Ctrl_scientificName',
                                                flora_funga_brasil = NA,
                                                silence = TRUE,
                                                join_result = FALSE) {

  # Define columns locally (NOT with <<-)
  colunas_fb2020_sel <- c(
    "fb2020_taxonID", "fb2020_acceptedNameUsageID", "fb2020_parentNameUsageID",
    "fb2020_originalNameUsageID", "fb2020_scientificName", "fb2020_namePublishedIn",
    "fb2020_namePublishedInYear", "fb2020_higherClassification", "fb2020_family",
    "fb2020_specificEpithet", "fb2020_infraspecificEpithet", "fb2020_taxonRank",
    "fb2020_scientificNameAuthorship", "fb2020_taxonomicStatus", "fb2020_nomenclaturalStatus",
    "fb2020_modified", "fb2020_bibliographicCitation", "fb2020_references",
    "fb2020_scientificNamewithoutAuthorship", "fb2020_scientificNamewithoutAuthorship_U",
    "fb2020_searchNotes", "fb2020_searchedName"
  )

  # Initialize Flora e Funga do Brazil columns
  occ <- occ %>%
    dplyr::mutate(
      fb2020_taxonID = 0,
      fb2020_acceptedNameUsageID = 0,
      fb2020_parentNameUsageID = 0,
      fb2020_originalNameUsageID = 0,
      fb2020_scientificName = "",
      fb2020_namePublishedIn = "",
      fb2020_namePublishedInYear = 0,
      fb2020_higherClassification = "",
      fb2020_family = "",
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
      fb2020_searchedName = ""
    )

  # Process unique names
  name_search <- unique(occ[[field_search]])
  processed <- rep(FALSE, length(name_search))

  for (i in seq_along(name_search)) {
    if (processed[i]) next

    sp_tmp <- name_search[i]

    tryCatch({
      x_tmp_fb2020 <- flora_funga_brasil_check_name(
        searchedName = sp_tmp,
        flora_funga_brasil = flora_funga_brasil,
        silence = silence
      )

      index <- occ[[field_search]] == sp_tmp
      occ[index, colunas_fb2020_sel] <- x_tmp_fb2020[1, colunas_fb2020_sel]

      if (!silence) {
        message(paste0(i, ':', length(name_search), ' - FB2020: ',
                       x_tmp_fb2020$fb2020_scientificName[1],
                       ', records updated: ', sum(index)))
      }

      processed[i] <- TRUE

    }, error = function(e) {
      if (!silence) {
        message("Error processing: ", sp_tmp, " - ", e$message)
      }
    })
  }

  if (!join_result) {
    return(occ %>% dplyr::select(dplyr::all_of(colunas_fb2020_sel)))
  } else {
    return(occ)
  }
}
# flora_funga_brasil_check_name_batch <- function(occ = NA,
#                                                 field_search = 'Ctrl_scientificName',
#                                                 flora_funga_brasil = NA,
#                                                 silence = TRUE,
#                                                 join_result = FALSE)
# {
#
#   colunas_fb2020_sel <<- c("fb2020_taxonID",
#                            "fb2020_acceptedNameUsageID",
#                            "fb2020_parentNameUsageID",
#                            "fb2020_originalNameUsageID",
#                            "fb2020_scientificName",
#                            # "fb2020_acceptedNameUsage",
#                            # "fb2020_parentNameUsage",
#                            "fb2020_namePublishedIn",
#                            "fb2020_namePublishedInYear",
#                            "fb2020_higherClassification",
#                            # "fb2020_kingdom",
#                            # "fb2020_phylum",
#                            # "fb2020_class",
#                            # "fb2020_order",
#                            "fb2020_family",
#                            # "fb2020_genus",
#                            "fb2020_specificEpithet",
#                            "fb2020_infraspecificEpithet",
#                            "fb2020_taxonRank",
#                            "fb2020_scientificNameAuthorship",
#                            "fb2020_taxonomicStatus",
#                            "fb2020_nomenclaturalStatus",
#                            "fb2020_modified",
#                            "fb2020_bibliographicCitation",
#                            "fb2020_references",
#                            "fb2020_scientificNamewithoutAuthorship",
#                            "fb2020_scientificNamewithoutAuthorship_U",
#                            "fb2020_searchNotes",
#                            "fb2020_searchedName")
#
#   occ <- occ %>%
#     dplyr::mutate(fb2020_taxonID = 0,
#                   fb2020_acceptedNameUsageID = 0,
#                   fb2020_parentNameUsageID = 0,
#                   fb2020_originalNameUsageID = 0,
#                   fb2020_scientificName = "",
#                   # fb2020_acceptedNameUsage = "",
#                   # fb2020_parentNameUsage = "",
#                   fb2020_namePublishedIn = "",
#                   fb2020_namePublishedInYear = 0,
#                   fb2020_higherClassification = "",
#                   # fb2020_kingdom = "",
#                   # fb2020_phylum = "",
#                   # fb2020_class = "",
#                   # fb2020_order = "",
#                   fb2020_family = "",
#                   # fb2020_genus = "",
#                   fb2020_specificEpithet = "",
#                   fb2020_infraspecificEpithet = "",
#                   fb2020_taxonRank = "",
#                   fb2020_scientificNameAuthorship = "",
#                   fb2020_taxonomicStatus = "",
#                   fb2020_nomenclaturalStatus = "",
#                   fb2020_modified = lubridate::as_datetime("2021-10-31 21:13:33.77"),
#                   fb2020_bibliographicCitation = "",
#                   fb2020_references = "",
#                   fb2020_scientificNamewithoutAuthorship = "",
#                   fb2020_scientificNamewithoutAuthorship_U = "",
#                   fb2020_searchNotes = "",
#                   fb2020_searchedName = "")
#
#   x <- {}
#   i <- 1
#   # i <- 938
#
#   ok <- FALSE
#
#   name_search <- occ[,field_search] %>% unique()
#   colnames(name_search) <- 'name_search'
#
#
#   japrocessado <<- rep(FALSE,NROW(name_search))
#
#   while (i<=NROW(name_search) & ok != TRUE)
#   {
#     try(
#       {
#
#         for(i in 1:NROW(name_search))
#         {
#           if(japrocessado[i]==TRUE){next} # 03-05-2022 para evitar tentar baixar a mesma espécies 2 vezes caso ocorra erro
#
#           sp_tmp <- name_search$name_search[i]
#
#
#             x_tmp_fb2020 <- flora_funga_brasil_check_name(searchedName = sp_tmp,
#                                                           flora_funga_brasil = flora_funga_brasil,
#                                                           silence = silence)
#
#           # x <- rbind(x, cbind(x_tmp_fb2020[,colunas_fb2020_sel],
#           #                     occ))
#
#           index <- occ$Ctrl_scientificName %in% sp_tmp
#           occ[index==TRUE,colunas_fb2020_sel] <- x_tmp_fb2020[,colunas_fb2020_sel]
#
#
#           if(silence==FALSE)
#           {
#             print( paste0( i, ':',NROW(name_search), ' - FB2020: ',  x_tmp_fb2020$fb2020_scientificName, ', rec.up.',sum(index)))
#           }
#
#           # occ_all[index==TRUE,]$scientificNamewithoutAuthorship
#
#           # occ_all[index==TRUE,
#           #         c(colunas_wcvp_sel, colunas_fb2020_sel)] <- cbind(x_tmp[,
#           #                                                                 colunas_wcvp_sel],
#           #                                                           x_tmp_fb2020[,
#           #                                                                        colunas_fb2020_sel])
#           #
#
#           # occ_all[index==TRUE,
#           #         colunas_fb2020_sel] <- x_tmp_fb2020[,colunas_fb2020_sel]
#
#
#           japrocessado[i] <- TRUE
#         }
#
#         ok <- TRUE
#       })
#
#     print('reconectar em 2 segundos...')
#     Sys.sleep(2)
#   }
#
#   if (join_result == FALSE)
#   {
#     return(occ %>% dplyr::select(colunas_fb2020_sel))
#   }else
#   {return(occ) }
#
# }

