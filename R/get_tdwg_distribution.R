#' @title Get TDWG Distribution Data from POWO
#' @name get_tdwg_distribution
#'
#' @description
#' Retrieves TDWG (Taxonomic Databases Working Group) distribution data
#' for a plant species from Plants of the World Online (POWO) API using
#' either scientific name or WCVP plant name ID.
#'
#' @param searchedName
#' Character. Scientific name of the plant species to search for.
#' Either this or `wcvp_plant_name_id` must be provided.
#'
#' @param wcvp_plant_name_id
#' Character. WCVP plant name ID for the species.
#' Either this or `searchedName` must be provided.
#'
#' @param wcvp_names
#' Data frame. WCVP names data containing taxon information with columns:
#' `taxon_name`, `taxon_status`, `plant_name_id`, `ipni_id`.
#'
#' @return
#' A data frame with TDWG distribution information containing:
#' - `LEVEL3_NAM`: TDWG level 3 region name
#' - `LEVEL3_COD`: TDWG level 3 region code
#' - `POWO_ID`: IPNI ID of the plant species
#' Returns `NULL` if no distribution data is found or if inputs are invalid.
#'
#' @details
#' This function queries the Plants of the World Online (POWO) API to retrieve
#' TDWG distribution data for native ranges of plant species. The function:
#'
#' 1. Resolves IPNI ID from either scientific name or WCVP plant name ID
#' 2. Queries POWO API for distribution data
#' 3. Processes and standardizes the returned distribution information
#' 4. Handles character encoding and API errors gracefully
#'
#' @note
#' - Requires an internet connection to access POWO API
#' - Only returns native distribution ranges (not introduced ranges)
#' - Returns NULL for invalid inputs or API errors
#' - WCVP names data must contain IPNI ID mappings
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @examples
#' \donttest{
#' # Get distribution by scientific name
#' distribution <- get_tdwg_distribution(
#'   searchedName = "Quercus robur",
#'   wcvp_names = wcvp_names_data
#' )
#'
#' # Get distribution by WCVP plant name ID
#' distribution <- get_tdwg_distribution(
#'   wcvp_plant_name_id = "123456",
#'   wcvp_names = wcvp_names_data
#' )
#'
#' # View distribution data
#' if (!is.null(distribution)) {
#'   head(distribution)
#' }
#' }
#'
#' @importFrom httr GET content http_error
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate rename recode
#' @export
get_tdwg_distribution <- function(searchedName = NA,
                                  wcvp_plant_name_id = NA,
                                  wcvp_names = NA) {

  # Validate inputs
  if (is.na(searchedName) && is.na(wcvp_plant_name_id)) {
    stop("Either 'searchedName' or 'wcvp_plant_name_id' must be provided.")
  }

  if (is.na(wcvp_names) || nrow(wcvp_names) == 0) {
    stop("Valid 'wcvp_names' data frame is required.")
  }

  ipni_id <- NA

  # Resolve IPNI ID from scientific name
  if (!is.na(searchedName)) {
    index <- wcvp_names$taxon_name == searchedName &
      wcvp_names$taxon_status == "Accepted"
    if (sum(index, na.rm = TRUE) > 0) {
      ipni_id <- wcvp_names$ipni_id[index]
      ipni_id <- ipni_id[1]  # Take first match if multiple
    }
  }

  # Resolve IPNI ID from WCVP plant name ID
  if (!is.na(wcvp_plant_name_id)) {
    index <- wcvp_names$plant_name_id == wcvp_plant_name_id
    if (sum(index, na.rm = TRUE) > 0) {
      ipni_id <- wcvp_names$ipni_id[index]
      ipni_id <- ipni_id[1]  # Take first match if multiple
    }
  }

  # Query POWO API if IPNI ID found
  if (!is.na(ipni_id)) {
    lookup_url <- paste0(
      "http://plantsoftheworldonline.org/api/2/taxon/urn:lsid:ipni.org:names:",
      ipni_id
    )

    response <- httr::GET(lookup_url, query = list(fields = "distribution"))

    if (!httr::http_error(response)) {
      returned_data <- jsonlite::fromJSON(httr::content(response, as = "text"))
      distribution <- returned_data$distribution$natives

      if (!is.null(distribution)) {
        results <- distribution %>%
          dplyr::mutate(POWO_ID = ipni_id) %>%
          dplyr::rename(LEVEL3_NAM = name, LEVEL3_COD = tdwgCode) %>%
          dplyr::mutate(LEVEL3_NAM = dplyr::recode(LEVEL3_NAM, "á" = "a"))
        return(results)
      }
    }
  }

  # Return NULL if no distribution data found
  return(NULL)
}
# get_tdwg_distribution <- function(searchedName = NA,
#                                     wcvp_plant_name_id = NA,
#                                     wcvp_names = NA){
#   ipni_id <- NA
#
#   if(!is.na(searchedName))
#   {
#     index <- wcvp_names$taxon_name == searchedName &
#              wcvp_names$taxon_status == "Accepted"
#     if(sum(index)>0)
#     {
#       ipni_id <- wcvp_names$ipni_id[index==TRUE]
#     }
#   }
#
#   if(!is.na(wcvp_plant_name_id))
#   {
#     index <- wcvp_names$plant_name_id == wcvp_plant_name_id
#     if(sum(index)>0)
#     {
#       ipni_id <- wcvp_names$ipni_id[index==TRUE]
#     }
#   }
#
#   if(!is.na(ipni_id[]))
#   {
#     lookup_url <- paste("http://plantsoftheworldonline.org/api/2/taxon/urn:lsid:ipni.org:names:", ipni_id, sep="")
#     response <- httr::GET(lookup_url, query=list(fields="distribution"))
#     if (! httr::http_error(response)) {
#       returned_data <- jsonlite::fromJSON(httr::content(response, as="text"))
#
#       distribution <- returned_data$distribution$natives
#
#       if (! is.null(distribution)) {
#         results = mutate(distribution, POWO_ID=ipni_id)
#         results = rename(results, LEVEL3_NAM=name, LEVEL3_COD=tdwgCode)
#         results = mutate(results, LEVEL3_NAM=recode(LEVEL3_NAM, "á"="a"))
#       }
#
#
#     }
#
#     return(results)
#
#   }
#   return(NULL)
# }

