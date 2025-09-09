#' @title Get TDWG Distribution Data from POWO
#' @name get_tdwg_distribution
#'
#' @description
#' Retrieves TDWG (Taxonomic Databases Working Group) distribution data
#' for a plant species from Plants of the World Online (POWO) API.
#'
#' @param searchedName
#' Character. Scientific name of the plant species to search for.
#'
#' @param wcvp_plant_name_id
#' Character. WCVP plant name ID for the species.
#'
#' @param wcvp_names
#' Data frame. WCVP names data containing taxon information.
#'
#' @return
#' Returns a data frame with TDWG distribution information containing:
#' - `LEVEL3_NAM`: TDWG level 3 region name
#' - `LEVEL3_COD`: TDWG level 3 region code
#' - `POWO_ID`: IPNI ID of the plant species
#' Returns `NULL` if no distribution data is found or if inputs are invalid.
#'
#' @details
#' This function queries the Plants of the World Online (POWO) API to retrieve
#' TDWG distribution data for a given plant species. It can search by either:
#' - Scientific name (searchedName)
#' - WCVP plant name ID (wcvp_plant_name_id)
#'
#' The function requires a WCVP names data frame to resolve IPNI IDs from
#' either scientific names or WCVP plant name IDs.
#'
#' @note
#' - Requires an internet connection to access POWO API
#' - Depends on WCVP names data for IPNI ID resolution
#' - Returns NULL if no matching distribution data is found
#' - Handles URL encoding and API response parsing
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
#' head(distribution)
#' }
#'
#' @importFrom httr GET content http_error
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate rename
#' @export
get_tdwg_distribution <- function(searchedName = NA,
                                    wcvp_plant_name_id = NA,
                                    wcvp_names = NA){
  ipni_id <- NA

  if(!is.na(searchedName))
  {
    index <- wcvp_names$taxon_name == searchedName &
             wcvp_names$taxon_status == "Accepted"
    if(sum(index)>0)
    {
      ipni_id <- wcvp_names$ipni_id[index==TRUE]
    }
  }

  if(!is.na(wcvp_plant_name_id))
  {
    index <- wcvp_names$plant_name_id == wcvp_plant_name_id
    if(sum(index)>0)
    {
      ipni_id <- wcvp_names$ipni_id[index==TRUE]
    }
  }

  if(!is.na(ipni_id[]))
  {
    lookup_url <- paste("http://plantsoftheworldonline.org/api/2/taxon/urn:lsid:ipni.org:names:", ipni_id, sep="")
    response <- httr::GET(lookup_url, query=list(fields="distribution"))
    if (! httr::http_error(response)) {
      returned_data <- jsonlite::fromJSON(httr::content(response, as="text"))

      distribution <- returned_data$distribution$natives

      if (! is.null(distribution)) {
        results = mutate(distribution, POWO_ID=ipni_id)
        results = rename(results, LEVEL3_NAM=name, LEVEL3_COD=tdwgCode)
        results = mutate(results, LEVEL3_NAM=recode(LEVEL3_NAM, "á"="a"))
      }


    }

    return(results)

  }
  return(NULL)
}

