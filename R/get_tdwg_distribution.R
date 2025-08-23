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

