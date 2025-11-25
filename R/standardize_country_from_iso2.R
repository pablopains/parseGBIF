#' @title Country names and codes from iso2
#' @name standardize_country_from_iso2
#'
#' @description Checks and standardizes country names and codes from iso2
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param iso2_field_name indicates the name of the field with ISO2 code of the countries
#' @param silence if TRUE does not display progress messages
#'
#' @details Converts ISO2 country codes to ISO3 codes and English country names using the countrycode package.
#'
#' @return
#' List with two data frames:
#' - `occ`: the original data set plus two columns, `parseGBIF_countryCode_ISO3` and `parseGBIF_countryName_en`
#' - `countrylist`: list of countries found with all columns from `countrycode::codelist`
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{prepare_gbif_occurrence_data}}, \code{\link[parseGBIF]{download_gbif_data_from_doi}}
#'
#' @examples
#' \donttest{
#' help(standardize_country_from_iso2)
#'
#' occ <- prepare_gbif_occurrence_data(
#'   gbif_occurrece_file = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt',
#'   columns = 'standard'
#' )
#'
#' x <- standardize_country_from_iso2(
#'   occ = occ,
#'   iso2_field_name = 'Ctrl_countryCode'
#' )
#'
#' colnames(x$occ)
#' head(x$countrylist)
#' }
#'
#' @importFrom countrycode codelist
#' @importFrom stats na.omit
#' @export
standardize_country_from_iso2 <- function(occ,
                                          iso2_field_name = 'Ctrl_countryCode',
                                          silence = TRUE)
{
  countrycode_table <- countrycode::codelist

  occ_sc <- data.frame(Ctrl_countryCode = occ[,iso2_field_name],
                       parseGBIF_countryCode_ISO3 = rep('',NROW(occ)),
                  parseGBIF_countryName_en = rep('',NROW(occ)))# countryCode_ISO3 = '')

  iso2c <- occ_sc[,iso2_field_name] %>% unique() %>% na.omit() %>% as.character()

  occ_x3 <- {}

  i<-1
  for(i in 1:NROW(iso2c))
  {
    if (silence==FALSE){print(paste0(i, ' de ', NROW(iso2c)))}

    index_1 <- occ_sc[,iso2_field_name] == iso2c[i]
    index_2 <- countrycode_table$iso2c %in% iso2c[i] & !is.na(countrycode_table$iso3c)

    if(any(index_2)==TRUE)
    {

      occ_sc$parseGBIF_countryCode_ISO3[index_1==TRUE] <- countrycode_table$iso3c[index_2==TRUE]
      occ_sc$parseGBIF_countryName_en[index_1==TRUE] <- countrycode_table$country.name.en[index_2==TRUE]

      if (silence==FALSE){print(paste0(iso2c[i], ' ', countrycode_table$iso3c[index_2==TRUE], ' ', countrycode_table$country.name.en[index_2==TRUE]))}
      occ_x3 <- rbind(occ_x3, countrycode_table[index_2==TRUE,]) #return_fields])
    }

  }

  return(list(occ = occ <- cbind(occ, occ_sc%>% dplyr::select(-iso2_field_name)),
              countrylist = occ_x3))

}
