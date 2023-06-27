#' @title Export of results
#' @name export_data
#'
#' @description Separate records into three data frames
#' Export of results:
#' * Useful data for spatial and taxonomic analysis
#' * Data in need of revision of spatial information or without identification
#' * Duplicates of the previous two datasets
#'
#' @param occ_digital_voucher_file CSV fila result of function select_digital_voucher_and_sample_identification()$occ_digital_voucher_and_sample_identification
#' @param occ_digital_voucher data frame result of function select_digital_voucher_and_sample_identification()$occ_digital_voucher_and_sample_identification
#'
#' @details Each data frame should be used as needed
#'
#' @return list with three data frames:
#' occ_in, Useful data for spatial and taxonomic analysis,
#' occ_out_to_recover, data in need of spatial data revision or without identification and
#' occ_dup, duplicates.
#'
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @examples
#' \donttest{
#' help(export_data)
#'
#' }
#' @export
export_data <- function(occ_digital_voucher_file = '',
                        occ_digital_voucher = NA)
{

  print('Loading occurrence file...')

  if(occ_digital_voucher_file !=''  )
  {
    if(!file.exists(occ_digital_voucher_file))
    {
      stop("Invalid occurrence file!")
    }

    occ_tmp <- readr::read_csv(occ_digital_voucher_file,
                               locale = readr::locale(encoding = "UTF-8"),
                               show_col_types = FALSE)
  }else
  {

    if (NROW(occ_digital_voucher)==0)
    {
      stop("Empty occurrence data frame!")
    }

    occ_tmp <- occ_digital_voucher
    rm(occ_digital_voucher)
  }

  print('Exporting data...')

  occ_in <- occ_dup <- occ_out_to_recover <- {}

  occ_tmp <- occ_tmp %>%
    dplyr::mutate(ID = 1:nrow(occ_tmp))

  occ_in = occ_tmp %>% dplyr::filter(
    ( (Ctrl_selectedMoreInformativeRecord == TRUE) &
        (Ctrl_useful_spatial_analysis == TRUE)) &
      (Ctrl_sampleIdentificationStatus == 'identified'))  %>%
    dplyr::mutate(Ctrl_need_revise_id_duplicates = ifelse(Ctrl_numberTaxonNamesSample==1,FALSE,TRUE))

  occ_out_to_recover <- anti_join(occ_tmp %>%
                                    dplyr::filter(Ctrl_selectedMoreInformativeRecord == TRUE), occ_in, "ID")

  occ_dup <- anti_join(occ_tmp %>%
                         dplyr::filter(Ctrl_selectedMoreInformativeRecord == FALSE), occ_in, "ID")

  return(list(occ_in = occ_in,
              occ_out_to_recover= occ_out_to_recover,
              occ_dup = occ_dup))

}
