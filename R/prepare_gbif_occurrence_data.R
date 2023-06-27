#' @title Prepare occurrence data from GBIF to use in package
#'
#' @name prepare_gbif_occurrence_data
#'
#' @description Prepare occurrence data downloaded from GBIF to be used by ParsGBIF functions
#'
#' @param gbif_occurrece_file The name of the file from which the with occurrence data
#' downloaded from GBIF (by default "occurrence.txt")
#' @param columns Character vector of strings to indicate column names of the GBIF occurrence file.
#' Use 'standard' to select basic columns for use in the package, 'all' to select all available columns.
#' The default is 'standard'
#'
#' @details Select data fields and rename field names prefixed with "Ctrl_"
#'
#' @return
#' data.frame with renamed selected fields with prefix "Ctrl_"
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{select_gbif_fields}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @importFrom readr read_delim
#'
#' @examples
#' \donttest{
#'
#' library(ParsGBIF)
#'
#' help(prepare_gbif_occurrence_data)
#'
#' occ_file <- 'https://raw.githubusercontent.com/pablopains/ParsGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt'
#'
#' occ <- prepare_gbif_occurrence_data(gbif_occurrece_file = occ_file,
#'                                     columns = 'standard')
#'
#' colnames(occ)
#'
#' head(occ)
#' }
#' @export
prepare_gbif_occurrence_data <- function(gbif_occurrece_file = '',
                                         columns = 'standard')
{

  if(gbif_occurrece_file=='')
  {
    stop("Inform the file name!")
  }


  occ <- readr::read_delim(file = gbif_occurrece_file,
                           delim = '\t',
                           locale = readr::locale(encoding = "UTF-8"),
                           show_col_types = FALSE)

  col_sel <- select_gbif_fields(columns = columns)

  occ <- occ[ ,col_sel]

  colnames(occ) <- paste0('Ctrl_',colnames(occ))

  return(occ)
}
