#' @title Extract GBIF issue occurrence records
#'
#' @name extract_gbif_issue 
#'
#' @description Extract GBIF validation rules for occurrence records
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param enumOccurrenceIssue An enumeration of validation rules for single occurrence records by GBIF file, if NA, will be used, data(EnumOccurrenceIssue)
#'
#' @details https://gbif.github.io/parsers/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
#'
#' @return list with two data frames: summary, with the frequency of issues in the records
#' and occ_gbif_issue, with issues in columns with TRUE or FALSE for each record.
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{prepare_gbif_occurrence_data}}, \code{\link[ParsGBIF]{select_gbif_fields}}
#'
#' @examples
#' \donttest{
#'
#' library(ParsGBIF)
#'
#' help(extract_gbif_issue)
#'
#' occ_file <- 'https://raw.githubusercontent.com/pablopains/ParsGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt'
#'
#' occ <- prepare_gbif_occurrence_data(gbif_occurrece_file = occ_file,
#'                                     columns = 'standard')
#'
#' data(EnumOccurrenceIssue)
#'
#' colnames(EnumOccurrenceIssue)
#'
#' head(EnumOccurrenceIssue)
#'
#' occ_gbif_issue <- extract_gbif_issue(occ = occ)
#'
#' names(occ_gbif_issue)
#'
#' head(occ_gbif_issue$summary)
#'
#' colnames(occ_gbif_issue$occ_gbif_issue)
#'
#' head(occ_gbif_issue$occ_gbif_issue)
#' }
#'
#' @import dplyr
#'
#' @export
extract_gbif_issue <- function(occ = NA,
                               enumOccurrenceIssue = NA)
{
  {
    if (is.na(enumOccurrenceIssue))
    {
      data(EnumOccurrenceIssue)
    }else
    {
      EnumOccurrenceIssue <- enumOccurrenceIssue
    }

    issue_table <- data.frame(t(EnumOccurrenceIssue$constant))
    colnames(issue_table) <- EnumOccurrenceIssue$constant

    issue_key <- colnames(issue_table)
    issue_table[1:NROW(occ),issue_key] <- rep(FALSE, NROW(occ))
  }


  ic <- 1
  for(ic in 1:length(issue_key))
  {
    x_issue <- grepl(issue_key[ic], occ$Ctrl_issue)
    issue_table[,ic] <- x_issue
  }

  issue_result <- data.frame(issue = issue_key,
                             n_occ = rep(0,length(issue_key)))

  i=1
  for(i in 1:length(issue_key))
  {
    n_occ <- issue_table[,issue_key[i]] %>% sum()
    issue_result$n_occ[i] <- issue_table[,issue_key[i]] %>% sum()
  }

  issue_result <- issue_result %>%
    dplyr::arrange(desc(n_occ))

  return(list(occ_gbif_issue=issue_table,
              summary=issue_result))

}
