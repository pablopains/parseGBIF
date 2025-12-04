#' @title Extracting GBIF issues
#'
#' @name extract_gbif_issue
#'
#' @description Extract GBIF validation rules for occurrence records
#'
#' GBIF recognises and documents several issues relating to the data fields for an individual record.
#' The issue field stores terms that represent an enumeration of GBIF validation rules.
#' Issues can lead to errors or unexpected data. The issues fields are therefore a valuable source of information
#' when assessing the quality of a record. In order to help GBIF and the data publishers improve the data,
#' GBIF flag records with various issues that they have encountered. These issues can be  used as filters applied
#' to occurrence searches. Not all issues indicate bad data, some flagthe fact that GBIF has altered values during
#' processing. The values of EnumOccurrenceIssue will be used by the function extract_gbif_issue as a model to tabulate
#' the GBIF issues of each record, individualizing them, in columns.TRUE or FALSE, flagging whether the issue applies or
#' not for each record.
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
    if (NROW(enumOccurrenceIssue)==1)
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
