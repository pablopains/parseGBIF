#' @title Extract and Tabulate GBIF Data Quality Issues
#' @name extract_gbif_issue
#'
#' @description
#' Extracts and tabulates GBIF validation rules for occurrence records, creating
#' individual columns for each issue type with TRUE/FALSE flags indicating whether
#' each issue applies to each record.
#'
#' @param occ
#' Data frame. GBIF occurrence table with selected columns as returned by
#' `select_gbif_fields(columns = 'standard')`. Must contain a `Ctrl_issue` column.
#'
#' @param enumOccurrenceIssue
#' Data frame. An enumeration of validation rules for single occurrence records.
#' If `NA` (default), uses the built-in `EnumOccurrenceIssue` dataset.
#'
#' @details
#' GBIF recognizes and documents several issues relating to data fields for individual records.
#' The `Ctrl_issue` field stores terms representing an enumeration of GBIF validation rules.
#' These issues can indicate data quality problems or processing alterations made by GBIF.
#'
#' This function:
#' 1. Uses the `EnumOccurrenceIssue` dataset as a reference for known GBIF issues
#' 2. Creates individual columns for each issue type
#' 3. Flags each record with TRUE/FALSE for each applicable issue
#' 4. Provides a summary of issue frequencies across the dataset
#'
#' Not all issues indicate bad data - some flag that GBIF has altered values during processing.
#'
#' @return
#' A list with two data frames:
#' - `occ_gbif_issue`: Original occurrence data with additional columns for each GBIF issue,
#'   containing TRUE/FALSE values indicating whether the issue applies to each record
#' - `summary`: Summary data frame showing the frequency of each issue across all records,
#'   sorted by most frequent issues first
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`prepare_gbif_occurrence_data()`] for preparing GBIF occurrence data,
#' [`select_gbif_fields()`] for selecting relevant GBIF fields,
#' [`EnumOccurrenceIssue`] for the GBIF issue enumeration dataset
#'
#' @examples
#' \donttest{
#' library(parseGBIF)
#'
#' # Load sample data
#' occ_file <- 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt'
#'
#' occ <- prepare_gbif_occurrence_data(
#'   gbif_occurrece_file = occ_file,
#'   columns = 'standard'
#' )
#'
#' # Extract GBIF issues
#' occ_gbif_issue <- extract_gbif_issue(occ = occ)
#'
#' # View results
#' names(occ_gbif_issue)
#'
#' # Summary of issues
#' head(occ_gbif_issue$summary)
#'
#' # Issue flags for each record
#' colnames(occ_gbif_issue$occ_gbif_issue)
#' head(occ_gbif_issue$occ_gbif_issue)
#' }
#'
#' @importFrom dplyr arrange
#' @importFrom utils data
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
