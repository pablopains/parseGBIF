#' @title GBIF Occurrence Issue Enumeration
#' @name EnumOccurrenceIssue
#'
#' @description
#' An enumeration of validation rules for single occurrence records from GBIF.
#'
#' There are many things that can go wrong with occurrence data and we continously
#' encounter unexpected data. In order to help us and publishers improve the data,
#' we flag records with various issues that we have encountered. This is also very
#' useful for data consumers as you can include these issues as filters in occurrence
#' searches. Not all issues indicate bad data. Some are merely flagging the fact
#' that GBIF has altered values during processing. On the details page of any
#' occurrence record you will see the list of issues in the notice at the bottom.
#'
#' @format
#' A data frame with 69 rows and 9 columns:
#' \describe{
#'   \item{constant}{Character. GBIF issue constant code (e.g., "ZERO_COORDINATE", "COORDINATE_INVALID")}
#'   \item{description}{Character. GBIF issue description}
#'   \item{definition}{Character. Our definition for classifying geographic issues}
#'   \item{type}{Character. Type of issue (e.g., "Geographic", "Taxonomic", "Temporal")}
#'   \item{priority}{Character. Impact of the issue for the use of geospatial information}
#'   \item{score}{Numeric. Impact score of the issue for geospatial information use}
#'   \item{selection_score}{Numeric. Value used to calculate geospatial information quality}
#'   \item{reasoning}{Character. Reasoning behind the impact assessment}
#'   \item{notes}{Character. Additional notes and observations}
#' }
#'
#' @details
#' ## Usage:
#' This dataset is used internally by `parseGBIF` functions to classify and score
#' data quality issues in GBIF occurrence records. It provides a standardized way
#' to evaluate the reliability of geospatial and taxonomic information.
#'
#' ## Issue Categories:
#' - **Geographic issues**: Coordinate problems, country mismatches, etc.
#' - **Taxonomic issues**: Name parsing, classification problems
#' - **Temporal issues**: Date formatting, future dates, etc.
#' - **Data quality**: Completeness, consistency, validity checks
#'
#' @docType data
#' @keywords datasets
#'
#' @usage
#' data(EnumOccurrenceIssue)
#'
#' @examples
#' \dontrun{
#' # Load the GBIF issue enumeration data
#' data(EnumOccurrenceIssue)
#'
#' # View the first few issues
#' head(EnumOccurrenceIssue)
#'
#' # Filter geographic issues only
#' geographic_issues <- EnumOccurrenceIssue[EnumOccurrenceIssue$type == "Geographic", ]
#'
#' # Get high priority issues (score > 7)
#' high_priority <- EnumOccurrenceIssue[EnumOccurrenceIssue$score > 7, ]
#' }
#'
#' @source
#' * [GBIF Infrastructure: Data processing](https://www.gbif.org/article/5i3CQEZ6DuWiycgMaaakCo/gbif-infrastructure-data-processing)
#' * [An enumeration of validation rules for single occurrence records](https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html)
#' * GBIF Occurrence Issue API documentation
