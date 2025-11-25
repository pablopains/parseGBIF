#' @title Standardize collection codes using Index Herbariorum
#' @name prepare_collectionCode
#'
#' @description Standardizes collection codes by comparing them with known
#' herbarium codes from the Index Herbariorum. This function helps to
#' identify and standardize institution and collection codes in GBIF data.
#'
#' @param occ A data frame containing occurrence records with columns
#' `Ctrl_collectionCode` and `Ctrl_institutionCode`.
#' @param herbarium_data A data frame containing Index Herbariorum data
#' with a column `code` for herbarium acronyms. If not provided, the function
#' will attempt to download it automatically.
#' @param quiet Logical, if TRUE suppresses progress messages
#'
#' @details
#' This function performs several standardization steps:
#' \itemize{
#'   \item{Handles missing values in collection and institution codes}
#'   \item{Selects the most appropriate code when both are present}
#'   \item{Checks codes against the official Index Herbariorum list}
#'   \item{Marks non-standard codes with an asterisk for easy identification}
#' }
#'
#' The function returns the input data frame with an additional column
#' `parseGBIF_collectionCode_standardized` containing the standardized codes.
#'
#' @return
#' The input data frame `occ` with an additional column:
#' \item{parseGBIF_collectionCode_standardized}{Standardized collection codes}
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' \code{\link[parseGBIF]{get_index_herbariorum}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' data(example_occurrences)
#'
#' # Standardize collection codes
#' standardized_data <- prepare_collectionCode(
#'   occ = example_occurrences
#' )
#'
#' # View results
#' head(standardized_data[, c("Ctrl_collectionCode", "Ctrl_institutionCode",
#'                           "parseGBIF_collectionCode_standardized")])
#' }
#'
#' @importFrom dplyr distinct mutate case_when arrange pull
#' @importFrom stats na.omit
#' @export
prepare_collectionCode <- function(occ = NA, herbarium_data = NA, quiet = FALSE)
{

  # Validate input
  if (missing(occ) || !is.data.frame(occ)) {
    stop("Parameter 'occ' must be a data frame")
  }

  if (missing(herbarium_data) || !is.data.frame(herbarium_data)) {
    message("Downloading Index Herbariorum data...")
    herbarium_data <- get_index_herbariorum(quiet = TRUE)
  }

  # Check required columns
  required_cols <- c("Ctrl_collectionCode", "Ctrl_institutionCode")
  if (!all(required_cols %in% colnames(occ))) {
    stop("Input data must contain columns: ",
         paste(required_cols, collapse = ", "))
  }

  if (!"code" %in% colnames(herbarium_data)) {
    stop("Herbarium data must contain 'code' column")
  }

  # Create unique pairs of institution and collection codes
  pares_instituicao_colecao <- occ %>%
    distinct(Ctrl_collectionCode, Ctrl_institutionCode)

  # Initial standardization logic
  pares_instituicao_colecao <- pares_instituicao_colecao %>%
    mutate(
      Ctrl_collectionCode_standardized = case_when(
        is.na(Ctrl_collectionCode) ~ Ctrl_institutionCode,
        is.na(Ctrl_institutionCode) ~ Ctrl_collectionCode,
        nchar(Ctrl_collectionCode) <= nchar(Ctrl_institutionCode) ~ Ctrl_collectionCode,
        TRUE ~ Ctrl_institutionCode
      )
    )

  # Handle numeric codes
  index_numeric_collection <- grepl("^[0-9]+$", pares_instituicao_colecao$Ctrl_collectionCode)
  pares_instituicao_colecao$Ctrl_collectionCode_standardized[index_numeric_collection] <-
    pares_instituicao_colecao$Ctrl_institutionCode[index_numeric_collection]

  index_numeric_institution <- grepl("^[0-9]+$", pares_instituicao_colecao$Ctrl_institutionCode)
  pares_instituicao_colecao$Ctrl_collectionCode_standardized[index_numeric_institution] <-
    pares_instituicao_colecao$Ctrl_collectionCode[index_numeric_institution]

  # Sort for consistency
  pares_instituicao_colecao <- pares_instituicao_colecao %>%
    arrange(Ctrl_collectionCode_standardized, Ctrl_collectionCode, Ctrl_institutionCode)

  # Get unique herbarium acronyms
  herbarium_acronyms <- herbarium_data %>%
    pull(code) %>%
    unique() %>%
    na.omit()

  # Add acronym identification
  pares_instituicao_colecao <- pares_instituicao_colecao %>%
    mutate(
      is_acronym = case_when(
        Ctrl_collectionCode %in% herbarium_acronyms ~ "Ctrl_collectionCode",
        Ctrl_institutionCode %in% herbarium_acronyms ~ "Ctrl_institutionCode",
        TRUE ~ "not_found_herbarium_list"
      ),
      Ctrl_collectionCode_standardized = case_when(
        is_acronym == "Ctrl_collectionCode" ~ Ctrl_collectionCode,
        is_acronym == "Ctrl_institutionCode" ~ Ctrl_institutionCode,
        TRUE ~ Ctrl_collectionCode_standardized
      )
    )

  # Mark non-standard codes
  pares_instituicao_colecao <- pares_instituicao_colecao %>%
    mutate(
      Ctrl_collectionCode_standardized = case_when(
        is_acronym != "not_found_herbarium_list" ~ Ctrl_collectionCode_standardized,
        TRUE ~ paste(Ctrl_collectionCode_standardized, " *")
      )
    )

  # Initialize standardized column
  occ$parseGBIF_collectionCode_standardized <- NA

  # Vectorized processing for better performance
  tot <- nrow(pares_instituicao_colecao)
  ctrl_collection <- pares_instituicao_colecao$Ctrl_collectionCode_standardized
  is_acronym <- pares_instituicao_colecao$is_acronym

  # Pre-process occurrence columns
  occ_collection <- occ$Ctrl_collectionCode
  occ_institution <- occ$Ctrl_institutionCode

  # Main processing loop
  for(i in seq_len(tot)) {
    if(i %% 100 == 0) {
      message(paste0(i, " : ", tot, ' - ', ctrl_collection[i]))
    }

    current_code <- ctrl_collection[i]

    if(is_acronym[i] == "Ctrl_collectionCode") {
      index <- which(occ_collection == current_code)
      if(length(index) > 0) {
        occ$parseGBIF_collectionCode_standardized[index] <- current_code
      }

    } else if(is_acronym[i] == "Ctrl_institutionCode") {
      index <- which(occ_institution == current_code)
      if(length(index) > 0) {
        occ$parseGBIF_collectionCode_standardized[index] <- current_code
      }

    } else {
      index <- which(occ_institution == current_code)
      if(length(index) > 0) {
        occ$parseGBIF_collectionCode_standardized[index] <- paste0(current_code, " *")
      }
    }
  }

  # Provide summary statistics
  standardized_count <- sum(!is.na(occ$parseGBIF_collectionCode_standardized))
  total_count <- nrow(occ)

  message("Standardization completed: ",
          standardized_count, " of ", total_count,
          " records (", round(standardized_count/total_count * 100, 1), "%)")

  return(occ)
}
