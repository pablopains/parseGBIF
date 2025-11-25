#' @title Load and Process parseGBIF Output Data
#' @name get_parseGBIF_output
#'
#' @description
#' Loads and processes parseGBIF output files, organizing occurrence data into
#' usable, unusable, and duplicate categories based on quality assessment results.
#' Extracts taxonomic information and standardizes data structure for analysis.
#'
#' @param path_file
#' Character. Directory path where the parseGBIF output file is located.
#'
#' @param name_file
#' Character. Name of the parseGBIF output file.
#' Default is "parseGBIF_5_occ_all_data.csv".
#'
#' @param data_sel
#' Character vector. Types of data to include. Options: 'useable', 'unusable', 'duplicate'.
#' Default includes all three categories.
#'
#' @param dwc_col
#' Character vector. Darwin Core columns to include in the output.
#' Default includes core identification and geographic fields.
#'
#' @param parseGBIF_col
#' Character vector. parseGBIF-specific processing columns to include.
#' Default includes quality assessment and taxonomic resolution fields.
#'
#' @return
#' A list with three data frames:
#' - `useable`: Records with taxonomic identification and valid coordinates
#' - `unusable`: Records without identification or invalid coordinates
#' - `duplicate`: Duplicate records of collection events
#'
#' @details
#' ## Processing Steps:
#' 1. Reads UTF-8 encoded parseGBIF output CSV files
#' 2. Selects specified Darwin Core and parseGBIF columns
#' 3. Extracts genus from scientific names based on identification status
#' 4. Extracts family from collection event key
#' 5. Filters records by specified data categories
#' 6. Organizes results into three quality categories
#'
#' ## Column Handling:
#' - Darwin Core fields: Core biodiversity data standards
#' - parseGBIF fields: Quality assessment and processing metadata
#' - Derived fields: Genus and family extracted from existing data
#'
#' @note
#' - Requires parseGBIF output files in CSV format with specific column structure
#' - Uses cross-platform file path handling
#' - Assumes specific column naming conventions from parseGBIF processing
#' - Returns data organized by quality assessment categories for easy analysis
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @examples
#' \donttest{
#' # Load parseGBIF output data
#' occ_data <- get_parseGBIF_output(
#'   path_file = "path/to/parseGBIF/output",
#'   name_file = "parseGBIF_5_occ_all_data.csv"
#' )
#'
#' # Access different data categories
#' usable_records <- occ_data$useable
#' unusable_records <- occ_data$unusable
#' duplicate_records <- occ_data$duplicate
#'
#' # Check record counts by category
#' sapply(occ_data, nrow)
#' }
#'
#' @importFrom readr read_delim locale
#' @importFrom dplyr select mutate filter
#' @importFrom stringr word
#' @importFrom utils message
#' @export
get_parseGBIF_output <- function(path_file,
                                  name_file = "parseGBIF_5_occ_all_data.csv",
                                  data_sel = c('useable', 'unusable', 'duplicate'),
                                  dwc_col = c("Ctrl_gbifID",
                                              #"Ctrl_bibliographicCitation"
                                              #"Ctrl_language"
                                              "Ctrl_institutionCode",
                                              "Ctrl_collectionCode",
                                              # "Ctrl_datasetName"
                                              "Ctrl_basisOfRecord",
                                              "Ctrl_catalogNumber",
                                              "Ctrl_recordNumber",
                                              "Ctrl_recordedBy",
                                              # "Ctrl_occurrenceStatus"
                                              "Ctrl_eventDate",
                                              # "Ctrl_year"
                                              # "Ctrl_month"
                                              # "Ctrl_day"
                                              # "Ctrl_habitat"
                                              # "Ctrl_fieldNotes"
                                              # "Ctrl_eventRemarks"
                                              "Ctrl_countryCode",
                                              "Ctrl_stateProvince",
                                              "Ctrl_municipality",
                                              # "Ctrl_county"
                                              # "Ctrl_locality"
                                              # "Ctrl_level0Name"
                                              # "Ctrl_level1Name"
                                              # "Ctrl_level2Name"
                                              # "Ctrl_level3Name"
                                              "Ctrl_identifiedBy",
                                              "Ctrl_dateIdentified",
                                              "Ctrl_scientificName",
                                              # "Crtl_family"         INCLUIR
                                              "Ctrl_taxonRank",
                                              "Ctrl_decimalLatitude",
                                              "Ctrl_decimalLongitude"
                                              ),

                                  parseGBIF_col = c("Ctrl_nameRecordedBy_Standard",
                                                  "Ctrl_recordNumber_Standard",
                                                  "Ctrl_key_family_recordedBy_recordNumber",
                                                  "Ctrl_geospatial_quality",
                                                  "Ctrl_verbatim_quality",
                                                  "Ctrl_moreInformativeRecord",
                                                  "Ctrl_coordinates_validated_by_gbif_issue",
                                                  "wcvp_searchedName",
                                                  "parseGBIF_digital_voucher",
                                                  "parseGBIF_duplicates",
                                                  "parseGBIF_num_duplicates",
                                                  "parseGBIF_non_groupable_duplicates",
                                                  "parseGBIF_duplicates_grouping_status",
                                                  "parseGBIF_unidentified_sample",
                                                  "parseGBIF_sample_taxon_name",
                                                  "parseGBIF_sample_taxon_name_status",
                                                  "parseGBIF_number_taxon_names",
                                                  "parseGBIF_decimalLongitude",
                                                  "parseGBIF_decimalLatitude",
                                                  "parseGBIF_wcvp_plant_name_id",
                                                  "parseGBIF_wcvp_taxon_rank",
                                                  "parseGBIF_wcvp_taxon_status",
                                                  "parseGBIF_wcvp_family",
                                                  "parseGBIF_wcvp_taxon_name",
                                                  "parseGBIF_wcvp_taxon_authors",
                                                  "parseGBIF_wcvp_reviewed",
                                                  "parseGBIF_dataset_result",
                                                  "parseGBIF_freq_duplicate_or_missing_data",
                                                  "parseGBIF_duplicates_map",
                                                  "parseGBIF_merged_fields",
                                                  "parseGBIF_merged"
                                                  )
                                  )
{

  # Use file.path for cross-platform compatibility
  file_path <- file.path(path_file, name_file)

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  utils::message("Loading file: ", file_path)

  # Read the data
  occ <- readr::read_delim(
    file = file_path,
    delim = ',',
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

  # Select specified columns
  occ <- occ %>%
    dplyr::select(dplyr::all_of(c(dwc_col, parseGBIF_col)))

  # Extract taxonomic information
  occ <- occ %>%
    dplyr::mutate(
      genus = "",
      family = sapply(strsplit(Ctrl_key_family_recordedBy_recordNumber, "_"), function(x) x[1])
    )

  # Extract genus based on identification status
  unidentified_idx <- occ$parseGBIF_unidentified_sample == TRUE
  identified_idx <- occ$parseGBIF_unidentified_sample == FALSE

  occ$genus[unidentified_idx] <- stringr::word(occ$wcvp_searchedName[unidentified_idx])
  occ$genus[identified_idx] <- stringr::word(occ$parseGBIF_sample_taxon_name[identified_idx])

  # Filter by selected data categories
  occ <- occ %>%
    dplyr::filter(parseGBIF_dataset_result %in% data_sel)

  utils::message("Total records loaded: ", nrow(occ))

  # Return organized list
  list(
    useable = occ %>% dplyr::filter(parseGBIF_dataset_result == "useable"),
    unusable = occ %>% dplyr::filter(parseGBIF_dataset_result == "unusable"),
    duplicate = occ %>% dplyr::filter(parseGBIF_dataset_result == "duplicate")
  )
}
# {
#
#   path_file <- paste0(path_file, "\\", name_file)  # GBIF occurrence data
#
#   occ <- {}
#
#   for(i in 1:NROW(path_file)){
#     message(path_file[i])
#     occ_tmp <- readr::read_delim(
#       file = path_file[i],
#       delim = ',',
#       locale = readr::locale(encoding = "UTF-8"),
#       show_col_types = FALSE
#     )
#
#     occ_tmp <- occ_tmp %>% dplyr::select(c(dwc_col,parseGBIF_col))
#
#     occ_tmp <- occ_tmp %>%
#       mutate(
#         genus = "",
#         family = sapply(strsplit(Ctrl_key_family_recordedBy_recordNumber, "_"), function(x) x[1])
#       )
#
#
#     index <- occ_tmp$parseGBIF_unidentified_sample == TRUE
#     occ_tmp$genus[index] <- stringr::word(occ_tmp$wcvp_searchedName[index])
#
#     index <- occ_tmp$parseGBIF_unidentified_sample == FALSE
#     occ_tmp$genus[index] <- stringr::word(occ_tmp$parseGBIF_sample_taxon_name[index])
#
#     occ_tmp <- occ_tmp %>% dplyr::filter(parseGBIF_dataset_result %in% data_sel)
#
#     occ <- rbind(occ,occ_tmp)
#     message(paste0("n.rows:",NROW(occ)))
#   }
#
#   return(list(useable = occ %>% dplyr::filter(parseGBIF_dataset_result == "useable"),
#               unusable = occ %>% dplyr::filter(parseGBIF_dataset_result == "unusable"),
#               duplicate = occ %>% dplyr::filter(parseGBIF_dataset_result == "duplicate")))
#
# }

# occ <- load_parseGBIF_output(path_file = path_root)
# names(occ)
#
# occ <- rbind(occ$useable, occ$unusable)
