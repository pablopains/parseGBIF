#' @title Get parseGBIF Output Data
#' @name get_parseGBIF_output
#'
#' @description
#' Loads and processes parseGBIF output files, filtering and organizing occurrence data
#' into usable, unusable, and duplicate categories.
#'
#' @param path_file
#' Character. Directory path where the parseGBIF output file is located.
#'
#' @param name_file
#' Character. Name of the parseGBIF output file. Default is "parseGBIF_5_occ_all_data.csv".
#'
#' @param data_sel
#' Character vector. Types of data to include: c('useable', 'unusable', 'duplicate').
#' Default includes all three categories.
#'
#' @param dwc_col
#' Character vector. Darwin Core columns to include in the output.
#' Default includes core Darwin Core fields.
#'
#' @param parseGBIF_col
#' Character vector. parseGBIF-specific columns to include in the output.
#' Default includes key parseGBIF processing columns.
#'
#' @return
#' Returns a list with three data frames:
#' - `useable`: Records classified as usable (identified specimens)
#' - `unusable`: Records classified as unusable (unidentified specimens)
#' - `duplicate`: Records classified as duplicates
#'
#' @details
#' This function reads parseGBIF output files, selects specified columns,
#' extracts genus and family information, and organizes the data into
#' three categories based on the parseGBIF_dataset_result field.
#'
#' The function handles:
#' - Reading UTF-8 encoded CSV files
#' - Column selection based on Darwin Core and parseGBIF specifications
#' - Genus extraction from scientific names
#' - Family extraction from the composite key field
#' - Data filtering based on quality assessment results
#'
#' @note
#' - Requires parseGBIF output files in CSV format
#' - Assumes specific column names from parseGBIF processing
#' - Handles large files through iterative reading
#' - Returns data organized by quality assessment categories
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @examples
#' \dontrun{
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
#' # View structure of the data
#' str(occ_data)
#' }
#'
#' @importFrom readr read_delim locale
#' @importFrom dplyr select mutate filter
#' @importFrom stringr word
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

  path_file <- paste0(path_file, "\\", name_file)  # GBIF occurrence data

  occ <- {}

  for(i in 1:NROW(path_file)){
    message(path_file[i])
    occ_tmp <- readr::read_delim(
      file = path_file[i],
      delim = ',',
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )

    occ_tmp <- occ_tmp %>% dplyr::select(c(dwc_col,parseGBIF_col))

    occ_tmp <- occ_tmp %>%
      mutate(
        genus = "",
        family = sapply(strsplit(Ctrl_key_family_recordedBy_recordNumber, "_"), function(x) x[1])
      )


    index <- occ_tmp$parseGBIF_unidentified_sample == TRUE
    occ_tmp$genus[index] <- stringr::word(occ_tmp$wcvp_searchedName[index])

    index <- occ_tmp$parseGBIF_unidentified_sample == FALSE
    occ_tmp$genus[index] <- stringr::word(occ_tmp$parseGBIF_sample_taxon_name[index])

    occ_tmp <- occ_tmp %>% dplyr::filter(parseGBIF_dataset_result %in% data_sel)

    occ <- rbind(occ,occ_tmp)
    message(paste0("n.rows:",NROW(occ)))
  }

  return(list(useable = occ %>% dplyr::filter(parseGBIF_dataset_result == "useable"),
              unusable = occ %>% dplyr::filter(parseGBIF_dataset_result == "unusable"),
              duplicate = occ %>% dplyr::filter(parseGBIF_dataset_result == "duplicate")))

}

# occ <- load_parseGBIF_output(path_file = path_root)
# names(occ)
#
# occ <- rbind(occ$useable, occ$unusable)
