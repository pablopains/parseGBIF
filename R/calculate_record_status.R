#' @title Calculate Record Status Metrics for Taxonomic Groups
#' @name calculate_record_status
#'
#' @description
#' Computes identification status metrics and taxonomic representation statistics
#' for genera or families by comparing GBIF occurrence data with WCVP taxonomic data.
#'
#' @param occ
#' Data frame. GBIF occurrence data containing at minimum:
#' - `parseGBIF_dataset_result`: Classification of records ("useable"/"unusable")
#' - `parseGBIF_unidentified_sample`: Logical indicating unidentified specimens
#' - `parseGBIF_sample_taxon_name`: Taxon names for species counting
#' - `genus`/`family`: Taxonomic grouping variables
#'
#' @param wcvp
#' Data frame. World Checklist of Vascular Plants data containing:
#' - `taxon_status`: Taxonomic status ("Accepted"/"Synonym", etc.)
#' - `taxon_rank`: Taxonomic rank ("Species"/"Genus", etc.)
#' - `taxon_name`: Scientific names
#' - `genus`/`family`: Taxonomic grouping variables
#'
#' @param group
#' Character. Taxonomic level for analysis:
#' - `"genus"`: Calculate metrics at genus level
#' - `"family"`: Calculate metrics at family level
#'
#' @return
#' A data frame with the following columns:
#' - `genus`/`family`: Taxonomic group identifier
#' - `total_specimens_useable`: Count of usable (identified) specimens
#' - `total_specimens_unusable`: Count of unusable (unidentified) specimens
#' - `n_species_GBIF`: Number of distinct species in GBIF data
#' - `total_species_accepted_wcvp`: Number of accepted species in WCVP
#' - `percentageIdentified`: Percentage of identified specimens (0-100)
#' - `percentageUnidentified`: Percentage of unidentified specimens (0-100)
#' - `percentage_wcvp_representation_gbif`: Percentage of WCVP species represented in GBIF
#'
#' @details
#' ## Metrics Calculated:
#'
#' ### 1. Specimen Identification Status:
#' - **Usable specimens**: Identified records classified as "useable"
#' - **Unusable specimens**: Unidentified records classified as "unusable"
#'
#' ### 2. Taxonomic Representation:
#' - Compares species diversity in GBIF vs. WCVP
#' - Calculates representation percentage of accepted species
#'
#' ### 3. Identification Rates:
#' - Percentage of identified specimens per taxonomic group
#' - Percentage of unidentified specimens per taxonomic group
#'
#' ## Data Processing:
#' - Converts taxonomic names to uppercase for consistency
#' - Filters WCVP data to accepted species only
#' - Handles missing values with appropriate replacements
#' - Performs case-insensitive matching
#'
#' @note
#' - Requires standardized column names in input data frames
#' - WCVP data should be filtered to relevant taxonomic scope
#' - GBIF data should be pre-processed with `parseGBIF` functions
#' - Returns 0 for percentages when no specimens are available
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @examples
#' \dontrun{
#' # Load required data
#' data(gbif_occurrences)
#' data(wcvp_data)
#'
#' # Calculate metrics at genus level
#' genus_metrics <- calculate_record_status(
#'   occ = gbif_occurrences,
#'   wcvp = wcvp_data,
#'   group = "genus"
#' )
#'
#' # Calculate metrics at family level
#' family_metrics <- calculate_record_status(
#'   occ = gbif_occurrences,
#'   wcvp = wcvp_data,
#'   group = "family"
#' )
#'
#' # View results
#' head(genus_metrics)
#' head(family_metrics)
#' }
#'
#' @importFrom dplyr filter group_by summarise n_distinct arrange rename mutate
#' @importFrom dplyr full_join left_join
#' @importFrom tidyr replace_na
#' @importFrom magrittr %>%
#' @export
calculate_record_status <- function(occ, wcvp, group) {


  # For UNUSABLE specimens (unidentified):
  index_unusable <- occ$parseGBIF_dataset_result == "unusable" &
    occ$parseGBIF_unidentified_sample == TRUE

  # For USABLE specimens (identified):
  index_useable <- occ$parseGBIF_dataset_result == "useable"


  if(group=='genus'){

    occ$genus <- occ$genus %>% toupper()

    total_species_accepted_wcvp <- wcvp %>%
      filter(
        taxon_status == "Accepted",  # Only accepted names
        taxon_rank == "Species"      # Only species-level taxa
      ) %>%
      group_by(genus) %>%
      summarise(
        total_species_accepted_wcvp = n_distinct(taxon_name)
      ) %>%
      arrange(desc(total_species_accepted_wcvp))

    total_species_accepted_wcvp$genus <- total_species_accepted_wcvp$genus %>% toupper()


    # Calculate specimen counts per genus-family combination
    total_specimens_useable <- occ[index_useable, ] %>%
      group_by(genus) %>%
      summarise(
        n_dataset_result = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(n_dataset_result))

    total_specimens_unusable <- occ[index_unusable, ] %>%
      group_by(genus) %>%
      summarise(
        n_dataset_result = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(n_dataset_result))

    total_species_gbif <- occ[index_useable, ] %>%
      group_by(genus) %>%
      summarise(
        n_species_GBIF = n_distinct(parseGBIF_sample_taxon_name),
        .groups = "drop"
      ) %>%
      arrange(desc(n_species_GBIF))

    # Combine all data into final analysis dataframe
    ident_percent <- full_join(
      total_specimens_useable,
      total_specimens_unusable,
      by = c('genus')
    ) %>%
      left_join(
        total_species_gbif,
        by = 'genus') %>%
      left_join(
        total_species_accepted_wcvp,
        by = 'genus'
      ) %>%
      rename(
        total_specimens_useable = n_dataset_result.x,
        total_specimens_unusable = n_dataset_result.y
      ) %>%
      mutate(
        total_specimens_useable = replace_na(total_specimens_useable, 0),
        total_specimens_unusable = replace_na(total_specimens_unusable, 0)
      )


    ident_percent <- ident_percent %>%
      mutate(        # Mantém as métricas de identificação
        percentageIdentified = ifelse(
          (total_specimens_useable + total_specimens_unusable) > 0,
          round(total_specimens_useable / (total_specimens_useable + total_specimens_unusable) * 100, 3),
          0
        ),
        percentageUnidentified = round((total_specimens_unusable / (total_specimens_useable + total_specimens_unusable)) * 100, 2)
      )%>%
      mutate(
        percentage_wcvp_representation_gbif = round(n_species_GBIF/total_species_accepted_wcvp*100,2) ) %>%
      arrange(desc(total_specimens_unusable))

  }


  if(group == 'family'){

    occ$family <- occ$family %>% toupper()

    total_species_accepted_wcvp <- wcvp %>%
      filter(
        taxon_status == "Accepted",  # Only accepted names
        taxon_rank == "Species"      # Only species-level taxa
      ) %>%
      group_by(family) %>%
      summarise(
        total_species_accepted_wcvp = n_distinct(taxon_name)
      ) %>%
      arrange(desc(total_species_accepted_wcvp))

    total_species_accepted_wcvp$family <- total_species_accepted_wcvp$family %>% toupper()


    # Calculate specimen counts per genus-family combination
    total_specimens_useable <- occ[index_useable, ] %>%
      group_by(family) %>%
      summarise(
        n_dataset_result = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(n_dataset_result))

    total_specimens_unusable <- occ[index_unusable, ] %>%
      group_by(family) %>%
      summarise(
        n_dataset_result = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(n_dataset_result))

    total_species_gbif <- occ[index_useable, ] %>%
      group_by(family) %>%
      summarise(
        n_species_GBIF = n_distinct(parseGBIF_sample_taxon_name),
        .groups = "drop"
      ) %>%
      arrange(desc(n_species_GBIF))

    # Combine all data into final analysis dataframe
    ident_percent <- full_join(
      total_specimens_useable,
      total_specimens_unusable,
      by = c('family')
    ) %>%
      left_join(
        total_species_gbif,
        by = 'family') %>%
      left_join(
        total_species_accepted_wcvp,
        by = 'family'
      ) %>%
      rename(
        total_specimens_useable = n_dataset_result.x,
        total_specimens_unusable = n_dataset_result.y
      ) %>%
      mutate(
        total_specimens_useable = replace_na(total_specimens_useable, 0),
        total_specimens_unusable = replace_na(total_specimens_unusable, 0)
      )

    ident_percent <- ident_percent %>%
      mutate(        # Mantém as métricas de identificação
        percentageIdentified = ifelse(
          (total_specimens_useable + total_specimens_unusable) > 0,
          round(total_specimens_useable / (total_specimens_useable + total_specimens_unusable) * 100, 3),
          0
        ),
        percentageUnidentified = round((total_specimens_unusable / (total_specimens_useable + total_specimens_unusable)) * 100, 2)
      )%>%
      mutate(
        percentage_wcvp_representation_gbif = round(n_species_GBIF/total_species_accepted_wcvp*100,2) ) %>%
      arrange(desc(total_specimens_unusable))

  }

  return(ident_percent)
}
