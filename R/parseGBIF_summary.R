#' @title Selecting the master digital voucher
#' @name parseGBIF_summary
#'
#' @description ...
#'
#' @param parseGBIF_all_data ...
#' @param file.parseGBIF_all_data ...
#' @param fields_to_merge fields to merge
#' @param fields_to_compare fields to compare content frequency
#' @param fields_to_parse all fields
#' @param silence if TRUE does not display progress messages
#'
#' @details ...
#'
#' @return
#' * __parseGBIF_general_summary__
#' * __parseGBIF_merge_fields_summary__
#' * __parseGBIF_merge_fields_summary_useable_data__
#' * __parseGBIF_merge_fields_summary_unusable_data__
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @examples
#' \donttest{
#'
#'
#' results <- export_data_v2.1(occ_digital_voucher_file = '',
#' occ_digital_voucher = occ_digital$all_data,
#' merge_unusable_data = TRUE,
#' silence = FALSE)
#'
#' names(results)
#'
#' head(results$occ_all)
#' colnames(results$occ_all)
#'
#' }
#' @export
parseGBIF_summary <- function(parseGBIF_all_data = NA,
                              file.parseGBIF_all_data = '',
                              fields_to_merge = c('Ctrl_fieldNotes',
                                                  'Ctrl_year',
                                                  'Ctrl_stateProvince',
                                                  'Ctrl_municipality',
                                                  'Ctrl_locality',
                                                  'Ctrl_countryCode',
                                                  'Ctrl_eventDate',
                                                  'Ctrl_habitat',
                                                  'Ctrl_level0Name',
                                                  'Ctrl_level1Name',
                                                  'Ctrl_level2Name',
                                                  'Ctrl_level3Name'))
{

  if(is.na(file.parseGBIF_all_data)){file.parseGBIF_all_data=''}

  if(file.parseGBIF_all_data!='')
  {
    # file.parseGBIF_all_data <- paste0(path_data,'\\parseGBIF_all_data\\','parseGBIF_all_data.csv')
    occ_tmp <- readr::read_delim(file = file.parseGBIF_all_data,
                                             delim = ',',
                                             locale = readr::locale(encoding = "UTF-8"),
                                             show_col_types = FALSE) %>% data.frame()
  }else
  {
    occ_tmp <- parseGBIF_all_data
  }

  # summary
  {

    {
      parseGBIF_general_summary <- data.frame(question = '',
                                              value = '0',
                                              condition = '')[-1,]

      parseGBIF_merge_fields_summary_complete <-
        parseGBIF_merge_fields_summary_incomplete <-
        parseGBIF_merge_fields_summary <-
        parseGBIF_general_summary

      add_summary <- function(question = '',
                              value = '0',
                              condition = '',
                              data = NA)
      {
        data <- data %>%
          dplyr::add_row(question = question,
                         value = as.character(value),
                         condition = as.character(condition))
        return(data)
      }

      freq_merged_fields <- function(fields=NA,
                                     occ_tmp=NA)
      {

        freq_fields <- data.frame(id='',val=0)[-1,]
        freq_fields <- freq_fields %>%
          dplyr::add_row(id=fields,
                         val=rep(0,NROW(fields)))

        for(i in 1:NROW(occ_tmp))
        {
          if(occ_tmp$parseGBIF_merged[i] == FALSE)
          {
            next
          }

          x <- jsonlite::fromJSON(occ_tmp$parseGBIF_merged_fields[i])

          ic=1

          for(ic in 1:NROW(fields_to_merge))
          {
            if(fields_to_merge[ic] %in% names(x))
            {
              freq_fields[ic,2]  <- freq_fields[ic,2]+1
            }
          }
        }

        freq_fields <- freq_fields %>%
          dplyr::arrange(desc(val))
        return(freq_fields)
      }

    }

    question <- 'total number of records'
    value <- NROW(occ_tmp)
    condition <- 'all lines'
    parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    question <- 'total number of unique collection events'
    ind <- occ_tmp$parseGBIF_digital_voucher==TRUE
    value <- sum(ind)
    condition <- "where parseGBIF_digital_voucher = TRUE"
    parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    question <- 'total number of duplicates records of unique collection events'
    ind <- occ_tmp$parseGBIF_dataset_result=='duplicate'
    value <- sum(ind)
    condition <- "where parseGBIF_dataset_result = 'duplicate'"
    parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    {
      question <- 'total number of useable records'
      ind <- occ_tmp$parseGBIF_dataset_result=='useable'
      value <- sum(ind)
      condition <- "where parseGBIF_dataset_result = 'useable'"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary


      question <- 'total number of useable records / consensus on identification'
      ind <- occ_tmp$parseGBIF_dataset_result=='useable' & occ_tmp$parseGBIF_sample_taxon_name_status=='identified'
      value <- sum(ind)
      condition <- "where parseGBIF_dataset_result = 'useable' AND parseGBIF_sample_taxon_name_status = 'identified' "
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary


      question <- 'total number of useable records / divergent identifications'
      ind <- occ_tmp$parseGBIF_dataset_result=='useable' & occ_tmp$parseGBIF_sample_taxon_name_status == 'divergent identifications'
      value <- sum(ind)
      condition <- "where parseGBIF_dataset_result = 'useable' AND parseGBIF_sample_taxon_name_status = 'divergent identifications'"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary
    }

    {
      question <- 'total number of unusable records'
      ind <- sum(occ_tmp$parseGBIF_dataset_result=='unusable')
      value <- sum(ind)
      condition <- "where parseGBIF_dataset_result = 'unusable'"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

      question <- 'total number of unusable records / unidentified'
      ind <- occ_tmp$parseGBIF_dataset_result=='unusable'
      value <- sum(ind)
      condition <- "where parseGBIF_dataset_result = 'unusable' AND parseGBIF_unidentified_sample = TRUE"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary


      question <- 'total number of unusable records / not suitable for geospatial analysis'
      ind <- occ_tmp$parseGBIF_dataset_result=='unusable' & occ_tmp$parseGBIF_useful_for_spatial_analysis == FALSE
      value <- sum(ind)
      condition <- "where parseGBIF_dataset_result = 'unusable' AND parseGBIF_useful_for_spatial_analysis = FALSE"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    }

    question <- paste0('total unique collection events containing merged fields',
                       ifelse(any(colnames(occ_tmp) %in% 'merge_unusable_data'),'',' (unusable data not included)'))
    ind <- occ_tmp$parseGBIF_merged==TRUE
    value <- sum(ind)
    condition <- "where parseGBIF_merged = TRUE"
    parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    parseGBIF_general_summary <- add_summary('----------', '----------', '----------',parseGBIF_general_summary); parseGBIF_general_summary

    {
    question <- 'Taxonomic diversity, based on GBIF taxonomy, from scientificName'
    ind <-  unique(occ_tmp$Ctrl_scientificName[occ_tmp$Ctrl_taxonRank %in%  c('SPECIES', 'SUBSPECIES', 'VARIETY')])
    value <- NROW(ind)
    condition <- "count scientificName where Ctrl_taxonRank = 'SPECIES' OR 'SUBSPECIES' OR 'VARIETY' "
    parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    question <- 'Taxonomic diversity, based on GBIF taxonomy, from scientificName / suitable for geospatial analysis'
    ind <-  unique(occ_tmp$Ctrl_scientificName[occ_tmp$Ctrl_taxonRank %in% c('SPECIES', 'SUBSPECIES', 'VARIETY') &
                                                 occ_tmp$parseGBIF_useful_for_spatial_analysis==TRUE])
    value <- NROW(ind)
    condition <- "count scientificName where (Ctrl_taxonRank = 'SPECIES' OR 'SUBSPECIES' OR 'VARIETY') AND (parseGBIF_useful_for_spatial_analysis = TRUE)"
    parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary
    }

    {
      question <- 'Taxonomic diversity, based on GBIF taxonomy, from standardized scientificName'
      ind <-  unique(occ_tmp$wcvp_searchedName)
      value <- NROW(ind)
      condition <- "count wcvp_searchedName "
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

      question <- 'Taxonomic diversity, based on GBIF taxonomy, from standardized scientificName / suitable for geospatial analysis'
      ind <-  unique(occ_tmp$wcvp_searchedName[occ_tmp$parseGBIF_useful_for_spatial_analysis==TRUE])
      value <- NROW(ind)
      condition <- "count wcvp_searchedName where (parseGBIF_useful_for_spatial_analysis = TRUE)"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    }

    {
      question <- 'Taxonomic diversity, based on WCVP taxonomy'
      ind <-  unique(occ_tmp$wcvp_taxon_name)
      value <- NROW(ind)
      condition <- "count wcvp_taxon_name "
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

      question <- 'Taxonomic diversity, based on WCVP taxonomy / suitable for geospatial analysis'
      ind <-  unique(occ_tmp$wcvp_taxon_name[occ_tmp$parseGBIF_useful_for_spatial_analysis==TRUE])
      value <- NROW(ind)
      condition <- "count wcvp_taxon_name where (parseGBIF_useful_for_spatial_analysis = TRUE)"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    }

    {
      question <- 'Taxonomic diversity, based on data cleaned in parseGBIF workflow'
      ind <-  unique(occ_tmp$parseGBIF_sample_taxon_name)
      value <- NROW(ind)
      condition <- "count parseGBIF_sample_taxon_name "
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

      question <- 'Taxonomic diversity, based on data cleaned in parseGBIF workflow / suitable for geospatial analysis'
      ind <-  unique(occ_tmp$parseGBIF_sample_taxon_name[occ_tmp$parseGBIF_useful_for_spatial_analysis==TRUE])
      value <- NROW(ind)
      condition <- "count parseGBIF_sample_taxon_name where (parseGBIF_useful_for_spatial_analysis = TRUE)"
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary

    }

    parseGBIF_general_summary <- add_summary('----------', '----------', '----------',parseGBIF_general_summary); parseGBIF_general_summary

    # 3. Data quality map based on GBIF issues and information quality
    freq_data <- table(freq_tmp <- occ_tmp$Ctrl_geospatial_quality,
                           exclude = NA) %>%
      data.frame() %>%
      dplyr::arrange(desc(Freq))
    condition <- 'frequency of selection_score'
    for (i in 1:NROW(freq_data)) {
      question <- paste0('Data quality map based on frequency of Impact of the issue for the use of geospatial information (',freq_data$Var1[i],')')
      value <- freq_data$Freq[i]
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary
    }
    parseGBIF_general_summary

    parseGBIF_general_summary <- add_summary('----------', '----------', '----------',parseGBIF_general_summary); parseGBIF_general_summary

    # 3. Data quality map based on GBIF issues and information quality
    freq_data <- table(freq_tmp <- occ_tmp$Ctrl_verbatim_quality,
                       exclude = NA) %>%
      data.frame() %>%
      dplyr::arrange(desc(Freq))
    condition <- 'frequency of Ctrl_verbatim_quality'

    for (i in 1:NROW(freq_data)) {
      question <- paste0('Data quality map based on record completeness (',freq_data$Var1[i],')')
      value <- freq_data$Freq[i]
      parseGBIF_general_summary <- add_summary(question, value, condition, parseGBIF_general_summary); parseGBIF_general_summary
    }
    parseGBIF_general_summary

    if (any(occ_tmp$parseGBIF_merged_fields != ''))
    {

    x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp)
    x_freq_merged_fields$id <- paste0(x_freq_merged_fields$id,' : total merge actions')
    parseGBIF_merge_fields_summary <- add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, condition, parseGBIF_merge_fields_summary)
    parseGBIF_merge_fields_summary


    x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='useable'))
    x_freq_merged_fields$id <- paste0(x_freq_merged_fields$id,' : merge actions ')
    parseGBIF_merge_fields_summary_complete <- add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, condition, parseGBIF_merge_fields_summary_complete)
    parseGBIF_merge_fields_summary_complete
    }

    if(NROW(occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='unusable'))>0 )
    {

      x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='unusable'))
      x_freq_merged_fields$id <- paste0(x_freq_merged_fields$id,' : merge actions ')
      parseGBIF_merge_fields_summary_incomplete <- add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, condition, parseGBIF_merge_fields_summary_incomplete)
      parseGBIF_merge_fields_summary_incomplete
    }

  }

  return(list(parseGBIF_general_summary = parseGBIF_general_summary,
              parseGBIF_merge_fields_summary = parseGBIF_merge_fields_summary,
              parseGBIF_merge_fields_summary_useable_data = parseGBIF_merge_fields_summary_complete,
              parseGBIF_merge_fields_summary_unusable_data = parseGBIF_merge_fields_summary_incomplete))

}

