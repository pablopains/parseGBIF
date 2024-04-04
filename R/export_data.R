#' @title Export of results
#' @name export_data
#'
#' @description For each unique collection event key, complete or incomplete,
#' outputs will be created which combine information from duplicate records and generate a
#' single unique collection event record to replace them.
#' The main output fields relating to taxonomic identification and geographic coordinates:
#' * parseGBIF_sample_taxon_name = scientific name chosen as taxonomic identification for unique collection event
#' * parseGBIF_number_taxon_names = number of scientific names found in duplicates of unique collection event
#' * parseGBIF_sample_taxon_name_status = status of choice of 'identified', 'divergent identifications', 'unidentified'
#' * parseGBIF_unidentified_sample = if unique collection event has taxonomic identification
#' * parseGBIF_decimalLatitude = latitude in decimal degrees
#' * parseGBIF_decimalLongitude = longitude in decimal degrees
#' * parseGBIF_useful_for_spatial_analysis = whether the coordinates are useful for spatial analysis.
#' __How is the taxon binomial attributed to the unique collection event selected?__
#' 1) Where the unique collection event key is complete:
#' The accepted TAXON_NAME selected is that which is most frequently applied to the duplicate vouchers at or below the rank of species.
#' Where two named are applied with equal frequency then a mechanical approach, using alphabetical order, is applied, the first listed TAXON_NAME being chosen.
#' Where there is no identification, at or below the rank of species, then the unique collection event, the unique collection event is indicated as unidentified.
#' 2) Where the unique collection event key is incomplete:
#' Where the unique collection event key is incomplete, then each record is treated as a unique collection event. If there is no identification, at or below the rank of species, then the unique collection event is classified as unidentified.
#' __Geospatial information __
#' If the master voucher does not have geographic coordinates, we will seek coordinates from the duplicate records associated with it.
#' Finally, the records are separated into three sets of data:
#' __useable_data__ - Where unique collection event with taxonomic identification and geographic coordinates are complete. This represents the useable dataset.
#' __unusable_data__  - Where unique collection event without taxonomic identification and/or geographic coordinates.
#' __duplicates__ The duplicates of unique collection events complete / incomplete
#'
#' With this, it is possible to perform:
#' Merge information between fields of duplicates of a unique collection event to create a synthetic record for each unique collection event,
#' Compare the frequency of content in fields
#' Generate a work package summary
#'
#' For each complete unique collection event key, data fields that are empty in the digital voucher record will be populated with data from the respective duplicates.
#' During content merging, we indicate fields associated with the description, location, and data of the unique collection event.
#' By default, fields_to_merge parameter of export_data function contains:
#' * Ctrl_fieldNotes
#' * Ctrl_year
#' * Ctrl_stateProvince
#' * Ctrl_municipality
#' * Ctrl_locality
#' * Ctrl_countryCode
#' * Ctrl_eventDate
#' * Ctrl_habitat
#' * Ctrl_level0Name
#' * Ctrl_level1Name
#' * Ctrl_level2Name
#' * Ctrl_level3Name
#'
#' @param occ_digital_voucher_file CSV fila result of function select_digital_voucher()$occ_digital_voucher
#' @param occ_digital_voucher data frame result of function select_digital_voucher()$occ_digital_voucher
#' @param merge_unusable_data include records unique collection events incomplete in merge processing
#' @param fields_to_merge fields to merge
#' @param fields_to_compare fields to compare content frequency
#' @param fields_to_parse all fields
#' @param silence if TRUE does not display progress messages
#'
#' @details Each data frame should be used as needed
#'
#' @return list with 10 data frames
#' * __all_data__ All records processed, merged Unique collection events complete / incomplete and their duplicates
#' * __useable_data_merge__ Merged Unique collection events complete
#' * __useable_data_raw__ Raw Unique collection events complete
#' * __duplicates__ Duplicates of unique collection events complete / incomplete
#' * __unusable_data_merge__ Merged Unique collection events incomplete,
#' It is NA if merge_unusable_data is FALSE.
#' * __unusable_data_raw__ Raw Unique collection events incomplete
#' * __parseGBIF_general_summary__
#' * __parseGBIF_merge_fields_summary__
#' * __parseGBIF_merge_fields_summary_useable_data__
#' * __parseGBIF_merge_fields_summary_unusable_data__ It is NA if merge_unusable_data is FALSE
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @import dplyr
#' @import stringr
#'
#' @examples
#' \donttest{
#' help(export_data)
#'
#' results <- export_data(occ_digital_voucher_file = file.occ_digital_voucher,
#'                        merge_unusable_data = TRUE)
#'
#' names(results)
#'
#' results$parseGBIF_general_summary
#' results$parseGBIF_merge_fields_summary
#' results$parseGBIF_merge_fields_summary_complete

#' NROW(results$all_data)
#' NROW(results$unique_collection_event_complete_merge)
#' NROW(results$unique_collection_event_incomplete_raw)
#' NROW(results$duplicates)
#'
#' }
#' @export
export_data <- function(occ_digital_voucher_file = '',
                             occ_digital_voucher = NA,
                             merge_unusable_data = FALSE,
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
                                                 'Ctrl_level3Name'),

                             fields_to_compare = c('Ctrl_gbifID',
                                                   'Ctrl_scientificName',
                                                   'Ctrl_recordedBy',
                                                   'Ctrl_recordNumber',
                                                   'Ctrl_identifiedBy',
                                                   'Ctrl_dateIdentified',
                                                   'Ctrl_institutionCode',
                                                   'Ctrl_collectionCode',
                                                   'Ctrl_datasetName',
                                                   'Ctrl_datasetName',
                                                   'Ctrl_language',
                                                   "wcvp_plant_name_id",
                                                   "wcvp_taxon_rank",
                                                   "wcvp_taxon_status",
                                                   "wcvp_family",
                                                   "wcvp_taxon_name",
                                                   "wcvp_taxon_authors",
                                                   "wcvp_reviewed",
                                                   "wcvp_searchNotes"),

                             fields_to_parse = c('Ctrl_gbifID',
                                                 'Ctrl_bibliographicCitation',
                                                 'Ctrl_language',
                                                 'Ctrl_institutionCode',
                                                 'Ctrl_collectionCode',
                                                 'Ctrl_datasetName',
                                                 'Ctrl_basisOfRecord',
                                                 'Ctrl_catalogNumber',
                                                 'Ctrl_recordNumber',
                                                 'Ctrl_recordedBy',
                                                 'Ctrl_occurrenceStatus',
                                                 'Ctrl_eventDate',
                                                 'Ctrl_year',
                                                 'Ctrl_month',
                                                 'Ctrl_day',
                                                 'Ctrl_habitat',
                                                 'Ctrl_fieldNotes',
                                                 'Ctrl_eventRemarks',
                                                 'Ctrl_countryCode',
                                                 'Ctrl_stateProvince',
                                                 'Ctrl_municipality',
                                                 'Ctrl_county',
                                                 'Ctrl_locality',
                                                 'Ctrl_issue',

                                                 'Ctrl_level0Name',
                                                 'Ctrl_level1Name',
                                                 'Ctrl_level2Name',
                                                 'Ctrl_level3Name',

                                                 'Ctrl_identifiedBy',
                                                 'Ctrl_dateIdentified',
                                                 'Ctrl_scientificName',
                                                 'Ctrl_taxonRank',
                                                 'Ctrl_decimalLatitude',
                                                 'Ctrl_decimalLongitude',

                                                 'Ctrl_nameRecordedBy_Standard',
                                                 'Ctrl_recordNumber_Standard',
                                                 'Ctrl_key_family_recordedBy_recordNumber',
                                                 'Ctrl_geospatial_quality',
                                                 'Ctrl_verbatim_quality',
                                                 'Ctrl_moreInformativeRecord',
                                                 'Ctrl_coordinates_validated_by_gbif_issue',

                                                 "wcvp_plant_name_id",
                                                 "wcvp_taxon_rank",
                                                 "wcvp_taxon_status",
                                                 "wcvp_family",
                                                 "wcvp_taxon_name",
                                                 "wcvp_taxon_authors",
                                                 "wcvp_reviewed",
                                                 "wcvp_searchedName",
                                                 "wcvp_searchNotes",

                                                 'parseGBIF_digital_voucher',
                                                 'parseGBIF_duplicates',
                                                 'parseGBIF_num_duplicates',
                                                 'parseGBIF_non_groupable_duplicates',
                                                 'parseGBIF_duplicates_grouping_status',

                                                 'parseGBIF_unidentified_sample',
                                                 'parseGBIF_sample_taxon_name',
                                                 'parseGBIF_sample_taxon_name_status',
                                                 'parseGBIF_number_taxon_names',
                                                 'parseGBIF_useful_for_spatial_analysis',
                                                 'parseGBIF_decimalLatitude',
                                                 'parseGBIF_decimalLongitude',

                                                 'parseGBIF_wcvp_plant_name_id',
                                                 'parseGBIF_wcvp_taxon_rank',
                                                 'parseGBIF_wcvp_taxon_status',
                                                 'parseGBIF_wcvp_family',
                                                 'parseGBIF_wcvp_taxon_name',
                                                 'parseGBIF_wcvp_taxon_authors',
                                                 # parseGBIF_wcvp_accepted_plant_name_id,
                                                 'parseGBIF_wcvp_reviewed',

                                                 'parseGBIF_dataset_result'),
                             silence=TRUE
)
{
  if(! silence == TRUE)
  {
    print('Loading occurrence file...')
  }

  if(is.na(occ_digital_voucher_file))
  {
    occ_digital_voucher_file <- ''
  }

  if(occ_digital_voucher_file !=''  )
  {
    if(!file.exists(occ_digital_voucher_file))
    {
      stop("Invalid occurrence file!")
    }

    occ_tmp <- readr::read_csv(occ_digital_voucher_file,
                               locale = readr::locale(encoding = "UTF-8"),
                               show_col_types = FALSE) %>% as.data.frame()
  }else
  {

    if (NROW(occ_digital_voucher)==0)
    {
      stop("Empty occurrence data frame!")
    }

    occ_tmp <- occ_digital_voucher %>% as.data.frame()
    rm(occ_digital_voucher)
  }


  # preparacao dos dados
  {
    occ_tmp <- occ_tmp %>%
      dplyr::select(all_of(fields_to_parse))

    occ_tmp <- occ_tmp %>%
      dplyr::mutate(
        # parseGBIF_unidentified_sample = TRUE,
        # parseGBIF_sample_taxon_name = '',
        # parseGBIF_sample_taxon_name_status = '',
        # parseGBIF_number_taxon_names = 0,
        # parseGBIF_useful_for_spatial_analysis = FALSE,
        # parseGBIF_decimalLatitude = NA,
        # parseGBIF_decimalLongitude = NA,
        # # parseGBIF_notes = '',
        # # parseGBIF_status = FALSE,
        parseGBIF_freq_duplicate_or_missing_data = '',
        parseGBIF_duplicates_map = '',
        parseGBIF_merged_fields = '',
        parseGBIF_merged = FALSE,

        after = 'parseGBIF_dataset_result')

    occ_tmp <- occ_tmp %>%
      dplyr::arrange(Ctrl_key_family_recordedBy_recordNumber)
  }

  if(! silence == TRUE)
  {
    print('Selecting...')
  }


  # in dup out_to_recover
  {
    # NROW(occ_tmp)
    # occ_tmp <- occ_tmp %>%
    #   dplyr::mutate(parseGBIF_duplicates_map = rep('',NROW(occ_tmp)),
    #                 parseGBIF_merged_fields = rep('',NROW(occ_tmp)),
    #                 parseGBIF_merged = rep(FALSE,NROW(occ_tmp)))

    occ_in <- occ_tmp %>%
      dplyr::filter(parseGBIF_dataset_result == 'useable')

    occ_dup <- occ_tmp %>%
      dplyr::filter(parseGBIF_dataset_result == 'duplicate')

    occ_out_to_recover <- occ_tmp %>%
      dplyr::filter(parseGBIF_dataset_result == 'unusable')

    # remove(occ_tmp)

  }

  if(! silence == TRUE)
  {
    print('Merging...')
  }

  # merge and compare
  {

    # fields_to_merge
    # fields_to_compare

    if(merge_unusable_data==TRUE)
    {
      occ_res_full <-  rbind(occ_in,occ_out_to_recover)
      # occ_res_full <-  occ_res_full %>%
      #   dplyr::mutate(parseGBIF_duplicates_map = rep('',NROW(occ_res_full)),
      #                 parseGBIF_merged_fields = rep('',NROW(occ_res_full)),
      #                 parseGBIF_merged = rep(FALSE,NROW(occ_res_full)))
    }else
    {
      occ_res_full <-  occ_in
    }


    key <- occ_res_full$Ctrl_key_family_recordedBy_recordNumber %>%
      unique()

    # key <- occ_tmp$Ctrl_key_family_recordedBy_recordNumber[occ_tmp$parseGBIF_non_groupable_duplicates==TRUE] %>% unique()

    tot <- NROW(key)
    fields_to_all <- c(fields_to_compare,fields_to_merge)

    # i = 52
    # i=1
    #
    # i = 41737
    #
    # # "41737 - 178212 - MYRTACEAE_EITEN_10658"
    # # Error in grepl(data_col_dup_ix, x_jonsom) :
    # #   invalid regular expression, reason 'Out of memory'
    # #
    #
    # # 105805 - 178212 - MYRTACEAE_REGNELL_116"
    #
    # i = 105805
    #
    # i = 26031
    i=1
    # key[1] <- 'RUBIACEAE_HOPKINS_56'

    for(i in 1:tot)
    {

      index <- occ_res_full$Ctrl_key_family_recordedBy_recordNumber %in% key[i]

      # "RUBIACEAE_HOPKINS_56"
      # index <- occ_res_full$Ctrl_key_family_recordedBy_recordNumber %in% "RUBIACEAE_HOPKINS_56"


      if(occ_res_full$parseGBIF_duplicates[index==TRUE][1]==TRUE)
      {
        if(! silence == TRUE)
        {
          print(paste0(i, ' - ', tot, ' - ', key[i]))
        }

        x_Ctrl_gbifID <- occ_res_full[index==TRUE, 'Ctrl_gbifID']

        index_dup <- occ_dup$Ctrl_key_family_recordedBy_recordNumber %in% key[i]


        if(sum(index_dup)==0)
        {next}

        ic <- 1

        x_jonsom_full <- ""
        parseGBIF_merged_fields <- ""

        freq_data_col_full <- ""

        for ( ic in 1:length(fields_to_all))
        {
          # print(ic)

          data_col <- occ_res_full[index==TRUE,
                                   fields_to_all[ic]]

          if(is.na(data_col))
          {
            data_col <- ""
          }

          data_col <- gsub('\\{|\\}|\\[|\\]|\\(|\\)|\\\\|\\*', '', data_col)


          x_data_col <- toupper(data_col)

          data_col_dup <- occ_dup[index_dup==TRUE,
                                  fields_to_all[ic]]

          freq_data_col <- table(freq_tmp <- data.frame(value= c(occ_res_full[index==TRUE,fields_to_all[ic]],
                                                                 occ_dup[index_dup==TRUE, fields_to_all[ic]])),
                                 exclude = NA) %>%
            data.frame() %>%
            dplyr::arrange(desc(Freq))

          if(NROW(freq_data_col)==0)
          {
            freq_data_col <- data.frame(value = 'empty',
                                        freq = occ_dup[index_dup==TRUE, 'parseGBIF_num_duplicates'][1])
          }else
          {
            colnames(freq_data_col) <- c('value','freq')
            freq_tmp <- occ_dup[index_dup==TRUE, 'parseGBIF_num_duplicates'][1] - sum(freq_data_col$freq)
            if(freq_tmp>0) #freq_data_col$freq[freq_data_col$value=='empty']>0)
            {
              freq_data_col <- freq_data_col %>%
                dplyr::add_row(value = 'empty',
                               freq = freq_tmp)
            }
          }

          # x <- paste0('{"',fields_to_all[ic],'":',jsonify::to_json( freq_data_col),"}" )
          x <- paste0('"',fields_to_all[ic],'":',jsonify::to_json( freq_data_col))

          freq_data_col_full <- paste0(freq_data_col_full,
                                       ifelse(freq_data_col_full=="","",","),
                                       x)

          # stop()

          ix <- 5
          # x <- ""
          x_jonsom <- ""

          occ_res_full$Ctrl_eventDate <-  as.character(occ_res_full$Ctrl_eventDate)

          for(ix in 1:NROW(data_col_dup))
          {

            fromJSON_flag <- FALSE
            fromJSON_flag2 <- FALSE

            if(is.na(data_col_dup[ix]))
            {
              next
            }

            # bug aqui
            if(nchar(data_col_dup[ix])>10000)
            {
              next
            }

            data_col_dup_ix <- gsub('\\{|\\}|\\[|\\]|\\(|\\)|\\\\|\\*', '', data_col_dup[ix])

            x_Ctrl_gbifID_dup <- occ_dup[index_dup==TRUE, 'Ctrl_gbifID'][ix]

            x_data_col_dup <- toupper(data_col_dup_ix)

            if(x_data_col == x_data_col_dup)
            {
              next
            }

            # # nao armazear dados repetidos entre duplicatas
            # if(any(data_col_dup_ix == data_col_dup[-ix] %>%
            #        ifelse(is.na(.), FALSE,.) )==TRUE )

            if (x_jonsom != "")
            {
              x_jonsom_test <- paste0('{',x_jonsom, ']}')

              fromJSON_flag2 <- FALSE

              try({
                x_jonsom_test <- jsonlite::fromJSON(x_jonsom_test)
                fromJSON_flag2 <- TRUE}, silent = TRUE)


              if(fromJSON_flag2 == FALSE)
              {
                next
              }


              if(data_col_dup_ix %in% x_jonsom_test[fields_to_all[ic]] %>% ifelse(is.na(.), FALSE,.))
              {
                next
              }

            }


            # armazena informações novas para o campo
            # cabeçalho

            if(x_jonsom=="") #aqui
            {
              # # if(data_col %>% as.character() !="")
              # if(fields_to_all[ic] == "Ctrl_gbifID" )
              # {
              x_jonsom <-  paste0('"',fields_to_all[ic],'"',":[",'"', gsub('"','',data_col),'"')
              # x_jonsom <-  paste0('"',fields_to_all[ic],'"',":[",'"', gsub('"','',data_col),':[',x_Ctrl_gbifID,']','"')
              # }else
              # {
              # x_jonsom <-  paste0('"',fields_to_all[ic],'"',":[")
              # }
            }


            # x_data_col_dup_ix <- paste0( "{\"Ctrl_locality\":[\"Comunidade de São Salvador., Rio Madeira\"",']}')
            x_data_col_dup_ix <- paste0( "{\"Ctrl_locality\":[\"",data_col_dup_ix,"\"",']}')


            fromJSON_flag <- FALSE

            try({
              x_test <- jsonlite::fromJSON(x_data_col_dup_ix)
              fromJSON_flag <- TRUE}, silent = TRUE)


            if(fromJSON_flag == FALSE)
            {
              next
            }


            # dados
            if(substr(x_jonsom,str_count(x_jonsom),str_count(x_jonsom)) == '[')
            {
              # x_jonsom <- paste0(x_jonsom, '"', gsub('"','',data_col_dup[ix]),':[',x_Ctrl_gbifID_dup,']"')

              # x_jonsom <- paste0(x_jonsom, '"', gsub('"','',data_col_dup[ix]),'"')
              x_jonsom <- paste0(x_jonsom, '"', gsub('"','',data_col_dup_ix),'"')

            }else
            {
              # x_jonsom <- paste0(x_jonsom, ",", '"', gsub('"','',data_col_dup[ix]),':[',x_Ctrl_gbifID_dup,']"')

              # x_jonsom <- paste0(x_jonsom, ",", '"', gsub('"','',data_col_dup[ix]),'"')
              x_jonsom <- paste0(x_jonsom, ",", '"', gsub('"','',data_col_dup_ix),'"')

            }


            # combina dasdos de uma amostra, adiciona informações novas à culunas vazias
            # referencia a infoamação com Ctrl_gbifID em parseGBIF_merged_fields
            if(x_data_col=="" &
               fields_to_all[ic] %in% fields_to_merge)
            {
              # print('merge')

              occ_res_full[index==TRUE,
                           fields_to_all[ic]] <- data_col_dup_ix

              # occ_res_full[index==TRUE,
              #              fields_to_all[ic]] <- dplyr::coalesce(occ_res_full[index==TRUE,
              #                                                                 fields_to_all[ic]], data_col_dup_ix)


              if(parseGBIF_merged_fields=="")
              {
                # parseGBIF_merged_fields <- paste0('"', fields_to_all[ic],'":[','"', ix+1,'"]')
                # parseGBIF_merged_fields <- paste0('"', fields_to_all[ic],'":[','"', ix,'"]')
                parseGBIF_merged_fields <- paste0('"', fields_to_all[ic],'":[','"', x_Ctrl_gbifID_dup,'"]')

              }else
              {
                # parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', fields_to_all[ic],'":[','"', ix+1,'"]')
                # parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', fields_to_all[ic],'":[','"', ix,'"]')
                parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', fields_to_all[ic],'":[','"', x_Ctrl_gbifID_dup,'"]')
              }
            }

          }

          if (x_jonsom != "")
          {
            x_jonsom <- paste0(x_jonsom, ']')

            if (x_jonsom_full=="")
            {
              x_jonsom_full <- x_jonsom
            }else
            {
              x_jonsom_full <- paste0(x_jonsom_full,',',x_jonsom)
            }
          }


        }

        if(x_jonsom_full != "")
        {
          occ_res_full$parseGBIF_duplicates_map[index==TRUE] <- paste0('{',x_jonsom_full,'}')
        }

        if (parseGBIF_merged_fields != "")
        {
          occ_res_full[index==TRUE,
                       c('parseGBIF_merged_fields',
                         'parseGBIF_merged')] <- c(paste0('{',parseGBIF_merged_fields,'}'),
                                                   TRUE)
        }

        if (freq_data_col_full != "")
        {
          occ_res_full[index==TRUE,'parseGBIF_freq_duplicate_or_missing_data'] <- paste0('{',freq_data_col_full,'}')
        }



      }
    }
  }

  if(merge_unusable_data==TRUE)
  {
    # in dup out_to_recover
    {

      occ_out_to_recover_merge <- occ_res_full %>%
        dplyr::filter(parseGBIF_dataset_result == 'unusable')
      # dplyr::filter(parseGBIF_digital_voucher == TRUE,
      #               (parseGBIF_unidentified_sample == TRUE |
      #                  parseGBIF_useful_for_spatial_analysis == FALSE))

      occ_res_full <- occ_res_full %>%
        dplyr::filter(parseGBIF_dataset_result == 'useable')
      # dplyr::filter(parseGBIF_digital_voucher == TRUE,
      #               parseGBIF_unidentified_sample == FALSE, # parseGBIF_sample_taxon_name_status %in% c('identified','divergent identifications'),
      #               parseGBIF_useful_for_spatial_analysis == TRUE)


    }

    occ_all <- rbind(occ_res_full,
                     occ_out_to_recover_merge,
                     occ_dup)

  }else
  {

    occ_all <- rbind(occ_res_full,
                     occ_out_to_recover,
                     occ_dup)

  }

  # summary
  {
    occ_tmp <- occ_all

    {
      parseGBIF_general_summary <- data.frame(question='',
                                              value="0")[-1,]

      parseGBIF_merge_fields_summary_complete <-
        parseGBIF_merge_fields_summary_incomplete <-
        parseGBIF_merge_fields_summary <-
        parseGBIF_general_summary

      add_summary <- function(question='',
                              value="0",
                              data = NA)
      {
        data <- data %>%
          dplyr::add_row(question=question,
                         value=as.character(value))
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
    parseGBIF_general_summary <- add_summary(question, value, parseGBIF_general_summary)
    parseGBIF_general_summary

    question <- 'total number of unique collection events'
    value <- sum(occ_tmp$parseGBIF_digital_voucher==TRUE)
    # value <- paste0(NROW(occ_tmp %>% dplyr::filter(parseGBIF_digital_voucher==TRUE)),
    #                 " (complete: ",sum(occ_tmp$parseGBIF_digital_voucher == TRUE &
    #                                      occ_tmp$parseGBIF_dataset_result=='in'),
    #                 " / incomplete: ",sum(occ_tmp$parseGBIF_digital_voucher == TRUE &
    #                                         occ_tmp$parseGBIF_dataset_result=='out_to_recover'),")")
    parseGBIF_general_summary <- add_summary(question, value, parseGBIF_general_summary)
    parseGBIF_general_summary

    question <- 'total number of unique collection events complete'
    value <- sum(occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='in')
    parseGBIF_general_summary <- add_summary(question, value, parseGBIF_general_summary)
    parseGBIF_general_summary


    question <- 'total number of unique collection events incomplete'
    value <- sum(occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='out_to_recover')
    parseGBIF_general_summary <- add_summary(question, value, parseGBIF_general_summary)
    parseGBIF_general_summary

    question <- 'total number of duplicates'
    value <- sum(occ_tmp$parseGBIF_dataset_result=='dup')
    parseGBIF_general_summary <- add_summary(question, value, parseGBIF_general_summary)
    parseGBIF_general_summary


    question <- paste0('total unique collection events containing merged fields', ifelse(merge_unusable_data==TRUE,'',
                                                                                         ' (only unique collection events complete)'))
    value <- NROW(occ_tmp %>%
                    dplyr::filter(parseGBIF_merged==TRUE))
    parseGBIF_general_summary <- add_summary(question, value, parseGBIF_general_summary)
    parseGBIF_general_summary

    x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp)
    x_freq_merged_fields$id <- paste0(x_freq_merged_fields$id,' : total merge actions')
    parseGBIF_merge_fields_summary <- add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, parseGBIF_merge_fields_summary)
    parseGBIF_merge_fields_summary

    if(NROW(occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='useable'))>0)
    {
    x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='useable'))
    x_freq_merged_fields$id <- paste0(x_freq_merged_fields$id,' : merge actions ')
    parseGBIF_merge_fields_summary_complete <- add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, parseGBIF_merge_fields_summary_complete)
    parseGBIF_merge_fields_summary_complete
    }

    if(merge_unusable_data==TRUE)
    {
      x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='unusable'))
      x_freq_merged_fields$id <- paste0(x_freq_merged_fields$id,' : merge actions ')
      parseGBIF_merge_fields_summary_incomplete <- add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, parseGBIF_merge_fields_summary_incomplete)
      parseGBIF_merge_fields_summary_incomplete
    }else
    {
      parseGBIF_merge_fields_summary_incomplete <- data.frame(question='',
                                                              value="0")[-1,]
    }

  }


  if(merge_unusable_data==TRUE)
  {
    return(list(all_data = occ_all,
                useable_data_merge = occ_res_full,
                useable_data_raw = occ_in,
                duplicates = occ_dup,
                unusable_data_merge = occ_out_to_recover_merge,
                unusable_data_raw = occ_out_to_recover
                # parseGBIF_general_summary = parseGBIF_general_summary,
                # parseGBIF_merge_fields_summary = parseGBIF_merge_fields_summary,
                # parseGBIF_merge_fields_summary_useable_data = parseGBIF_merge_fields_summary_complete,
                # parseGBIF_merge_fields_summary_unusable_data = parseGBIF_merge_fields_summary_incomplete
    ))
  }else
  {

    return(list(all_data = occ_all,
                useable_data_merge = occ_res_full,
                useable_data_raw = occ_in,
                duplicates = occ_dup,
                unusable_data_merge = NA,
                unusable_data_raw = occ_out_to_recover
                # parseGBIF_general_summary = parseGBIF_general_summary,
                # parseGBIF_merge_fields_summary = parseGBIF_merge_fields_summary,
                # parseGBIF_merge_fields_summary_useable_data = parseGBIF_merge_fields_summary_complete,
                # parseGBIF_merge_fields_summary_unusable_data = parseGBIF_merge_fields_summary_incomplete
    ))
  }

}
