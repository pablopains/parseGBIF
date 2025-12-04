#' @title Export of results Cluster - divides the work into 24 steps to reduce the total processing time
#' @name export_data_cluster
#'
#' @description divides the work into 24 steps to reduce the total processing time
#' For each unique collection event key, complete or incomplete,
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
#' @param n_minimo minimum number of records to split processing
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
#'                        merge_unusable_data = merge_unusable_data)
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
export_data_cluster <- function(occ_digital_voucher_file = '',
                             occ_digital_voucher = NA,
                             merge_unusable_data = TRUE,
                             n_minimo=10000)
{

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

    occ_digital_voucher <- readr::read_csv(occ_digital_voucher_file,
                               locale = readr::locale(encoding = "UTF-8"),
                               show_col_types = FALSE) %>% as.data.frame()
  }

    if (NROW(occ_digital_voucher)==0)
    {
      stop("Empty occurrence data frame!")
    }


  # divide
  {
    # divide
    n_part <- 24

    export_data1 <- export_data2 <- export_data3 <-
      export_data4 <- export_data5 <- digital_voucher6 <-
      export_data7 <- export_data8 <- export_data9 <-
      export_data10 <- export_data11 <- export_data12 <-
      export_data13 <- export_data14 <- export_data15 <-
      export_data16 <- export_data17 <- export_data18 <-
      export_data19 <- export_data20 <- export_data21 <-
      export_data22 <- export_data23 <- export_data24 <- {}


    {

      key <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %>% unique()
      n_k <- NROW(key)
      tamanho_parte <- round(n_k / n_part,0)
      inicio <- rep(0,n_part)
      fim <- rep(0,n_part)

      i <- 1
      for(i in 1:n_part)
      {
        inicio[i] <- (i - 1) * tamanho_parte + 1
        fim[i] <- min(n_k, i * tamanho_parte)
      }

      i_key <- 1:n_k

      ind_k1 <- i_key >= inicio[1] & i_key <= fim[1]
      ind_k2 <- i_key >= inicio[2] & i_key <= fim[2]
      ind_k3 <- i_key >= inicio[3] & i_key <= fim[3]
      ind_k4 <- i_key >= inicio[4] & i_key <= fim[4]
      ind_k5 <- i_key >= inicio[5] & i_key <= fim[5]
      ind_k6 <- i_key >= inicio[6] & i_key <= fim[6]
      ind_k7 <- i_key >= inicio[7] & i_key <= fim[7]
      ind_k8 <- i_key >= inicio[8] & i_key <= fim[8]
      ind_k9 <- i_key >= inicio[9] & i_key <= fim[9]

      ind_k10 <- i_key >= inicio[10] & i_key <= fim[10]
      ind_k11 <- i_key >= inicio[11] & i_key <= fim[11]
      ind_k12 <- i_key >= inicio[12] & i_key <= fim[12]
      ind_k13 <- i_key >= inicio[13] & i_key <= fim[13]
      ind_k14 <- i_key >= inicio[14] & i_key <= fim[14]
      ind_k15 <- i_key >= inicio[15] & i_key <= fim[15]
      ind_k16 <- i_key >= inicio[16] & i_key <= fim[16]
      ind_k17 <- i_key >= inicio[17] & i_key <= fim[17]
      ind_k18 <- i_key >= inicio[18] & i_key <= fim[18]
      ind_k19 <- i_key >= inicio[19] & i_key <= fim[19]
      ind_k20 <- i_key >= inicio[20] & i_key <= fim[20]
      ind_k21 <- i_key >= inicio[21] & i_key <= fim[21]
      ind_k22 <- i_key >= inicio[22] & i_key <= fim[22]
      ind_k23 <- i_key >= inicio[23] & i_key <= fim[23]
      ind_k24 <- i_key >= inicio[24] & i_key <= n_k

      i_k_1 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k1==TRUE]
      i_k_2 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k2==TRUE]
      i_k_3 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k3==TRUE]
      i_k_4 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k4==TRUE]
      i_k_5 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k5==TRUE]
      i_k_6 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k6==TRUE]
      i_k_7 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k7==TRUE]
      i_k_8 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k8==TRUE]
      i_k_9 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k9==TRUE]

      i_k_10 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k10==TRUE]
      i_k_11 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k11==TRUE]
      i_k_12 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k12==TRUE]
      i_k_13 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k13==TRUE]
      i_k_14 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k14==TRUE]
      i_k_15 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k15==TRUE]
      i_k_16 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k16==TRUE]
      i_k_17 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k17==TRUE]
      i_k_18 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k18==TRUE]
      i_k_19 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k19==TRUE]
      i_k_20 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k20==TRUE]
      i_k_21 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k21==TRUE]
      i_k_22 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k22==TRUE]
      i_k_23 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k23==TRUE]
      i_k_24 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k24==TRUE]
    }
  }


  if (NROW(occ_digital_voucher)>=n_minimo)
  {

      print('export_data (1/24)')
      export_data1 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_1==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = FALSE)

      print('export_data (2/24)')
      export_data2 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_2==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)
      print('export_data (3/24)')
      export_data3 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_3==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)
      print('export_data (4/24)')
      export_data4 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_4==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)

      print('export_data (5/24)')
      export_data5 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_5==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)

      print('export_data (6/24)')
      export_data6 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_6==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)

      print('export_data (7/24)')
      export_data7 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_7==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)
      print('export_data (8/24)')
      export_data8 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_8==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)

      print('export_data (9/24)')
      export_data9 <- export_data(occ_digital_voucher_file = '',
                                       occ_digital_voucher = occ_digital_voucher[i_k_9==TRUE,],
                                       merge_unusable_data = merge_unusable_data,
                                       silence = TRUE)

      print('export_data (10/24)')
      export_data10 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_10==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (11/24)')
      export_data11 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_11==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (12/24)')
      export_data12 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_12==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (13/24)')
      export_data13 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_13==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (14/24)')
      export_data14 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_14==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (15/24)')
      export_data15 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_15==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (16/24)')
      export_data16 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_16==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (17/24)')
      export_data17 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_17==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (18/24)')
      export_data18 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_18==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (19/24)')
      export_data19 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_19==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (20/24)')
      export_data20 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_20==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (21/24)')
      export_data21 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_21==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (22/24)')
      export_data22 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_22==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (23/24)')
      export_data23 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_23==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

      print('export_data (24/24)')
      export_data24 <- export_data(occ_digital_voucher_file = '',
                                        occ_digital_voucher = occ_digital_voucher[i_k_24==TRUE,],
                                        merge_unusable_data = merge_unusable_data,
                                        silence = TRUE)

    results <- list(all_data = {})

    results$all_data <- rbind(export_data1$all_data,
                              export_data2$all_data,
                              export_data3$all_data,
                              export_data4$all_data,
                              export_data5$all_data,
                              export_data6$all_data,
                              export_data7$all_data,
                              export_data8$all_data,
                              export_data9$all_data,
                              export_data10$all_data,
                              export_data11$all_data,
                              export_data12$all_data,
                              export_data13$all_data,
                              export_data14$all_data,
                              export_data15$all_data,
                              export_data16$all_data,
                              export_data17$all_data,
                              export_data18$all_data,
                              export_data19$all_data,
                              export_data20$all_data,
                              export_data21$all_data,
                              export_data22$all_data,
                              export_data23$all_data,
                              export_data24$all_data) %>%
      data.frame(stringsAsFactors = FALSE)

      remove(export_data1 , export_data2 , export_data3 ,
      export_data4 , export_data5 , digital_voucher6 ,
      export_data7 , export_data8 , export_data9 ,
      export_data10 , export_data11 , export_data12 ,
      export_data13 , export_data14 , export_data15 ,
      export_data16 , export_data17 , export_data18 ,
      export_data19 , export_data20 , export_data21 ,
      export_data22 , export_data23 , export_data24)

  }else
  {

    print('export_data')
    results <- export_data(occ_digital_voucher_file = '',
                                occ_digital_voucher = occ_digital_voucher,
                                merge_unusable_data = merge_unusable_data,
                                silence = TRUE)


  }

  return(results$all_data)

}
