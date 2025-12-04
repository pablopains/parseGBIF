#' @title Selecting the digital voucher cluster - divides the processing for selecting the digital voucher into 24 parts
#' @name select_digital_voucher_cluster
#'
#' @description Divides the processing for selecting the digital voucher into 24 parts
#' To group duplicates and choose the digital voucher:
#' Unique collection events can result in many ‘duplicate’ GBIF records. We designate one of these ‘duplicate’ records
#' as the master digital voucher, to which data from other duplicate vouchers can be merged (see export_data):
#'
#' __Where the collection event key for grouping duplicates is complete__, then duplicates can be grouped / parsed.
#' To do so, we evaluate record completeness. Record completeness is calculated based on data-quality scores
#' for the information in the following  fields: recordedBy, recordNumber, year, institutionCode, catalogNumber, locality, municipality,
#' countryCode, stateProvince and fieldNotes. The spatial coordinates associated with each duplicate are ranked using a score for the
#' quality of the geospatial information. This score is calculated using the issues listed in the GBIF table, EnumOccurrenceIssue.
#' A score is calculated based on these issues (see above). The duplicate with the highest total score is assigned as the master voucher
#' for the unique collection event. Missing information contained in duplicate records of the unique collection event can then be merged
#' into the master digital voucher (see export_data).
#'
#' __Where the collection event key is incomplete__, unique collection event duplicates cannot be parsed. In this case,
#' each record is considered as a unique collection event, without duplicates. However, to know the integrity
#' of the information, record completeness and quality of the geospatial information, are evaluated as described above.
#'
#' __How is the quality score calculated?__
#' parseGBIF_digital_voucher = The duplicate with the highest total score, sum of record completeness + quality of geospatial information.
#'
#' __How is record completeness calculated?__
#' The quality of the duplicate records associated with each collection event key is measured as the
#' completeness of a record, using the sum of a number of flags (see below) equal to TRUE.
#'
#' __Flags used to calculate record completeness__
#'
#' * Is there information about the collector?
#' * Is there information about the collection number?
#' * Is there information about the year of collection?
#' * Is there information about the institution code?
#' * Is there information about the catalog number?
#' * Is there information about the locality?
#' * Is there information about the municipality of collection?
#' * Is there information about the state/province of collection?
#' * Is there information about the field notes?
#'
#' __The quality of geospatial information is based on geographic issues raised by GBIF.__
#' GIBF issues relating to geospatial data were classified into three classes based on the data quality
#' scores that we assigned to each of the following GBIF issues recorded in the EnumOccurrenceIssue.
#'
#' * Issue does not affect coordinating accuracy, with selection_score equal to -1
#' * Issue has potential to affect coordinate accuracy, with selection_score equal to -3
#' * Records with a selection_score equal to -9 are excluded.
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param occ_gbif_issue = result of function extract_gbif_issue()$occ_gbif_issue
#' @param occ_wcvp_check_name = result of function batch_checkName_wcvp()$occ_wcvp_check_name
#' @param occ_collectorsDictionary = result of function update_collectorsDictionary()$occ_collectorsDictionary
#' @param enumOccurrenceIssue An enumeration of validation rules for single occurrence records by GBIF file, if NA, will be used, data(EnumOccurrenceIssue)
#' @param n_minimo minimum number of records to split processing
#'
#' @details
#' * parseGBIF_duplicates_grouping_status - "groupable", "not groupable: no recordedBy and no recordNumber",
#' "not groupable: no recordNumber" or "not groupable: no recordedBy"
#' * parseGBIF_num_duplicates number of duplicates records
#' * parseGBIF_duplicates TRUE/FALSE
#' * parseGBIF_non_groupable_duplicates TRUE/FALSE
#'
#'
#' @return list with two data frames: occ_digital voucher_and:
#' occ_digital_voucher,  with all data processing fields and
#' occ_results, only result fields.
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @examples
#' \donttest{
#' help(select_digital_voucher_cluster)
#'
#' head(occ)
#' head(res_gbif_issue$occ_gbif_issue)
#' head(res_checkName_wcvp$occ_wcvp_check_name)
#' head(res_collectorsDictionary$occ_collectorsDictionary)
#' res_digital_voucher_and_sample_identification <- select_digital_voucher_cluster(occ = occ,
#'                                                                         occ_gbif_issue = res_gbif_issue$occ_gbif_issue,
#'                                                                         occ_wcvp_check_name = res_checkName_wcvp$occ_wcvp_check_name,
#'                                                                         occ_collectorsDictionary = res_collectorsDictionary$occ_collectorsDictionary)
#'
#' names(res_digital_voucher_and_sample_identification)
#'
#' head(res_digital_voucher_and_sample_identification$occ_digital_voucher)
#' colnames(res_digital_voucher_and_sample_identification$occ_digital_voucher)
#'
#' }
#' @export
select_digital_voucher_cluster <- function(occ = NA,
                                           occ_gbif_issue= NA,
                                           occ_wcvp_check_name = NA,
                                           occ_collectorsDictionary = NA,
                                           file_name_occ_issue=NA,
                                           file_name_occ_wcvp_check_name=NA,
                                           file_name_occ_collectorsDictionary=NA,
                                           n_minimo=10000)
{
# file_name_occ_issue <- 'parseGBIF_1_occ_issue.csv'
  if(!is.na(file_name_occ_issue)){
occ_gbif_issue <- readr::read_csv(file_name_occ_issue,
                                  locale = readr::locale(encoding = "UTF-8"),
                                  show_col_types = FALSE)}

  if(!is.na(file_name_occ_wcvp_check_name)){
# file_name_occ_wcvp_check_name <- 'parseGBIF_2_occ_wcvp_check_name.csv'
occ_wcvp_check_name <- readr::read_csv(file_name_occ_wcvp_check_name,
                                       locale = readr::locale(encoding = "UTF-8"),
                                       show_col_types = FALSE)}

  if(!is.na(file_name_occ_collectorsDictionary)){
# file_name_occ_collectorsDictionary <- 'parseGBIF_3_occ_collectorsDictionary.csv'
occ_collectorsDictionary <- readr::read_csv(file_name_occ_collectorsDictionary,
                                            locale = readr::locale(encoding = "UTF-8"),
                                            show_col_types = FALSE)}


if(NROW(occ)<=n_minimo)
{
  digital_voucher <- select_digital_voucher(occ = occ,
                                            occ_gbif_issue = occ_gbif_issue,
                                            occ_wcvp_check_name = occ_wcvp_check_name,
                                            occ_collectorsDictionary = occ_collectorsDictionary,
                                            silence = FALSE)
}else
{

  # divide
  {
    n_part <- 24

    digital_voucher1 <- digital_voucher2 <- digital_voucher3 <-
      digital_voucher4 <- digital_voucher5 <- digital_voucher6 <-
      digital_voucher7 <- digital_voucher8 <- digital_voucher9 <-
      digital_voucher10 <- digital_voucher11 <- digital_voucher12 <-
      digital_voucher13 <- digital_voucher14 <- digital_voucher15 <-
      digital_voucher16 <- digital_voucher17 <- digital_voucher18 <-
      digital_voucher19 <- digital_voucher20 <- digital_voucher21 <-
      digital_voucher22 <- digital_voucher23 <- digital_voucher24 <- {}

    key <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %>% unique()
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

    i_k_1 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k1==TRUE]
    i_k_2 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k2==TRUE]
    i_k_3 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k3==TRUE]
    i_k_4 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k4==TRUE]
    i_k_5 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k5==TRUE]
    i_k_6 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k6==TRUE]
    i_k_7 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k7==TRUE]
    i_k_8 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k8==TRUE]
    i_k_9 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k9==TRUE]

    i_k_10 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k10==TRUE]
    i_k_11 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k11==TRUE]
    i_k_12 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k12==TRUE]
    i_k_13 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k13==TRUE]
    i_k_14 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k14==TRUE]
    i_k_15 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k15==TRUE]
    i_k_16 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k16==TRUE]
    i_k_17 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k17==TRUE]
    i_k_18 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k18==TRUE]
    i_k_19 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k19==TRUE]
    i_k_20 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k20==TRUE]
    i_k_21 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k21==TRUE]
    i_k_22 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k22==TRUE]
    i_k_23 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k23==TRUE]
    i_k_24 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k24==TRUE]
  }

  print('select_digital_voucher (1/24)')
  digital_voucher1 <- select_digital_voucher(occ = occ[i_k_1==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_1==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_1==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_1==TRUE,],
                                             silence = FALSE)

  print('select_digital_voucher (2/24)')
  digital_voucher2 <- select_digital_voucher(occ = occ[i_k_2==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_2==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_2==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_2==TRUE,],
                                             silence = TRUE)
  print('select_digital_voucher (3/24)')
  digital_voucher3 <- select_digital_voucher(occ = occ[i_k_3==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_3==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_3==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_3==TRUE,],
                                             silence = TRUE)

  print('select_digital_voucher (4/24)')
  digital_voucher4 <- select_digital_voucher(occ = occ[i_k_4==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_4==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_4==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_4==TRUE,],
                                             silence = TRUE)

  print('select_digital_voucher (5/24)')
  digital_voucher5 <- select_digital_voucher(occ = occ[i_k_5==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_5==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_5==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_5==TRUE,],
                                             silence = TRUE)

  print('select_digital_voucher (6/24)')
  digital_voucher6 <- select_digital_voucher(occ = occ[i_k_6==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_6==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_6==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_6==TRUE,],
                                             silence = TRUE)

  print('select_digital_voucher (7/24)')
  digital_voucher7 <- select_digital_voucher(occ = occ[i_k_7==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_7==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_7==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_7==TRUE,],
                                             silence = TRUE)

  print('select_digital_voucher (8/24)')
  digital_voucher8 <- select_digital_voucher(occ = occ[i_k_8==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_8==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_8==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_8==TRUE,],
                                             silence = TRUE)

  print('select_digital_voucher (9/24)')
  digital_voucher9 <- select_digital_voucher(occ = occ[i_k_9==TRUE,],
                                             occ_gbif_issue = occ_gbif_issue[i_k_9==TRUE,],
                                             occ_wcvp_check_name = occ_wcvp_check_name[i_k_9==TRUE,],
                                             occ_collectorsDictionary = occ_collectorsDictionary[i_k_9==TRUE,],
                                             silence = TRUE)

  print('select_digital_voucher (10/24)')
  digital_voucher10 <- select_digital_voucher(occ = occ[i_k_10==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_10==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_10==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_10==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (11/24)')
  digital_voucher11 <- select_digital_voucher(occ = occ[i_k_11==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_11==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_11==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_11==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (12/24)')
  digital_voucher12 <- select_digital_voucher(occ = occ[i_k_12==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_12==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_12==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_12==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (13/24)')
  digital_voucher13 <- select_digital_voucher(occ = occ[i_k_13==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_13==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_13==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_13==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (14/24)')
  digital_voucher14 <- select_digital_voucher(occ = occ[i_k_14==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_14==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_14==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_14==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (15/24)')
  digital_voucher15 <- select_digital_voucher(occ = occ[i_k_15==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_15==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_15==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_15==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (16/24)')
  digital_voucher16 <- select_digital_voucher(occ = occ[i_k_16==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_16==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_16==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_16==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (17/24)')
  digital_voucher17 <- select_digital_voucher(occ = occ[i_k_17==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_17==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_17==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_17==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (18/24)')
  digital_voucher18 <- select_digital_voucher(occ = occ[i_k_18==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_18==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_18==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_18==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (19/24)')
  digital_voucher19 <- select_digital_voucher(occ = occ[i_k_19==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_19==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_19==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_19==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (20/24)')
  digital_voucher20 <- select_digital_voucher(occ = occ[i_k_20==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_20==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_20==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_20==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (21/24)')
  digital_voucher21 <- select_digital_voucher(occ = occ[i_k_21==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_21==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_21==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_21==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (22/24)')
  digital_voucher22 <- select_digital_voucher(occ = occ[i_k_22==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_22==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_22==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_22==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (23/24)')
  digital_voucher23 <- select_digital_voucher(occ = occ[i_k_23==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_23==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_23==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_23==TRUE,],
                                              silence = TRUE)

  print('select_digital_voucher (24/24)')
  digital_voucher24 <- select_digital_voucher(occ = occ[i_k_24==TRUE,],
                                              occ_gbif_issue = occ_gbif_issue[i_k_24==TRUE,],
                                              occ_wcvp_check_name = occ_wcvp_check_name[i_k_24==TRUE,],
                                              occ_collectorsDictionary = occ_collectorsDictionary[i_k_24==TRUE,],
                                              silence = TRUE)
  # results <- list(all_data = {},
  #                 occ_results = {})


  digital_voucher <- rbind(digital_voucher1$occ_digital_voucher,
                                               digital_voucher2$occ_digital_voucher,
                                               digital_voucher3$occ_digital_voucher,
                                               digital_voucher4$occ_digital_voucher,
                                               digital_voucher5$occ_digital_voucher,
                                               digital_voucher6$occ_digital_voucher,
                                               digital_voucher7$occ_digital_voucher,
                                               digital_voucher8$occ_digital_voucher,
                                               digital_voucher9$occ_digital_voucher,
                                               digital_voucher10$occ_digital_voucher,
                                               digital_voucher11$occ_digital_voucher,
                                               digital_voucher12$occ_digital_voucher,
                                               digital_voucher13$occ_digital_voucher,
                                               digital_voucher14$occ_digital_voucher,
                                               digital_voucher15$occ_digital_voucher,
                                               digital_voucher16$occ_digital_voucher,
                                               digital_voucher17$occ_digital_voucher,
                                               digital_voucher18$occ_digital_voucher,
                                               digital_voucher19$occ_digital_voucher,
                                               digital_voucher20$occ_digital_voucher,
                                               digital_voucher21$occ_digital_voucher,
                                               digital_voucher22$occ_digital_voucher,
                                               digital_voucher23$occ_digital_voucher,
                                               digital_voucher24$occ_digital_voucher) %>%
    data.frame(stringsAsFactors = FALSE)



}

return(digital_voucher)
}
