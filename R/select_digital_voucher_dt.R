#' @title Selecting the master digital voucher (data.table version)
#' @name select_digital_voucher_dt
#'
#' @description To group duplicates and choose the digital voucher:
#' Unique collection events can result in many 'duplicate' GBIF records. We designate one of these 'duplicate' records
#' as the master digital voucher, to which data from other duplicate vouchers can be merged (see export_data).
#'
#' This is an optimized version using \code{data.table} for improved performance on large datasets,
#' while maintaining the same interface and output as \code{select_digital_voucher}.
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
#' @param occ_gbif_issue result of function extract_gbif_issue()$occ_gbif_issue
#' @param occ_wcvp_check_name result of function batch_checkName_wcvp()$occ_wcvp_check_name
#' @param occ_collectorsDictionary result of function update_collectorsDictionary()$occ_collectorsDictionary
#' @param enumOccurrenceIssue An enumeration of validation rules for single occurrence records by GBIF file, if NA, will be used, data(EnumOccurrenceIssue)
#' @param silence if TRUE does not display progress messages
#'
#' @details
#' * parseGBIF_duplicates_grouping_status - "groupable", "not groupable: no recordedBy and no recordNumber",
#' "not groupable: no recordNumber" or "not groupable: no recordedBy"
#' * parseGBIF_num_duplicates number of duplicates records
#' * parseGBIF_duplicates TRUE/FALSE
#' * parseGBIF_non_groupable_duplicates TRUE/FALSE
#'
#'
#' @return list with two data frames:
#' - `occ_digital_voucher`: all data processing fields (as data.frame)
#' - `occ_results`: only result fields (as data.frame)
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}, \code{\link[ParsGBIF]{select_digital_voucher}}
#'
#' @examples
#' \donttest{
#' help(select_digital_voucher_dt)
#'
#' head(occ)
#' head(res_gbif_issue$occ_gbif_issue)
#' head(res_checkName_wcvp$occ_wcvp_check_name)
#' head(res_collectorsDictionary$occ_collectorsDictionary)
#' res_digital_voucher_and_sample_identification <- select_digital_voucher_dt(occ = occ,
#'                                                                         occ_gbif_issue = res_gbif_issue$occ_gbif_issue,
#'                                                                         occ_wcvp_check_name = res_checkName_wcvp$occ_wcvp_check_name,
#'                                                                         occ_collectorsDictionary = res_collectorsDictionary$occ_collectorsDictionary,
#'                                                                         enumOccurrenceIssue = EnumOccurrenceIssue)
#'
#' names(res_digital_voucher_and_sample_identification)
#'
#' head(res_digital_voucher_and_sample_identification$occ_digital_voucher)
#' colnames(res_digital_voucher_and_sample_identification$occ_digital_voucher)
#'
#' }
#'
#' @importFrom data.table as.data.table set setorder setkey rbindlist fread
#' @importFrom stringr str_sub str_count str_locate
#' @export
select_digital_voucher_dt <- function(occ = NA,
                                       occ_gbif_issue = NA,
                                       occ_wcvp_check_name = NA,
                                       occ_collectorsDictionary = NA,
                                       enumOccurrenceIssue = NA,
                                       silence = TRUE) {
  
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  
  library(data.table)
  
  # -------------------------
  # 0) EnumOccurrenceIssue
  # -------------------------
  if (is.na(enumOccurrenceIssue)[1]) {
    data(EnumOccurrenceIssue, envir = environment())
  } else {
    EnumOccurrenceIssue <- enumOccurrenceIssue
  }
  
  # -------------------------
  # 1) cbind (dados alinhados linha a linha)
  # -------------------------
  occ_in <- occ
  occ_df <- cbind(occ_gbif_issue, occ_in, occ_wcvp_check_name, occ_collectorsDictionary)
  
  # Garante nomes únicos
  nms <- colnames(occ_df)
  if (anyDuplicated(nms)) {
    colnames(occ_df) <- make.unique(nms, sep = "__dup__")
  }
  
  occ_dt <- as.data.table(occ_df)
  
  # normalizações WCVP
  if ("wcvp_taxon_rank" %in% names(occ_dt)) {
    occ_dt[, wcvp_taxon_rank := fifelse(is.na(wcvp_taxon_rank), "", wcvp_taxon_rank)]
  }
  if ("wcvp_taxon_status" %in% names(occ_dt)) {
    occ_dt[, wcvp_taxon_status := fifelse(is.na(wcvp_taxon_status), "", wcvp_taxon_status)]
  }
  
  # índices de issues geoespaciais
  index_tmp1 <- EnumOccurrenceIssue$score == 1 & EnumOccurrenceIssue$type == "geospatial"
  index_tmp2 <- EnumOccurrenceIssue$score == 2 & EnumOccurrenceIssue$type == "geospatial"
  index_tmp3 <- EnumOccurrenceIssue$score == 3 & EnumOccurrenceIssue$type == "geospatial"
  index_tmp1 <- ifelse(is.na(index_tmp1), FALSE, index_tmp1)
  index_tmp2 <- ifelse(is.na(index_tmp2), FALSE, index_tmp2)
  index_tmp3 <- ifelse(is.na(index_tmp3), FALSE, index_tmp3)
  
  # -------------------------
  # 2) Flags Ctrl_verbatim_quality
  # -------------------------
  occ_dt[, temAnoColeta :=
           ifelse(is.na(Ctrl_year) | Ctrl_year == "" | Ctrl_year == 0 | Ctrl_year <= 10, FALSE, TRUE)]
  occ_dt[, temAnoColeta := ifelse(is.na(temAnoColeta), FALSE, temAnoColeta)]
  
  occ_dt[, temCodigoInstituicao := ifelse(is.na(Ctrl_institutionCode) | Ctrl_institutionCode == "", FALSE, TRUE)]
  occ_dt[, temCodigoInstituicao := ifelse(is.na(temCodigoInstituicao), FALSE, temCodigoInstituicao)]
  
  occ_dt[, temNumeroCatalogo := ifelse(is.na(Ctrl_catalogNumber) | Ctrl_catalogNumber == "", FALSE, TRUE)]
  occ_dt[, temNumeroCatalogo := ifelse(is.na(temNumeroCatalogo), FALSE, temNumeroCatalogo)]
  
  occ_dt[, temColetor := ifelse(is.na(Ctrl_recordedBy) | Ctrl_recordedBy == "", FALSE, TRUE)]
  occ_dt[, temColetor := ifelse(is.na(temColetor), FALSE, temColetor)]
  
  occ_dt[, temNumeroColeta := ifelse(is.na(Ctrl_recordNumber) | Ctrl_recordNumber == "", FALSE, TRUE)]
  occ_dt[, temNumeroColeta := ifelse(is.na(temNumeroColeta), FALSE, temNumeroColeta)]
  
  occ_dt[, temPais := ifelse(COUNTRY_INVALID == TRUE, FALSE, TRUE)]
  occ_dt[, temPais := ifelse(is.na(temPais), FALSE, temPais)]
  
  occ_dt[, temUF := ifelse(is.na(Ctrl_stateProvince) | Ctrl_stateProvince == "", FALSE, TRUE)]
  occ_dt[, temUF := ifelse(is.na(temUF), FALSE, temUF)]
  
  occ_dt[, temMunicipio := ifelse(is.na(Ctrl_municipality) | Ctrl_municipality == "", FALSE, TRUE)]
  occ_dt[, temMunicipio := ifelse(is.na(temMunicipio), FALSE, temMunicipio)]
  
  occ_dt[, temLocalidade := ifelse(is.na(Ctrl_locality) | Ctrl_locality == "", FALSE, TRUE)]
  occ_dt[, temLocalidade := ifelse(is.na(temLocalidade), FALSE, temLocalidade)]
  
  occ_dt[, temNotas := ifelse(is.na(Ctrl_fieldNotes) | Ctrl_fieldNotes == "", FALSE, TRUE)]
  occ_dt[, temNotas := ifelse(is.na(temNotas), FALSE, temNotas)]
  
  # -------------------------
  # 3) Inicializações
  # -------------------------
  occ_dt[, `:=`(
    Ctrl_geospatial_quality = 0,
    Ctrl_verbatim_quality = 0,
    Ctrl_moreInformativeRecord = 0,
    parseGBIF_digital_voucher = FALSE,
    parseGBIF_duplicates = FALSE,
    parseGBIF_non_groupable_duplicates = FALSE,
    parseGBIF_num_duplicates = 0,
    parseGBIF_duplicates_grouping_status = "",
    Ctrl_coordinates_validated_by_gbif_issue = FALSE
  )]
  
  cols3 <- EnumOccurrenceIssue$constant[index_tmp3 == TRUE]
  cols2 <- EnumOccurrenceIssue$constant[index_tmp2 == TRUE]
  cols1 <- EnumOccurrenceIssue$constant[index_tmp1 == TRUE]
  
  for (cc in unique(c(cols1, cols2, cols3))) {
    if (!is.na(cc) && nzchar(cc) && !(cc %in% names(occ_dt))) occ_dt[, (cc) := 0L]
  }
  
  occ_dt[, Ctrl_coordinates_validated_by_gbif_issue :=
           ifelse(rowSums(as.matrix(occ_dt[, ..cols3])) == 0, TRUE, FALSE)]
  
  occ_dt[, Ctrl_coordinates_validated_by_gbif_issue :=
           ifelse(Ctrl_hasCoordinate == FALSE | Ctrl_decimalLatitude == 0 | Ctrl_decimalLongitude == 0,
                  FALSE, Ctrl_coordinates_validated_by_gbif_issue)]
  
  occ_dt[, Ctrl_coordinates_validated_by_gbif_issue :=
           ifelse(is.na(Ctrl_coordinates_validated_by_gbif_issue), FALSE, Ctrl_coordinates_validated_by_gbif_issue)]
  
  occ_dt[, Ctrl_geospatial_quality :=
           ifelse(rowSums(as.matrix(occ_dt[, ..cols3])) > 0, -9,
                  ifelse(rowSums(as.matrix(occ_dt[, ..cols2])) > 0, -3,
                         ifelse(rowSums(as.matrix(occ_dt[, ..cols1])) > 0, -1, 0)))]
  
  occ_dt[, Ctrl_geospatial_quality := ifelse(Ctrl_hasCoordinate == FALSE, -9, Ctrl_geospatial_quality)]
  
  occ_dt[, Ctrl_verbatim_quality := (
    temColetor +
      temNumeroColeta +
      temAnoColeta +
      temCodigoInstituicao +
      temNumeroCatalogo +
      temLocalidade +
      temMunicipio +
      temUF +
      temPais +
      temNotas
  )]
  
  occ_dt[, Ctrl_moreInformativeRecord := Ctrl_geospatial_quality + Ctrl_verbatim_quality]
  
  # -------------------------
  # 4) Seleciona colunas para o loop
  # -------------------------
  keep_mid <- c(
    "Ctrl_key_family_recordedBy_recordNumber",
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "wcvp_taxon_status",
    "wcvp_searchNotes",
    "Ctrl_taxonRank",
    "Ctrl_geospatial_quality",
    "Ctrl_verbatim_quality",
    "Ctrl_moreInformativeRecord",
    "parseGBIF_digital_voucher",
    "parseGBIF_duplicates",
    "parseGBIF_num_duplicates",
    "parseGBIF_non_groupable_duplicates",
    "parseGBIF_duplicates_grouping_status",
    "Ctrl_coordinates_validated_by_gbif_issue",
    "Ctrl_decimalLatitude",
    "Ctrl_decimalLongitude"
  )
  keep_mid <- keep_mid[keep_mid %in% names(occ_dt)]
  occ_work <- occ_dt[, ..keep_mid]
  
  occ_work[, `:=`(
    parseGBIF_unidentified_sample = TRUE,
    parseGBIF_wcvp_plant_name_id = "",
    parseGBIF_sample_taxon_name = "",
    parseGBIF_sample_taxon_name_status = "",
    parseGBIF_number_taxon_names = 0,
    parseGBIF_useful_for_spatial_analysis = FALSE,
    parseGBIF_decimalLatitude = NA_real_,
    parseGBIF_decimalLongitude = NA_real_
  )]
  
  # Ajuste na chave
  idx <- stringr::str_sub(
    occ_work$Ctrl_key_family_recordedBy_recordNumber,
    stringr::str_count(occ_work$Ctrl_key_family_recordedBy_recordNumber) - 2,
    stringr::str_count(occ_work$Ctrl_key_family_recordedBy_recordNumber)
  ) %in% "_NA"
  occ_work$Ctrl_key_family_recordedBy_recordNumber[idx == TRUE] <-
    stringr::str_sub(
      occ_work$Ctrl_key_family_recordedBy_recordNumber[idx == TRUE],
      1,
      stringr::str_count(occ_work$Ctrl_key_family_recordedBy_recordNumber[idx == TRUE]) - 2
    )
  
  recordedBy_unique <- unique(occ_work$Ctrl_key_family_recordedBy_recordNumber)
  tot <- length(recordedBy_unique)
  s <- 0L
  
  # -------------------------
  # 5) LOOP principal (idêntico)
  # -------------------------
  for (r in recordedBy_unique) {
    
    s <- s + 1L
    if (!silence) if (s %% 1000 == 0) print(paste0(s, " de ", tot))
    
    index_occ <- (occ_work$Ctrl_key_family_recordedBy_recordNumber %in% r)
    index_occ <- ifelse(is.na(index_occ), FALSE, index_occ)
    
    occ_key <- occ_work[index_occ == TRUE]
    num_records <- nrow(occ_key)
    
    if (num_records == 0) {
      print(r); print("table"); break
    }
    
    FAMILY__ <- FAMILY__recordNumber <- FAMILY_recordedBy_ <- FALSE
    sp_name <- ""
    
    # Análise da chave
    fam <- stringr::str_sub(r, 1, stringr::str_locate(r, "_")[1] - 1)
    fam <- ifelse(is.na(fam), "", fam)
    
    if (stringr::str_sub(r, stringr::str_count(r), stringr::str_count(r)) == "_" |
        grepl("__", r) |
        grepl("UNKNOWN-COLLECTOR", r)) {
      
      FAMILY__ <- grepl("__", r) & (stringr::str_locate(r, "__")[2] == stringr::str_count(r))
      FAMILY__ <- ifelse(is.na(FAMILY__), FALSE, FAMILY__)
      
      if (FAMILY__ == FALSE) {
        
        FAMILY_recordedBy_ <- (grepl("__", r) & (stringr::str_locate(r, "__")[2] != stringr::str_count(r))) |
          grepl("UNKNOWN-COLLECTOR", r)
        FAMILY_recordedBy_ <- ifelse(is.na(FAMILY_recordedBy_), FALSE, FAMILY_recordedBy_)
        
        if (FAMILY_recordedBy_ == FALSE) {
          FAMILY__recordNumber <- (
            stringr::str_sub(r, stringr::str_count(r), stringr::str_count(r)) == "_" &
              !stringr::str_sub(r, stringr::str_count(r) - 1, stringr::str_count(r) - 1) == "_"
          )
          FAMILY__recordNumber <- ifelse(is.na(FAMILY__recordNumber), FALSE, FAMILY__recordNumber)
        }
      }
    }
    
    # Caso não agrupável
    if (FAMILY__ == TRUE | FAMILY__recordNumber == TRUE | FAMILY_recordedBy_ == TRUE) {
      
      sp_name <- ifelse(occ_key$wcvp_taxon_status == "Accepted",
                        as.character(occ_key$wcvp_taxon_name),
                        "")
      sp_id <- ifelse(occ_key$wcvp_taxon_status == "Accepted",
                      as.character(occ_key$wcvp_plant_name_id),
                      "")
      
      status_txt <- ifelse(FAMILY__ == TRUE,
                           "not groupable: no recordedBy and no recordNumber",
                           ifelse(FAMILY__recordNumber == TRUE,
                                  "not groupable: no recordNumber ",
                                  ifelse(FAMILY_recordedBy_ == TRUE,
                                         "not groupable: no recordedBy",
                                         "not groupable")))
      
      occ_work[index_occ == TRUE, `:=`(
        parseGBIF_digital_voucher = TRUE,
        parseGBIF_non_groupable_duplicates = TRUE,
        parseGBIF_duplicates = FALSE,
        parseGBIF_num_duplicates = 1,
        parseGBIF_wcvp_plant_name_id = sp_id,
        parseGBIF_sample_taxon_name = sp_name,
        parseGBIF_unidentified_sample = ifelse(sp_name == "", TRUE, FALSE),
        parseGBIF_duplicates_grouping_status = status_txt,
        parseGBIF_number_taxon_names = ifelse(sp_name == "", 0, 1),
        parseGBIF_sample_taxon_name_status = ifelse(sp_name == "", "unidentified", "identified"),
        parseGBIF_decimalLatitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE, Ctrl_decimalLatitude, NA_real_),
        parseGBIF_decimalLongitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE, Ctrl_decimalLongitude, NA_real_),
        parseGBIF_useful_for_spatial_analysis = Ctrl_coordinates_validated_by_gbif_issue
      )]
      
      next
    }
    
    # Caso agrupável
    max_info <- max(occ_key$Ctrl_moreInformativeRecord)
    occ_work[index_occ == TRUE, `:=`(
      parseGBIF_duplicates_grouping_status = "groupable",
      parseGBIF_duplicates = (num_records > 1),
      parseGBIF_num_duplicates = num_records,
      parseGBIF_digital_voucher = (Ctrl_moreInformativeRecord == max_info)
    )]
    
    # Desempate
    if (sum(occ_work[index_occ == TRUE]$parseGBIF_digital_voucher) > 1) {
      dv <- occ_work[index_occ == TRUE]$parseGBIF_digital_voucher == TRUE
      n_tmp <- sum(dv)
      if (n_tmp == 1) {
        occ_work[index_occ == TRUE, parseGBIF_digital_voucher := fifelse(dv, TRUE, FALSE)]
      } else {
        pos <- which(dv)[1]
        tmp <- rep(FALSE, length(dv))
        tmp[pos] <- TRUE
        occ_work[index_occ == TRUE, parseGBIF_digital_voucher := tmp]
      }
    }
    
    # Refresh occ_key
    occ_key <- occ_work[index_occ == TRUE]
    occ_key[, wcvp_taxon_name_and_wcvp_plant_name_id := paste0(wcvp_taxon_name, ";", wcvp_plant_name_id)]
    
    # Determinação do nome
    if (!any(is.na(occ_key$wcvp_taxon_name) == FALSE)) {
      
      occ_work[index_occ == TRUE, `:=`(
        parseGBIF_number_taxon_names = 0,
        parseGBIF_wcvp_plant_name_id = "",
        parseGBIF_sample_taxon_name = "",
        parseGBIF_sample_taxon_name_status = "unidentified"
      )]
      
    } else {
      
      taxon_name_sample <- as.data.table(as.data.frame(
        table(occ_key$wcvp_taxon_name_and_wcvp_plant_name_id,
              occ_key$wcvp_taxon_status,
              exclude = NA),
        stringsAsFactors = FALSE
      ))
      setnames(taxon_name_sample, c("Var1", "Var2", "Freq"))
      
      taxon_name_sample <- taxon_name_sample[Freq > 0]
      setorder(taxon_name_sample, -Freq, Var1)
      
      num_taxon_name <- nrow(taxon_name_sample)
      if (num_taxon_name == 0) {
        print(occ_key$wcvp_taxon_name); print("0 - Error"); break
      }
      
      if (num_taxon_name == 1 & (taxon_name_sample$Var2[1] %in% c("Accepted"))) {
        sp_name_id <- strsplit(taxon_name_sample$Var1[1], ";", fixed = TRUE)[[1]]
        occ_work[index_occ == TRUE, `:=`(
          parseGBIF_number_taxon_names = num_taxon_name,
          parseGBIF_wcvp_plant_name_id = sp_name_id[2],
          parseGBIF_sample_taxon_name = sp_name_id[1],
          parseGBIF_sample_taxon_name_status = "identified",
          parseGBIF_unidentified_sample = FALSE
        )]
      }
      
      if (num_taxon_name > 1) {
        for (ii in 1:nrow(taxon_name_sample)) {
          if (taxon_name_sample$Var2[ii] %in% c("Accepted")) {
            sp_name_id <- strsplit(taxon_name_sample$Var1[ii], ";", fixed = TRUE)[[1]]
            occ_work[index_occ == TRUE, `:=`(
              parseGBIF_number_taxon_names = num_taxon_name,
              parseGBIF_wcvp_plant_name_id = sp_name_id[2],
              parseGBIF_sample_taxon_name = sp_name_id[1],
              parseGBIF_sample_taxon_name_status = "divergent identifications",
              parseGBIF_unidentified_sample = FALSE
            )]
            break
          }
        }
      }
    }
    
    # Determinação das coordenadas
    index_voucher <- occ_work[index_occ == TRUE]$parseGBIF_digital_voucher == TRUE
    
    if (occ_work[index_occ == TRUE]$Ctrl_coordinates_validated_by_gbif_issue[index_voucher == TRUE] == TRUE) {
      
      lat_v <- occ_work[index_occ == TRUE]$Ctrl_decimalLatitude[index_voucher == TRUE]
      lon_v <- occ_work[index_occ == TRUE]$Ctrl_decimalLongitude[index_voucher == TRUE]
      
      occ_work[index_occ == TRUE, `:=`(
        parseGBIF_decimalLatitude = lat_v,
        parseGBIF_decimalLongitude = lon_v,
        parseGBIF_useful_for_spatial_analysis = TRUE
      )]
      
    } else {
      
      idx_useful <- occ_work[index_occ == TRUE]$Ctrl_coordinates_validated_by_gbif_issue == TRUE
      
      if (sum(idx_useful) == 1) {
        lat_u <- occ_work[index_occ == TRUE]$Ctrl_decimalLatitude[idx_useful == TRUE]
        lon_u <- occ_work[index_occ == TRUE]$Ctrl_decimalLongitude[idx_useful == TRUE]
        occ_work[index_occ == TRUE, `:=`(
          parseGBIF_decimalLatitude = lat_u,
          parseGBIF_decimalLongitude = lon_u,
          parseGBIF_useful_for_spatial_analysis = TRUE
        )]
      }
      
      if (sum(idx_useful) > 1) {
        geospatial_quality_tmp <- max(occ_work[index_occ == TRUE]$Ctrl_geospatial_quality[idx_useful == TRUE])
        idx_gq <- occ_work[index_occ == TRUE]$Ctrl_geospatial_quality[idx_useful == TRUE] == geospatial_quality_tmp
        lat_u <- occ_work[index_occ == TRUE]$Ctrl_decimalLatitude[idx_useful == TRUE][idx_gq == TRUE][1]
        lon_u <- occ_work[index_occ == TRUE]$Ctrl_decimalLongitude[idx_useful == TRUE][idx_gq == TRUE][1]
        occ_work[index_occ == TRUE, `:=`(
          parseGBIF_decimalLatitude = lat_u,
          parseGBIF_decimalLongitude = lon_u,
          parseGBIF_useful_for_spatial_analysis = TRUE
        )]
      }
    }
  }
  
  # -------------------------
  # 6) occ_results
  # -------------------------
  occ_results <- occ_work[, .(
    Ctrl_geospatial_quality,
    Ctrl_verbatim_quality,
    Ctrl_moreInformativeRecord,
    parseGBIF_digital_voucher,
    parseGBIF_duplicates,
    parseGBIF_num_duplicates,
    parseGBIF_non_groupable_duplicates,
    parseGBIF_duplicates_grouping_status,
    Ctrl_coordinates_validated_by_gbif_issue,
    parseGBIF_unidentified_sample,
    parseGBIF_wcvp_plant_name_id,
    parseGBIF_sample_taxon_name,
    parseGBIF_sample_taxon_name_status,
    parseGBIF_number_taxon_names,
    parseGBIF_useful_for_spatial_analysis,
    parseGBIF_decimalLatitude,
    parseGBIF_decimalLongitude
  )]
  
  # -------------------------
  # 7) Reconstrói tabela completa
  # -------------------------
  full_df <- cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, occ_results)
  nms2 <- colnames(full_df)
  if (anyDuplicated(nms2)) colnames(full_df) <- make.unique(nms2, sep="__dup__")
  full_dt <- as.data.table(full_df)
  
  occ_in_2 <- full_dt[
    parseGBIF_digital_voucher == TRUE &
      parseGBIF_unidentified_sample == FALSE &
      parseGBIF_useful_for_spatial_analysis == TRUE
  ]
  occ_in_2[, parseGBIF_dataset_result := "useable"]
  
  occ_dup <- full_dt[parseGBIF_digital_voucher == FALSE]
  occ_dup[, parseGBIF_dataset_result := "duplicate"]
  
  occ_out_to_recover <- full_dt[
    parseGBIF_digital_voucher == TRUE &
      (parseGBIF_unidentified_sample == TRUE | parseGBIF_useful_for_spatial_analysis == FALSE)
  ]
  occ_out_to_recover[, parseGBIF_dataset_result := "unusable"]
  
  occ_all <- rbindlist(list(occ_in_2, occ_out_to_recover, occ_dup), use.names = TRUE, fill = TRUE)
  
  # -------------------------
  # 8) CORREÇÃO: merge com dados WCVP (baseado no ID)
  # -------------------------
  xn <- as.data.table(occ_wcvp_check_name)
  xn[, wcvp_plant_name_id := as.character(wcvp_plant_name_id)]
  
  # Remove duplicatas (mesmo procedimento da versão original)
  xn <- unique(xn, by = "wcvp_plant_name_id")
  
  # Seleciona as colunas desejadas
  xn <- xn[, .(
    wcvp_plant_name_id,
    wcvp_taxon_rank,
    wcvp_taxon_status,
    wcvp_family,
    wcvp_taxon_name,
    wcvp_taxon_authors,
    wcvp_reviewed
  )]
  
  # Renomeia com prefixo
  setnames(xn, names(xn), paste0("parseGBIF_", names(xn)))
  
  # Merge (left join) com occ_all pela chave parseGBIF_wcvp_plant_name_id
  occ_all <- merge(
    occ_all, xn,
    by.x = "parseGBIF_wcvp_plant_name_id",
    by.y = "parseGBIF_wcvp_plant_name_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  # -------------------------
  # 9) Seleciona colunas finais
  # -------------------------
  final_cols <- c(
    "Ctrl_gbifID",
    "Ctrl_bibliographicCitation",
    "Ctrl_language",
    "Ctrl_institutionCode",
    "Ctrl_collectionCode",
    "Ctrl_datasetName",
    "Ctrl_basisOfRecord",
    "Ctrl_catalogNumber",
    "Ctrl_recordNumber",
    "Ctrl_recordedBy",
    "Ctrl_georeferenceVerificationStatus",
    "Ctrl_occurrenceStatus",
    "Ctrl_eventDate",
    "Ctrl_year",
    "Ctrl_month",
    "Ctrl_day",
    "Ctrl_habitat",
    "Ctrl_fieldNotes",
    "Ctrl_eventRemarks",
    "Ctrl_locationID",
    "Ctrl_higherGeography",
    "Ctrl_islandGroup",
    "Ctrl_island",
    "Ctrl_countryCode",
    "Ctrl_stateProvince",
    "Ctrl_municipality",
    "Ctrl_county",
    "Ctrl_locality",
    "Ctrl_verbatimLocality",
    "Ctrl_locationRemarks",
    "Ctrl_level0Name",
    "Ctrl_level1Name",
    "Ctrl_level2Name",
    "Ctrl_level3Name",
    "Ctrl_identifiedBy",
    "Ctrl_dateIdentified",
    "Ctrl_scientificName",
    "Ctrl_decimalLatitude",
    "Ctrl_decimalLongitude",
    "Ctrl_identificationQualifier",
    "Ctrl_typeStatus",
    "Ctrl_family",
    "Ctrl_taxonRank",
    "Ctrl_issue",
    "Ctrl_nameRecordedBy_Standard",
    "Ctrl_recordNumber_Standard",
    "Ctrl_key_family_recordedBy_recordNumber",
    "Ctrl_geospatial_quality",
    "Ctrl_verbatim_quality",
    "Ctrl_moreInformativeRecord",
    "Ctrl_coordinates_validated_by_gbif_issue",
    "wcvp_plant_name_id",
    "wcvp_taxon_rank",
    "wcvp_taxon_status",
    "wcvp_family",
    "wcvp_taxon_name",
    "wcvp_taxon_authors",
    "wcvp_reviewed",
    "wcvp_searchedName",
    "wcvp_searchNotes",
    "parseGBIF_digital_voucher",
    "parseGBIF_duplicates",
    "parseGBIF_num_duplicates",
    "parseGBIF_non_groupable_duplicates",
    "parseGBIF_duplicates_grouping_status",
    "parseGBIF_unidentified_sample",
    "parseGBIF_sample_taxon_name",
    "parseGBIF_sample_taxon_name_status",
    "parseGBIF_number_taxon_names",
    "parseGBIF_useful_for_spatial_analysis",
    "parseGBIF_decimalLatitude",
    "parseGBIF_decimalLongitude",
    "parseGBIF_dataset_result",
    "parseGBIF_wcvp_plant_name_id",
    "parseGBIF_wcvp_taxon_rank",
    "parseGBIF_wcvp_taxon_status",
    "parseGBIF_wcvp_family",
    "parseGBIF_wcvp_taxon_name",
    "parseGBIF_wcvp_taxon_authors",
    "parseGBIF_wcvp_reviewed"
  )
  
  final_cols <- final_cols[!duplicated(final_cols)]
  
  miss <- setdiff(final_cols, names(occ_all))
  if (length(miss)) occ_all[, (miss) := NA]
  
  setcolorder(occ_all, final_cols)
  occ_all <- occ_all[, ..final_cols]
  
  return(list(
    occ_digital_voucher = occ_all,
    occ_results = occ_results
  ))
}
