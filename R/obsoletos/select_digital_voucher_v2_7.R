select_digital_voucher_v2_7 <- function(
    occ = NA,
    occ_gbif_issue = NA,
    occ_wcvp_check_name = NA,
    occ_collectorsDictionary = NA,
    enumOccurrenceIssue = NA,
    silence = TRUE
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  
  library(data.table)
  library(stringr)
  
  # -----------------------------
  # EnumOccurrenceIssue (igual)
  # -----------------------------
  if (is.na(enumOccurrenceIssue)[1]) {
    data(EnumOccurrenceIssue, envir = environment())
  } else {
    EnumOccurrenceIssue <- enumOccurrenceIssue
  }
  
  occ_in <- occ
  
  # -----------------------------
  # 0) cbind 1x (paridade) + nomes únicos (paridade de colisoes)
  # -----------------------------
  occ_cb <- cbind(occ_gbif_issue, occ_in, occ_wcvp_check_name, occ_collectorsDictionary)
  names(occ_cb) <- make.unique(names(occ_cb), sep = ".")
  
  # Converte uma vez para data.table (sem mutate em cascata)
  dt_full <- as.data.table(occ_cb)
  
  # NA -> '' (igual)
  if ("wcvp_taxon_rank" %in% names(dt_full)) {
    dt_full[is.na(wcvp_taxon_rank), wcvp_taxon_rank := ""]
  }
  if ("wcvp_taxon_status" %in% names(dt_full)) {
    dt_full[is.na(wcvp_taxon_status), wcvp_taxon_status := ""]
  }
  
  # -----------------------------
  # 1) índices geoespaciais (igual)
  # -----------------------------
  index_tmp1 <- EnumOccurrenceIssue$score == 1 & EnumOccurrenceIssue$type == "geospatial"
  index_tmp2 <- EnumOccurrenceIssue$score == 2 & EnumOccurrenceIssue$type == "geospatial"
  index_tmp3 <- EnumOccurrenceIssue$score == 3 & EnumOccurrenceIssue$type == "geospatial"
  index_tmp1[is.na(index_tmp1)] <- FALSE
  index_tmp2[is.na(index_tmp2)] <- FALSE
  index_tmp3[is.na(index_tmp3)] <- FALSE
  
  cols1 <- EnumOccurrenceIssue$constant[index_tmp1]
  cols2 <- EnumOccurrenceIssue$constant[index_tmp2]
  cols3 <- EnumOccurrenceIssue$constant[index_tmp3]
  
  # Só as colunas que existem de fato
  cols1 <- cols1[cols1 %in% names(dt_full)]
  cols2 <- cols2[cols2 %in% names(dt_full)]
  cols3 <- cols3[cols3 %in% names(dt_full)]
  
  # -----------------------------
  # 2) Flags verbatim (igual à sua mutate)
  # -----------------------------
  dt_full[, temAnoColeta :=
            ifelse(is.na(Ctrl_year) | Ctrl_year == "" | Ctrl_year == 0 | Ctrl_year <= 10,
                   FALSE, TRUE)]
  dt_full[is.na(temAnoColeta), temAnoColeta := FALSE]
  
  dt_full[, temCodigoInstituicao := ifelse(is.na(Ctrl_institutionCode) | Ctrl_institutionCode == "", FALSE, TRUE)]
  dt_full[is.na(temCodigoInstituicao), temCodigoInstituicao := FALSE]
  
  dt_full[, temNumeroCatalogo := ifelse(is.na(Ctrl_catalogNumber) | Ctrl_catalogNumber == "", FALSE, TRUE)]
  dt_full[is.na(temNumeroCatalogo), temNumeroCatalogo := FALSE]
  
  dt_full[, temColetor := ifelse(is.na(Ctrl_recordedBy) | Ctrl_recordedBy == "", FALSE, TRUE)]
  dt_full[is.na(temColetor), temColetor := FALSE]
  
  dt_full[, temNumeroColeta := ifelse(is.na(Ctrl_recordNumber) | Ctrl_recordNumber == "", FALSE, TRUE)]
  dt_full[is.na(temNumeroColeta), temNumeroColeta := FALSE]
  
  # temPais (igual): ifelse(COUNTRY_INVALID==TRUE, FALSE, TRUE) e NA->FALSE
  if ("COUNTRY_INVALID" %in% names(dt_full)) {
    dt_full[, temPais := ifelse(COUNTRY_INVALID == TRUE, FALSE, TRUE)]
    dt_full[is.na(temPais), temPais := FALSE]
  } else {
    # se não existir, a original quebraria; aqui mantemos TRUE
    dt_full[, temPais := TRUE]
  }
  
  dt_full[, temUF := ifelse(is.na(Ctrl_stateProvince) | Ctrl_stateProvince == "", FALSE, TRUE)]
  dt_full[is.na(temUF), temUF := FALSE]
  
  dt_full[, temMunicipio := ifelse(is.na(Ctrl_municipality) | Ctrl_municipality == "", FALSE, TRUE)]
  dt_full[is.na(temMunicipio), temMunicipio := FALSE]
  
  dt_full[, temLocalidade := ifelse(is.na(Ctrl_locality) | Ctrl_locality == "", FALSE, TRUE)]
  dt_full[is.na(temLocalidade), temLocalidade := FALSE]
  
  dt_full[, temNotas := ifelse(is.na(Ctrl_fieldNotes) | Ctrl_fieldNotes == "", FALSE, TRUE)]
  dt_full[is.na(temNotas), temNotas := FALSE]
  
  # -----------------------------
  # 3) Inicialização (igual)
  # -----------------------------
  dt_full[, `:=`(
    Ctrl_geospatial_quality = 0L,
    Ctrl_verbatim_quality = 0L,
    Ctrl_moreInformativeRecord = 0L,
    parseGBIF_digital_voucher = FALSE,
    parseGBIF_duplicates = FALSE,
    parseGBIF_non_groupable_duplicates = FALSE,
    parseGBIF_num_duplicates = 0L,
    parseGBIF_duplicates_grouping_status = "",
    Ctrl_coordinates_validated_by_gbif_issue = FALSE
  )]
  
  # -----------------------------
  # 4) Ctrl_coordinates_validated_by_gbif_issue (igual rowSums do original)
  #    IMPORTANTE: usa rowSums(data.frame subset) como na original -> coerções iguais
  # -----------------------------
  if (length(cols3) > 0) {
    s3 <- rowSums(as.data.frame(dt_full[, ..cols3]))
    dt_full[, Ctrl_coordinates_validated_by_gbif_issue := ifelse(s3 == 0, TRUE, FALSE)]
  } else {
    dt_full[, Ctrl_coordinates_validated_by_gbif_issue := TRUE]
  }
  
  dt_full[, Ctrl_coordinates_validated_by_gbif_issue :=
            ifelse(Ctrl_hasCoordinate == FALSE | Ctrl_decimalLatitude == 0 | Ctrl_decimalLongitude == 0,
                   FALSE, Ctrl_coordinates_validated_by_gbif_issue)]
  
  dt_full[, Ctrl_coordinates_validated_by_gbif_issue :=
            ifelse(is.na(Ctrl_coordinates_validated_by_gbif_issue), FALSE, Ctrl_coordinates_validated_by_gbif_issue)]
  
  # Ctrl_geospatial_quality (igual ao encadeado)
  s3p <- if (length(cols3) > 0) rowSums(as.data.frame(dt_full[, ..cols3])) else rep(0, nrow(dt_full))
  s2p <- if (length(cols2) > 0) rowSums(as.data.frame(dt_full[, ..cols2])) else rep(0, nrow(dt_full))
  s1p <- if (length(cols1) > 0) rowSums(as.data.frame(dt_full[, ..cols1])) else rep(0, nrow(dt_full))
  
  dt_full[, Ctrl_geospatial_quality :=
            ifelse(s3p > 0, -9L,
                   ifelse(s2p > 0, -3L,
                          ifelse(s1p > 0, -1L, 0L)))]
  
  dt_full[, Ctrl_geospatial_quality :=
            ifelse(Ctrl_hasCoordinate == FALSE, -9L, Ctrl_geospatial_quality)]
  
  # Ctrl_verbatim_quality / Ctrl_moreInformativeRecord (igual)
  dt_full[, Ctrl_verbatim_quality :=
            (temColetor + temNumeroColeta + temAnoColeta + temCodigoInstituicao +
               temNumeroCatalogo + temLocalidade + temMunicipio + temUF + temPais + temNotas)]
  
  dt_full[, Ctrl_moreInformativeRecord := (Ctrl_geospatial_quality + Ctrl_verbatim_quality)]
  
  # -----------------------------
  # 5) Reduz para as colunas do “miolo” (igual ao seu select no meio)
  # -----------------------------
  dt <- dt_full[, .(
    Ctrl_key_family_recordedBy_recordNumber,
    wcvp_plant_name_id,
    wcvp_taxon_name,
    wcvp_taxon_status,
    wcvp_searchNotes,
    Ctrl_taxonRank,
    Ctrl_geospatial_quality,
    Ctrl_verbatim_quality,
    Ctrl_moreInformativeRecord,
    parseGBIF_digital_voucher,
    parseGBIF_duplicates,
    parseGBIF_num_duplicates,
    parseGBIF_non_groupable_duplicates,
    parseGBIF_duplicates_grouping_status,
    Ctrl_coordinates_validated_by_gbif_issue,
    Ctrl_decimalLatitude,
    Ctrl_decimalLongitude
  )]
  
  dt[, `:=`(
    parseGBIF_unidentified_sample = TRUE,
    parseGBIF_wcvp_plant_name_id = "",
    parseGBIF_sample_taxon_name = "",
    parseGBIF_sample_taxon_name_status = "",
    parseGBIF_number_taxon_names = 0L,
    parseGBIF_useful_for_spatial_analysis = FALSE,
    parseGBIF_decimalLatitude = as.numeric(NA),
    parseGBIF_decimalLongitude = as.numeric(NA)
  )]
  
  # -----------------------------
  # 6) Ajuste da chave "_NA" (EXATAMENTE como original)
  # original: se últimos 3 chars == "_NA" => remove 2 chars finais (fica sem "NA", mantém "_")
  # -----------------------------
  idx_na <- str_sub(dt$Ctrl_key_family_recordedBy_recordNumber,
                    str_count(dt$Ctrl_key_family_recordedBy_recordNumber) - 2,
                    str_count(dt$Ctrl_key_family_recordedBy_recordNumber)) %in% "_NA"
  if (any(idx_na, na.rm = TRUE)) {
    dt$Ctrl_key_family_recordedBy_recordNumber[idx_na == TRUE] <-
      str_sub(dt$Ctrl_key_family_recordedBy_recordNumber[idx_na == TRUE], 1,
              str_count(dt$Ctrl_key_family_recordedBy_recordNumber[idx_na == TRUE]) - 2)
  }
  
  # -----------------------------
  # 7) Troca do gargalo: agrupa sem for+%in%
  #    Mantém a ORDEM do unique() (paridade)
  # -----------------------------
  keys_in_order <- unique(dt$Ctrl_key_family_recordedBy_recordNumber)
  dt[, gid__ := match(Ctrl_key_family_recordedBy_recordNumber, keys_in_order)]
  
  tot <- length(keys_in_order)
  s <- 0L
  
  dt[, {
    s <<- s + 1L
    if (!silence && (s %% 100L == 0L)) print(paste0(s, " de ", tot))
    
    r <- Ctrl_key_family_recordedBy_recordNumber[1]
    num_records <- .N
    
    FAMILY__ <- FAMILY__recordNumber <- FAMILY_recordedBy_ <- FALSE
    
    if (str_sub(r, str_count(r), str_count(r)) == "_" |
        grepl("__", r) |
        grepl("UNKNOWN-COLLECTOR", r)) {
      
      FAMILY__ <- (grepl("__", r) & str_locate(r, "__")[2] == str_count(r)) %>%
        ifelse(is.na(.), FALSE, .)
      
      if (FAMILY__ == FALSE) {
        
        FAMILY_recordedBy_ <- ((grepl("__", r) & str_locate(r, "__")[2] != str_count(r)) |
                                 grepl("UNKNOWN-COLLECTOR", r)) %>%
          ifelse(is.na(.), FALSE, .)
        
        if (FAMILY_recordedBy_ == FALSE) {
          FAMILY__recordNumber <- ((str_sub(r, str_count(r), str_count(r)) == "_") &
                                     !str_sub(r, str_count(r) - 1, str_count(r) - 1) == "_") %>%
            ifelse(is.na(.), FALSE, .)
        }
      }
    }
    
    # ---- non-groupable (igual)
    if (FAMILY__ == TRUE | FAMILY__recordNumber == TRUE | FAMILY_recordedBy_ == TRUE) {
      
      sp_name <- ifelse(wcvp_taxon_status == "Accepted", as.character(wcvp_taxon_name), "")
      sp_id   <- ifelse(wcvp_taxon_status == "Accepted", as.character(wcvp_plant_name_id), "")
      
      list(
        parseGBIF_digital_voucher = rep(TRUE, num_records),
        parseGBIF_non_groupable_duplicates = rep(TRUE, num_records),
        parseGBIF_duplicates = rep(FALSE, num_records),
        parseGBIF_num_duplicates = rep(1L, num_records),
        
        parseGBIF_wcvp_plant_name_id = sp_id,
        parseGBIF_sample_taxon_name = sp_name,
        parseGBIF_unidentified_sample = ifelse(sp_name == "", TRUE, FALSE),
        
        parseGBIF_duplicates_grouping_status =
          rep(ifelse(FAMILY__ == TRUE,
                     "not groupable: no recordedBy and no recordNumber",
                     ifelse(FAMILY__recordNumber == TRUE,
                            "not groupable: no recordNumber ",
                            ifelse(FAMILY_recordedBy_ == TRUE,
                                   "not groupable: no recordedBy",
                                   "not groupable"))),
              num_records),
        
        parseGBIF_number_taxon_names = ifelse(sp_name == "", 0L, 1L),
        parseGBIF_sample_taxon_name_status = ifelse(sp_name == "", "unidentified", "identified"),
        
        parseGBIF_decimalLatitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE, Ctrl_decimalLatitude, NA_real_),
        parseGBIF_decimalLongitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE, Ctrl_decimalLongitude, NA_real_),
        parseGBIF_useful_for_spatial_analysis = Ctrl_coordinates_validated_by_gbif_issue
      )
      
    } else {
      
      # ---- groupable: voucher (paridade do original: == max e depois zera extras mantendo o primeiro)
      dv <- (Ctrl_moreInformativeRecord == max(Ctrl_moreInformativeRecord))
      if (sum(dv) > 1) {
        idx_true <- which(dv)
        if (length(idx_true) > 1) dv[idx_true[-1]] <- FALSE
      }
      
      # flags de duplicata
      dup_flag <- (num_records > 1)
      
      # nomes / ids (igual)
      wcvp_taxon_name_and_id <- paste0(wcvp_taxon_name, ";", wcvp_plant_name_id)
      
      if (!any(is.na(wcvp_taxon_name) == FALSE)) {
        p_num <- rep(0L, num_records)
        p_id  <- rep("", num_records)
        p_nm  <- rep("", num_records)
        p_st  <- rep("unidentified", num_records)
        p_un  <- rep(TRUE, num_records)
      } else {
        tax <- table(wcvp_taxon_name_and_id, wcvp_taxon_status, exclude = NA) |>
          as.data.frame() |>
          (\(x) x[x$Freq > 0, , drop = FALSE])() |>
          (\(x) x[order(-x$Freq, x$Var1), , drop = FALSE])()
        
        num_tax <- NROW(tax)
        
        if (num_tax == 0) {
          p_num <- rep(0L, num_records)
          p_id  <- rep("", num_records)
          p_nm  <- rep("", num_records)
          p_st  <- rep("unidentified", num_records)
          p_un  <- rep(TRUE, num_records)
        } else if (num_tax == 1 && tax$Var2[1] %in% c("Accepted")) {
          sp <- str_split(tax$Var1[1], ";", simplify = TRUE)
          p_num <- rep(num_tax, num_records)
          p_id  <- rep(sp[,2], num_records)
          p_nm  <- rep(sp[,1], num_records)
          p_st  <- rep("identified", num_records)
          p_un  <- rep(FALSE, num_records)
        } else if (num_tax > 1) {
          chosen <- NA_integer_
          for (ii in seq_len(num_tax)) {
            if (tax$Var2[ii] %in% c("Accepted")) { chosen <- ii; break }
          }
          if (!is.na(chosen)) {
            sp <- str_split(tax$Var1[chosen], ";", simplify = TRUE)
            p_num <- rep(num_tax, num_records)
            p_id  <- rep(sp[,2], num_records)
            p_nm  <- rep(sp[,1], num_records)
            p_st  <- rep("divergent identifications", num_records)
            p_un  <- rep(FALSE, num_records)
          } else {
            p_num <- rep(num_tax, num_records)
            p_id  <- rep("", num_records)
            p_nm  <- rep("", num_records)
            p_st  <- rep("unidentified", num_records)
            p_un  <- rep(TRUE, num_records)
          }
        } else {
          p_num <- rep(num_tax, num_records)
          p_id  <- rep("", num_records)
          p_nm  <- rep("", num_records)
          p_st  <- rep("unidentified", num_records)
          p_un  <- rep(TRUE, num_records)
        }
      }
      
      # coordenadas (igual)
      idx_voucher <- which(dv)[1]
      
      use_lat <- NA_real_
      use_lon <- NA_real_
      use_spa <- FALSE
      
      if (isTRUE(Ctrl_coordinates_validated_by_gbif_issue[idx_voucher] == TRUE)) {
        use_lat <- Ctrl_decimalLatitude[idx_voucher]
        use_lon <- Ctrl_decimalLongitude[idx_voucher]
        use_spa <- TRUE
      } else {
        idx_ok <- which(Ctrl_coordinates_validated_by_gbif_issue == TRUE)
        if (length(idx_ok) == 1) {
          use_lat <- Ctrl_decimalLatitude[idx_ok]
          use_lon <- Ctrl_decimalLongitude[idx_ok]
          use_spa <- TRUE
        } else if (length(idx_ok) > 1) {
          gbest <- max(Ctrl_geospatial_quality[idx_ok])
          idx_best <- idx_ok[which(Ctrl_geospatial_quality[idx_ok] == gbest)]
          use_lat <- Ctrl_decimalLatitude[idx_best[1]]
          use_lon <- Ctrl_decimalLongitude[idx_best[1]]
          use_spa <- TRUE
        }
      }
      
      list(
        parseGBIF_duplicates_grouping_status = rep("groupable", num_records),
        parseGBIF_duplicates = rep(dup_flag, num_records),
        parseGBIF_num_duplicates = rep(as.integer(num_records), num_records),
        parseGBIF_digital_voucher = dv,
        
        parseGBIF_number_taxon_names = p_num,
        parseGBIF_wcvp_plant_name_id = p_id,
        parseGBIF_sample_taxon_name = p_nm,
        parseGBIF_sample_taxon_name_status = p_st,
        parseGBIF_unidentified_sample = p_un,
        
        parseGBIF_decimalLatitude = rep(use_lat, num_records),
        parseGBIF_decimalLongitude = rep(use_lon, num_records),
        parseGBIF_useful_for_spatial_analysis = rep(use_spa, num_records)
      )
    }
    
  }, by = gid__]
  
  dt[, gid__ := NULL]
  
  # -----------------------------
  # 8) occ_results (igual ao select final da original)
  # -----------------------------
  occ_results <- dt[, .(
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
  
  # -----------------------------
  # 9) occ_all (paridade do final da original)
  # -----------------------------
  # Reconstroi como o original: cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, occ_results)
  occ_all_base <- as.data.table(cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, as.data.frame(occ_results)))
  names(occ_all_base) <- make.unique(names(occ_all_base), sep = ".")
  
  occ_in_2 <- occ_all_base[
    parseGBIF_digital_voucher == TRUE &
      parseGBIF_unidentified_sample == FALSE &
      parseGBIF_useful_for_spatial_analysis == TRUE
  ]
  occ_dup <- occ_all_base[parseGBIF_digital_voucher == FALSE]
  occ_out_to_recover <- occ_all_base[
    parseGBIF_digital_voucher == TRUE &
      (parseGBIF_unidentified_sample == TRUE | parseGBIF_useful_for_spatial_analysis == FALSE)
  ]
  
  occ_in_2[, parseGBIF_dataset_result := "useable"]
  occ_out_to_recover[, parseGBIF_dataset_result := "unusable"]
  occ_dup[, parseGBIF_dataset_result := "duplicate"]
  
  occ_all <- rbindlist(list(occ_in_2, occ_out_to_recover, occ_dup), use.names = TRUE, fill = TRUE)
  
  # xn (igual)
  xn <- as.data.table(occ_wcvp_check_name)
  xn[, wcvp_plant_name_id := as.character(wcvp_plant_name_id)]
  xn <- unique(xn[, .(
    wcvp_plant_name_id,
    wcvp_taxon_rank,
    wcvp_taxon_status,
    wcvp_family,
    wcvp_taxon_name,
    wcvp_taxon_authors,
    wcvp_reviewed
  )])
  setnames(xn, old = names(xn), new = paste0("parseGBIF_", names(xn)))
  
  # left_join (paridade)
  if ("parseGBIF_wcvp_plant_name_id" %in% names(occ_all) && "parseGBIF_wcvp_plant_name_id" %in% names(xn)) {
    occ_all <- merge(occ_all, xn, by = "parseGBIF_wcvp_plant_name_id", all.x = TRUE, sort = FALSE)
  }
  
  # seleção final de colunas (igual ao seu select; mantém apenas as que existirem)
  final_cols <- c(
    "Ctrl_gbifID","Ctrl_bibliographicCitation","Ctrl_language","Ctrl_institutionCode","Ctrl_collectionCode",
    "Ctrl_datasetName","Ctrl_basisOfRecord","Ctrl_catalogNumber","Ctrl_recordNumber","Ctrl_recordedBy",
    "Ctrl_georeferenceVerificationStatus","Ctrl_occurrenceStatus","Ctrl_eventDate","Ctrl_year","Ctrl_month","Ctrl_day",
    "Ctrl_habitat","Ctrl_fieldNotes","Ctrl_eventRemarks","Ctrl_locationID","Ctrl_higherGeography","Ctrl_islandGroup","Ctrl_island",
    "Ctrl_countryCode","Ctrl_stateProvince","Ctrl_municipality","Ctrl_county","Ctrl_locality","Ctrl_verbatimLocality","Ctrl_locationRemarks",
    "Ctrl_level0Name","Ctrl_level1Name","Ctrl_level2Name","Ctrl_level3Name","Ctrl_identifiedBy","Ctrl_dateIdentified","Ctrl_scientificName",
    "Ctrl_decimalLatitude","Ctrl_decimalLongitude","Ctrl_identificationQualifier",
    "Ctrl_typeStatus","Ctrl_identifiedBy","Ctrl_dateIdentified","Ctrl_scientificName","Ctrl_family","Ctrl_taxonRank","Ctrl_issue",
    "Ctrl_nameRecordedBy_Standard","Ctrl_recordNumber_Standard","Ctrl_key_family_recordedBy_recordNumber",
    "Ctrl_geospatial_quality","Ctrl_verbatim_quality","Ctrl_moreInformativeRecord","Ctrl_coordinates_validated_by_gbif_issue",
    "wcvp_plant_name_id","wcvp_taxon_rank","wcvp_taxon_status","wcvp_family","wcvp_taxon_name","wcvp_taxon_authors","wcvp_reviewed",
    "wcvp_searchedName","wcvp_searchNotes",
    "parseGBIF_digital_voucher","parseGBIF_duplicates","parseGBIF_num_duplicates","parseGBIF_non_groupable_duplicates",
    "parseGBIF_duplicates_grouping_status","parseGBIF_unidentified_sample","parseGBIF_sample_taxon_name",
    "parseGBIF_sample_taxon_name_status","parseGBIF_number_taxon_names","parseGBIF_useful_for_spatial_analysis",
    "parseGBIF_decimalLatitude","parseGBIF_decimalLongitude","parseGBIF_dataset_result",
    "parseGBIF_wcvp_plant_name_id","parseGBIF_wcvp_taxon_rank","parseGBIF_wcvp_taxon_status","parseGBIF_wcvp_family",
    "parseGBIF_wcvp_taxon_name","parseGBIF_wcvp_taxon_authors","parseGBIF_wcvp_reviewed"
  )
  final_cols <- final_cols[final_cols %in% names(occ_all)]
  occ_all <- occ_all[, ..final_cols]
  
  list(
    occ_digital_voucher = as.data.frame(occ_all),
    occ_results = as.data.frame(occ_results)
  )
}
