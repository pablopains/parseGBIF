select_digital_voucher_optimized <- function(occ, occ_gbif_issue, occ_wcvp_check_name, 
                                         occ_collectorsDictionary, enumOccurrenceIssue = NA, 
                                         silence = TRUE) {
  
  # 1. PREPARAÇÃO
  if (is.na(enumOccurrenceIssue)) {
    data(EnumOccurrenceIssue, envir = environment())
  }
  
  # Preservar ordem original
  occ_dt <- as.data.table(occ)
  occ_dt[, .orig_id := .I]
  
  occ_gbif_issue <- as.data.table(occ_gbif_issue)
  occ_wcvp_check_name <- as.data.table(occ_wcvp_check_name)
  occ_collectorsDictionary <- as.data.table(occ_collectorsDictionary)
  
  # Combinação idêntica
  occ_dt <- cbind(occ_gbif_issue, occ_dt, occ_wcvp_check_name, occ_collectorsDictionary)
  
  # 2. FLAGS DE QUALIDADE
  occ_dt[, `:=`(
    wcvp_taxon_rank = as.character(fcoalesce(wcvp_taxon_rank, "")),
    wcvp_taxon_status = as.character(fcoalesce(wcvp_taxon_status, "")),
    
    # Flags booleanas
    temAnoColeta = !is.na(Ctrl_year) & Ctrl_year > 10,
    temCodigoInstituicao = !is.na(Ctrl_institutionCode) & Ctrl_institutionCode != "",
    temNumeroCatalogo = !is.na(Ctrl_catalogNumber) & Ctrl_catalogNumber != "",
    temColetor = !is.na(Ctrl_recordedBy) & Ctrl_recordedBy != "",
    temNumeroColeta = !is.na(Ctrl_recordNumber) & Ctrl_recordNumber != "",
    temPais = !fcoalesce(COUNTRY_INVALID, FALSE),
    temUF = !is.na(Ctrl_stateProvince) & Ctrl_stateProvince != "",
    temMunicipio = !is.na(Ctrl_municipality) & Ctrl_municipality != "",
    temLocalidade = !is.na(Ctrl_locality) & Ctrl_locality != "",
    temNotas = !is.na(Ctrl_fieldNotes) & Ctrl_fieldNotes != "",
    
    # Inicializações idênticas à original
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
  
  # 3. QUALIDADE GEOSPATIAL (CORREÇÃO CRÍTICA)
  issues_geo <- EnumOccurrenceIssue[type == "geospatial"]
  i3 <- issues_geo[score == 3, constant]
  i2 <- issues_geo[score == 2, constant]
  i1 <- issues_geo[score == 1, constant]
  
  # Verificar issues
  if(length(i3) > 0) {
    occ_dt[, has_i3 := rowSums(.SD, na.rm = TRUE) > 0, .SDcols = i3]
  } else {
    occ_dt[, has_i3 := FALSE]
  }
  
  if(length(i2) > 0) {
    occ_dt[, has_i2 := rowSums(.SD, na.rm = TRUE) > 0, .SDcols = i2]
  } else {
    occ_dt[, has_i2 := FALSE]
  }
  
  if(length(i1) > 0) {
    occ_dt[, has_i1 := rowSums(.SD, na.rm = TRUE) > 0, .SDcols = i1]
  } else {
    occ_dt[, has_i1 := FALSE]
  }
  
  # Calcular qualidade geospatial (IDÊNTICO À ORIGINAL)
  occ_dt[, Ctrl_geospatial_quality := fifelse(
    has_i3 | !Ctrl_hasCoordinate | 
      is.na(Ctrl_decimalLatitude) | Ctrl_decimalLatitude == 0 |
      is.na(Ctrl_decimalLongitude) | Ctrl_decimalLongitude == 0, -9L,
    fifelse(has_i2, -3L,
            fifelse(has_i1, -1L, 0L)))
  ]
  
  # Coordenadas validadas (EXATAMENTE COMO NA ORIGINAL)
  occ_dt[, Ctrl_coordinates_validated_by_gbif_issue := 
           Ctrl_hasCoordinate & 
           !is.na(Ctrl_decimalLatitude) & Ctrl_decimalLatitude != 0 &
           !is.na(Ctrl_decimalLongitude) & Ctrl_decimalLongitude != 0 &
           !has_i3]
  
  # 4. SCORES TOTAIS
  occ_dt[, Ctrl_verbatim_quality := 
           as.integer(temColetor) + as.integer(temNumeroColeta) + 
           as.integer(temAnoColeta) + as.integer(temCodigoInstituicao) + 
           as.integer(temNumeroCatalogo) + as.integer(temLocalidade) + 
           as.integer(temMunicipio) + as.integer(temUF) + 
           as.integer(temPais) + as.integer(temNotas)]
  
  occ_dt[, Ctrl_moreInformativeRecord := Ctrl_geospatial_quality + Ctrl_verbatim_quality]
  
  # 5. LIMPEZA DE CHAVE
  occ_dt[, key_clean := sub("_NA$", "", Ctrl_key_family_recordedBy_recordNumber)]
  
  # 6. STATUS DE AGRUPAMENTO
  occ_dt[, parseGBIF_duplicates_grouping_status := 
           as.character(fifelse(
             grepl("__$", key_clean), 
             "not groupable: no recordedBy and no recordNumber",
             fifelse(grepl("UNKNOWN-COLLECTOR", key_clean),
                     "not groupable: no recordedBy",
                     fifelse(grepl("_$", key_clean) & !grepl("__", key_clean),
                             "not groupable: no recordNumber", 
                             "groupable"))))
  ]
  
  # 7. PROCESSAMENTO POR GRUPO
  # Inicializar
  occ_dt[, `:=`(
    parseGBIF_num_duplicates = 0L,
    parseGBIF_duplicates = FALSE,
    parseGBIF_non_groupable_duplicates = FALSE,
    parseGBIF_digital_voucher = FALSE
  )]
  
  # Agrupáveis (com proteção contra NAs)
  occ_dt[parseGBIF_duplicates_grouping_status == "groupable",
         `:=`(
           parseGBIF_num_duplicates = .N,
           parseGBIF_duplicates = .N > 1,
           parseGBIF_digital_voucher = {
             scores <- Ctrl_moreInformativeRecord
             scores[is.na(scores)] <- -Inf
             (frank(-scores, ties.method = "first") == 1)
           }
         ), by = key_clean]
  
  # Não-agrupáveis (idêntico à original)
  occ_dt[parseGBIF_duplicates_grouping_status != "groupable",
         `:=`(
           parseGBIF_num_duplicates = 1L,
           parseGBIF_non_groupable_duplicates = TRUE,
           parseGBIF_digital_voucher = TRUE
         )]
  
  # 8. TAXONOMIA
  # Inicializar
  occ_dt[, `:=`(
    parseGBIF_number_taxon_names = 0L,
    parseGBIF_sample_taxon_name = "",
    parseGBIF_wcvp_plant_name_id = "",
    parseGBIF_unidentified_sample = TRUE
  )]
  
  # Agrupáveis
  occ_dt[parseGBIF_duplicates_grouping_status == "groupable",
         c("parseGBIF_number_taxon_names", 
           "parseGBIF_sample_taxon_name",
           "parseGBIF_wcvp_plant_name_id",
           "parseGBIF_unidentified_sample") := {
             
             accepted_names <- unique(wcvp_taxon_name[
               wcvp_taxon_status == "Accepted" & 
                 wcvp_taxon_name != "" & !is.na(wcvp_taxon_name)
             ])
             
             accepted_ids <- unique(wcvp_plant_name_id[
               wcvp_taxon_status == "Accepted" & 
                 wcvp_plant_name_id != "" & !is.na(wcvp_plant_name_id)
             ])
             
             n_names <- length(accepted_names)
             
             .(as.integer(n_names),
               if(n_names > 0) as.character(accepted_names[1]) else "",
               if(n_names > 0) as.character(accepted_ids[1]) else "",
               n_names == 0)
           }, by = key_clean]
  
  # Não-agrupáveis
  occ_dt[parseGBIF_duplicates_grouping_status != "groupable",
         `:=`(
           parseGBIF_number_taxon_names = fifelse(
             wcvp_taxon_status == "Accepted" & 
               wcvp_taxon_name != "" & !is.na(wcvp_taxon_name), 1L, 0L),
           parseGBIF_sample_taxon_name = fifelse(
             wcvp_taxon_status == "Accepted", 
             as.character(wcvp_taxon_name), ""),
           parseGBIF_wcvp_plant_name_id = fifelse(
             wcvp_taxon_status == "Accepted", 
             as.character(wcvp_plant_name_id), ""),
           parseGBIF_unidentified_sample = (wcvp_taxon_status != "Accepted")
         )]
  
  # Status
  occ_dt[, parseGBIF_sample_taxon_name_status := 
           as.character(fifelse(
             parseGBIF_number_taxon_names == 0, "unidentified",
             fifelse(parseGBIF_number_taxon_names == 1, "identified",
                     "divergent identifications")))
  ]
  
  # 9. COORDENADAS (com segurança)
  occ_dt[, `:=`(
    # Coordenadas do voucher (com verificação)
    v_lat = if(any(parseGBIF_digital_voucher, na.rm = TRUE)) 
      Ctrl_decimalLatitude[which(parseGBIF_digital_voucher)[1]] else NA_real_,
    v_lon = if(any(parseGBIF_digital_voucher, na.rm = TRUE)) 
      Ctrl_decimalLongitude[which(parseGBIF_digital_voucher)[1]] else NA_real_,
    v_val = if(any(parseGBIF_digital_voucher, na.rm = TRUE)) 
      Ctrl_coordinates_validated_by_gbif_issue[which(parseGBIF_digital_voucher)[1]] else FALSE,
    
    # Melhor alternativa
    best_alt_lat = {
      valid_idx <- which(Ctrl_coordinates_validated_by_gbif_issue)
      if(length(valid_idx) > 0) {
        best_idx <- valid_idx[which.max(Ctrl_geospatial_quality[valid_idx])]
        Ctrl_decimalLatitude[best_idx]
      } else NA_real_
    },
    best_alt_lon = {
      valid_idx <- which(Ctrl_coordinates_validated_by_gbif_issue)
      if(length(valid_idx) > 0) {
        best_idx <- valid_idx[which.max(Ctrl_geospatial_quality[valid_idx])]
        Ctrl_decimalLongitude[best_idx]
      } else NA_real_
    }
  ), by = key_clean]
  
  # Atribuir coordenadas finais
  occ_dt[, `:=`(
    parseGBIF_decimalLatitude = as.numeric(fifelse(v_val, v_lat, best_alt_lat)),
    parseGBIF_decimalLongitude = as.numeric(fifelse(v_val, v_lon, best_alt_lon)),
    parseGBIF_useful_for_spatial_analysis = 
      !is.na(parseGBIF_decimalLatitude) & !is.na(parseGBIF_decimalLongitude)
  )]
  
  # 10. CLASSIFICAÇÃO FINAL
  occ_dt[, parseGBIF_dataset_result := 
           as.character(fifelse(
             parseGBIF_digital_voucher & 
               !parseGBIF_unidentified_sample & 
               parseGBIF_useful_for_spatial_analysis, 
             "useable",
             fifelse(parseGBIF_digital_voucher & 
                       (parseGBIF_unidentified_sample | 
                          !parseGBIF_useful_for_spatial_analysis), 
                     "unusable", 
                     "duplicate")))
  ]
  
  # 11. REORDENAR E LIMPAR
  setorder(occ_dt, .orig_id)
  
  # Colunas de resultado (todas da original)
  result_cols <- c(
    "Ctrl_geospatial_quality", "Ctrl_verbatim_quality", "Ctrl_moreInformativeRecord",
    "parseGBIF_digital_voucher", "parseGBIF_duplicates", "parseGBIF_num_duplicates",
    "parseGBIF_non_groupable_duplicates", "parseGBIF_duplicates_grouping_status",
    "Ctrl_coordinates_validated_by_gbif_issue", "parseGBIF_unidentified_sample",
    "parseGBIF_wcvp_plant_name_id", "parseGBIF_sample_taxon_name",
    "parseGBIF_sample_taxon_name_status", "parseGBIF_number_taxon_names",
    "parseGBIF_useful_for_spatial_analysis", "parseGBIF_decimalLatitude",
    "parseGBIF_decimalLongitude", "parseGBIF_dataset_result"
  )
  
  # Criar versão limpa (sem colunas intermediárias)
  cols_to_keep <- setdiff(names(occ_dt), 
                          c(".orig_id", "key_clean", "v_lat", "v_lon", "v_val",
                            "best_alt_lat", "best_alt_lon", "has_i1", "has_i2", "has_i3",
                            "temAnoColeta", "temCodigoInstituicao", "temNumeroCatalogo",
                            "temColetor", "temNumeroColeta", "temPais", "temUF",
                            "temMunicipio", "temLocalidade", "temNotas"))
  
  final_occ <- occ_dt[, ..cols_to_keep]
  
  # 12. RETORNO
  return(list(
    occ_digital_voucher = final_occ,
    occ_results = occ_dt[, ..result_cols]
  ))
}