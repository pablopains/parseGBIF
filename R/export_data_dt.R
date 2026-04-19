#' @title Export Parsed GBIF Data Results (data.table version)
#' @name export_data_dt
#'
#' @description
#' Processes and exports results from parsed GBIF occurrence data, merging information
#' from duplicate records to create unique collection event records. For each unique
#' collection event key (complete or incomplete), this function combines information
#' from duplicate records and generates a single unique collection event record.
#' 
#' This is an optimized version using \code{data.table} for improved performance on
#' large datasets, while maintaining the same interface and output as \code{export_data}.
#'
#' @param occ_digital_voucher_file
#' Character. Path to CSV file result from `select_digital_voucher()$occ_digital_voucher`.
#'
#' @param occ_digital_voucher
#' Data frame. Result from `select_digital_voucher()$occ_digital_voucher`.
#'
#' @param merge_unusable_data
#' Logical. If `TRUE`, includes incomplete unique collection events in merge processing.
#' Default is `FALSE`.
#'
#' @param fields_to_merge
#' Character vector. Fields to merge from duplicates. Default includes:
#' `Ctrl_fieldNotes`, `Ctrl_year`, `Ctrl_stateProvince`, `Ctrl_municipality`,
#' `Ctrl_locality`, `Ctrl_countryCode`, `Ctrl_eventDate`, `Ctrl_habitat`,
#' `Ctrl_level0Name`, `Ctrl_level1Name`, `Ctrl_level2Name`, `Ctrl_level3Name`.
#'
#' @param fields_to_compare
#' Character vector. Fields to compare content frequency across duplicates.
#'
#' @param fields_to_parse
#' Character vector. All fields to include in output.
#'
#' @param silence
#' Logical. If `TRUE`, does not display progress messages. Default is `TRUE`.
#'
#' @details
#' ## Taxonomic Identification Selection:
#' For complete unique collection event keys, the accepted taxon name is selected as:
#' 1. The most frequently applied name at or below species rank among duplicates
#' 2. If equal frequency, uses alphabetical order
#' 3. If no species-level identification, marked as unidentified
#'
#' ## Geospatial Information:
#' If the master voucher lacks coordinates, coordinates are sought from duplicate records.
#'
#' ## Output Datasets:
#' - **useable_data**: Unique collection events with taxonomic identification and coordinates
#' - **unusable_data**: Unique collection events without identification and/or coordinates
#' - **duplicates**: All duplicate records of unique collection events
#'
#' ## Field Merging:
#' For complete unique collection events, empty fields in the digital voucher record
#' are populated with data from duplicates during content merging.
#'
#' @return
#' A list with 6 data frames:
#' - `all_data`: All processed records (merged unique collection events and duplicates)
#' - `useable_data_merge`: Merged complete unique collection events
#' - `useable_data_raw`: Raw complete unique collection events
#' - `duplicates`: Duplicates of unique collection events
#' - `unusable_data_merge`: Merged incomplete unique collection events (NA if merge_unusable_data=FALSE)
#' - `unusable_data_raw`: Raw incomplete unique collection events
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`export_data()`] for the base R version,
#' [`select_digital_voucher_dt()`] for selecting digital vouchers with data.table,
#' [`batch_checkName_wcvp()`] for taxonomic name checking,
#' [`extract_gbif_issue()`] for GBIF data quality issues
#'
#' @importFrom data.table as.data.table fread set setorder setkey rbindlist
#' @importFrom jsonlite fromJSON
#' @importFrom jsonify to_json
#' @export
export_data_dt <- function(occ_digital_voucher_file = '',
                            occ_digital_voucher = NA,
                            merge_unusable_data = FALSE,
                            fields_to_merge = c('Ctrl_fieldNotes', 'Ctrl_year', 'Ctrl_stateProvince', 'Ctrl_municipality',
                                                'Ctrl_locality', 'Ctrl_countryCode', 'Ctrl_eventDate', 'Ctrl_habitat',
                                                'Ctrl_level0Name', 'Ctrl_level1Name', 'Ctrl_level2Name', 'Ctrl_level3Name'),
                            fields_to_compare = c('Ctrl_gbifID', 'Ctrl_scientificName', 'Ctrl_recordedBy', 'Ctrl_recordNumber',
                                                  'Ctrl_identifiedBy', 'Ctrl_dateIdentified', 'Ctrl_institutionCode',
                                                  'Ctrl_collectionCode', 'Ctrl_datasetName', 'Ctrl_language',
                                                  "wcvp_plant_name_id", "wcvp_taxon_rank", "wcvp_taxon_status",
                                                  "wcvp_family", "wcvp_taxon_name", "wcvp_taxon_authors", "wcvp_searchNotes"),
                            fields_to_parse = c('Ctrl_gbifID', 'Ctrl_bibliographicCitation', 'Ctrl_language',
                                                'Ctrl_institutionCode', 'Ctrl_collectionCode', 'Ctrl_datasetName',
                                                'Ctrl_basisOfRecord', 'Ctrl_catalogNumber', 'Ctrl_recordNumber',
                                                'Ctrl_recordedBy', 'Ctrl_occurrenceStatus', 'Ctrl_eventDate',
                                                'Ctrl_year', 'Ctrl_month', 'Ctrl_day', 'Ctrl_habitat', 'Ctrl_fieldNotes',
                                                'Ctrl_eventRemarks', 'Ctrl_countryCode', 'Ctrl_stateProvince',
                                                'Ctrl_municipality', 'Ctrl_county', 'Ctrl_locality', 'Ctrl_issue',
                                                'Ctrl_level0Name', 'Ctrl_level1Name', 'Ctrl_level2Name', 'Ctrl_level3Name',
                                                'Ctrl_identifiedBy', 'Ctrl_dateIdentified', 'Ctrl_scientificName',
                                                'Ctrl_taxonRank', 'Ctrl_decimalLatitude', 'Ctrl_decimalLongitude',
                                                'Ctrl_nameRecordedBy_Standard', 'Ctrl_recordNumber_Standard',
                                                'Ctrl_key_family_recordedBy_recordNumber', 'Ctrl_geospatial_quality',
                                                'Ctrl_verbatim_quality', 'Ctrl_moreInformativeRecord',
                                                'Ctrl_coordinates_validated_by_gbif_issue',
                                                "wcvp_plant_name_id", "wcvp_taxon_rank", "wcvp_taxon_status",
                                                "wcvp_family", "wcvp_taxon_name", "wcvp_taxon_authors",
                                                "wcvp_searchedName", "wcvp_searchNotes",
                                                'parseGBIF_digital_voucher', 'parseGBIF_duplicates',
                                                'parseGBIF_num_duplicates', 'parseGBIF_non_groupable_duplicates',
                                                'parseGBIF_duplicates_grouping_status', 'parseGBIF_unidentified_sample',
                                                'parseGBIF_sample_taxon_name', 'parseGBIF_sample_taxon_name_status',
                                                'parseGBIF_number_taxon_names', 'parseGBIF_useful_for_spatial_analysis',
                                                'parseGBIF_decimalLatitude', 'parseGBIF_decimalLongitude',
                                                'parseGBIF_wcvp_plant_name_id', 'parseGBIF_wcvp_taxon_rank',
                                                'parseGBIF_wcvp_taxon_status', 'parseGBIF_wcvp_family',
                                                'parseGBIF_wcvp_taxon_name', 'parseGBIF_wcvp_taxon_authors',
                                                'parseGBIF_wcvp_reviewed', 'parseGBIF_dataset_result'),
                            silence = TRUE) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Pacote data.table é necessário.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Pacote jsonlite é necessário.")
  if (!requireNamespace("jsonify", quietly = TRUE)) stop("Pacote jsonify é necessário.")
  
  # 1. Carregamento dos dados
  if (!is.na(occ_digital_voucher_file) && occ_digital_voucher_file != "") {
    occ_tmp <- data.table::fread(occ_digital_voucher_file, encoding = "UTF-8", data.table = TRUE)
  } else {
    occ_tmp <- data.table::as.data.table(occ_digital_voucher)
  }
  
  # 2. Conversão de datas para character (evita problemas de merge)
  date_cols <- names(occ_tmp)[sapply(occ_tmp, function(x) inherits(x, c("POSIXct", "POSIXlt", "Date")))]
  for (col in date_cols) data.table::set(occ_tmp, j = col, value = as.character(occ_tmp[[col]]))
  
  # 3. Seleção e inicialização de colunas
  occ_tmp <- occ_tmp[, ..fields_to_parse]
  occ_tmp[, c("parseGBIF_freq_duplicate_or_missing_data", "parseGBIF_duplicates_map", "parseGBIF_merged_fields") := .("", "", "")]
  if (!"parseGBIF_merged" %in% names(occ_tmp)) occ_tmp[, parseGBIF_merged := FALSE]
  
  # 4. Separação dos datasets
  data.table::setorder(occ_tmp, Ctrl_key_family_recordedBy_recordNumber)
  occ_in <- occ_tmp[parseGBIF_dataset_result == "useable"]
  occ_dup <- occ_tmp[parseGBIF_dataset_result == "duplicate"]
  occ_out_to_recover <- occ_tmp[parseGBIF_dataset_result == "unusable"]
  
  occ_res_full <- if (merge_unusable_data) 
    data.table::rbindlist(list(occ_in, occ_out_to_recover), use.names = TRUE, fill = TRUE) 
  else 
    data.table::copy(occ_in)
  
  # 5. Indexação para performance
  data.table::setkey(occ_res_full, Ctrl_key_family_recordedBy_recordNumber)
  data.table::setkey(occ_dup, Ctrl_key_family_recordedBy_recordNumber)
  keys <- unique(occ_res_full$Ctrl_key_family_recordedBy_recordNumber)
  fields_to_all <- unique(c(fields_to_compare, fields_to_merge))
  
  # 6. Loop principal de processamento
  for (s in seq_along(keys)) {
    k <- keys[s]
    if (is.na(k) || k == "") next
    
    master_subset <- occ_res_full[.(k)]
    if (nrow(master_subset) == 0 || !isTRUE(master_subset$parseGBIF_duplicates[1])) next
    
    dups <- occ_dup[.(k)]
    if (nrow(dups) == 0) next
    
    raw_master <- as.list(master_subset[1, ..fields_to_all])
    raw_dups_list <- lapply(fields_to_all, function(col) dups[[col]])
    names(raw_dups_list) <- fields_to_all
    
    # Limpeza de strings (remove caracteres problemáticos, inclusive aspas)
    clean_master <- lapply(raw_master, function(x) {
      if (is.na(x)) return("")
      gsub('\\{|\\}|\\[|\\]|\\(|\\)|\\\\|\\*|\\"', '', as.character(x))
    })
    clean_dups <- lapply(raw_dups_list, function(vec) {
      vec[is.na(vec)] <- ""
      gsub('\\{|\\}|\\[|\\]|\\(|\\)|\\\\|\\*|\\"', '', as.character(vec))
    })
    
    freq_json_list <- character()
    dup_map_json_list <- character()
    merged_fields_json <- character()
    merged_any <- FALSE
    
    for (col in fields_to_all) {
      val_master_clean <- clean_master[[col]]
      vals_dup_clean <- clean_dups[[col]]
      
      # --- Tabela de frequência (igual à original) ---
      all_vals <- c(raw_master[[col]], raw_dups_list[[col]])
      tbl <- table(all_vals, useNA = "no")
      freq_df <- if(length(tbl) > 0) {
        d <- as.data.frame(tbl, stringsAsFactors = FALSE)
        names(d) <- c("value", "freq")
        d[order(-d$freq), ]
      } else data.frame(value = character(), freq = integer())
      
      diff_empty <- master_subset$parseGBIF_num_duplicates[1] - (if(nrow(freq_df) > 0) sum(freq_df$freq) else 0)
      if (diff_empty > 0) freq_df <- rbind(freq_df, data.frame(value = "empty", freq = diff_empty))
      if (nrow(freq_df) > 0) freq_json_list <- c(freq_json_list, sprintf('"%s":%s', col, jsonify::to_json(freq_df)))
      
      # --- Mapa de duplicatas (com validação JSON, igual à original) ---
      current_col_map <- character()
      added_vals <- character()
      for (ix in seq_len(nrow(dups))) {
        v_clean <- vals_dup_clean[ix]
        if (v_clean == "" || nchar(v_clean) > 10000) next
        if (v_clean %in% added_vals) next
        if (toupper(v_clean) == toupper(val_master_clean)) next
        
        # Validação JSON: tenta interpretar o valor em uma estrutura simples
        test_json <- tryCatch({
          jsonlite::fromJSON(paste0('{"test":["', gsub('"', '', v_clean), '"]}'))
          TRUE
        }, error = function(e) FALSE)
        if (!test_json) next
        
        added_vals <- c(added_vals, v_clean)
        current_col_map <- c(current_col_map, sprintf('"%s"', v_clean))
      }
      if (length(current_col_map) > 0) {
        dup_map_json_list <- c(dup_map_json_list, sprintf('"%s":["%s",%s]', col, val_master_clean, paste(current_col_map, collapse = ",")))
      }
      
      # --- Merge de campos vazios (COMPORTAMENTO ORIGINAL: usa a última duplicata válida) ---
      if (val_master_clean == "" && col %in% fields_to_merge) {
        for (ix in seq_len(nrow(dups))) {
          if (vals_dup_clean[ix] != "" && nchar(vals_dup_clean[ix]) <= 10000) {
            occ_res_full[.(k), (col) := raw_dups_list[[col]][ix]]
            merged_fields_json <- c(merged_fields_json, sprintf('"%s":["%s"]', col, dups$Ctrl_gbifID[ix]))
            merged_any <- TRUE
            # NÃO há break: continua para que a última duplicata prevaleça
          }
        }
      }
    }
    
    # Atualiza as colunas de metadados no registro mestre
    if (length(freq_json_list) > 0) 
      occ_res_full[.(k), parseGBIF_freq_duplicate_or_missing_data := paste0("{", paste(freq_json_list, collapse = ","), "}")]
    if (length(dup_map_json_list) > 0) 
      occ_res_full[.(k), parseGBIF_duplicates_map := paste0("{", paste(dup_map_json_list, collapse = ","), "}")]
    if (merged_any) 
      occ_res_full[.(k), `:=`(parseGBIF_merged_fields = paste0("{", paste(merged_fields_json, collapse = ","), "}"), parseGBIF_merged = TRUE)]
    
    if (!silence && s %% 1000 == 0) 
      print(paste0("Processados ", s, " de ", length(keys), " grupos"))
  }
  
  # 7. Recomposição final dos objetos de saída
  occ_all <- data.table::rbindlist(list(
    occ_res_full[parseGBIF_dataset_result == "useable"],
    if (merge_unusable_data) occ_res_full[parseGBIF_dataset_result == "unusable"] else occ_out_to_recover,
    occ_dup
  ), use.names = TRUE, fill = TRUE)
  
  return(list(
    all_data = as.data.frame(occ_all),
    useable_data_merge = as.data.frame(occ_res_full[parseGBIF_dataset_result == "useable"]),
    useable_data_raw = as.data.frame(occ_in),
    duplicates = as.data.frame(occ_dup),
    unusable_data_merge = if (merge_unusable_data) as.data.frame(occ_res_full[parseGBIF_dataset_result == "unusable"]) else NA,
    unusable_data_raw = as.data.frame(occ_out_to_recover)
  ))
}
