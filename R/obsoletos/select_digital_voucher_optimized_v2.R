select_digital_voucher_optimized_v2 <- function(occ = NA,
                                        occ_gbif_issue = NA,
                                        occ_wcvp_check_name = NA,
                                        occ_collectorsDictionary = NA,
                                        enumOccurrenceIssue = NA,
                                        silence = TRUE) {

  stopifnot(requireNamespace("data.table", quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))

  library(data.table)
  library(stringr)
  library(dplyr)

  # -----------------------------
  # EnumOccurrenceIssue
  # -----------------------------
  if (is.na(enumOccurrenceIssue)[1]) {
    data(EnumOccurrenceIssue, envir = environment())
  } else {
    EnumOccurrenceIssue <- enumOccurrenceIssue
  }

  occ_in <- occ

  # -----------------------------
  # 1) "cbind" (mesma premissa do original: alinhado por linha)
  #    -> use data.table e evite cópias repetidas
  # -----------------------------
  dt_occ_in  <- as.data.table(occ_in)
  dt_issue   <- as.data.table(occ_gbif_issue)
  dt_wcvp    <- as.data.table(occ_wcvp_check_name)
  dt_collDic <- as.data.table(occ_collectorsDictionary)

  n <- nrow(dt_occ_in)
  if (!all(n == nrow(dt_issue), n == nrow(dt_wcvp), n == nrow(dt_collDic))) {
    stop("All inputs must have identical nrow (same assumption as original cbind).")
  }

  dt <- copy(dt_issue)
  # Nota: mantém paridade com cbind desde que não haja nomes duplicados relevantes.
  # Se houver, você precisa renomear antes (senão, o original cria .1/.2 e aqui não).
  for (nm in names(dt_occ_in))  dt[, (nm) := dt_occ_in[[nm]]]
  for (nm in names(dt_wcvp))    dt[, (nm) := dt_wcvp[[nm]]]
  for (nm in names(dt_collDic)) dt[, (nm) := dt_collDic[[nm]]]

  # NA -> '' (paridade)
  if ("wcvp_taxon_rank" %in% names(dt))   dt[is.na(wcvp_taxon_rank),   wcvp_taxon_rank := ""]
  if ("wcvp_taxon_status" %in% names(dt)) dt[is.na(wcvp_taxon_status), wcvp_taxon_status := ""]

  # -----------------------------
  # 2) índices de issues geoespaciais
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
  cols1 <- cols1[cols1 %in% names(dt)]
  cols2 <- cols2[cols2 %in% names(dt)]
  cols3 <- cols3[cols3 %in% names(dt)]

  # -----------------------------
  # 3) Flags de verbatim (vetorizado, in-place)
  # -----------------------------
  nzchar0 <- function(x) !is.na(x) & x != ""

  dt[, temAnoColeta := !(is.na(Ctrl_year) | Ctrl_year == "" | Ctrl_year == 0 | Ctrl_year <= 10)]
  dt[is.na(temAnoColeta), temAnoColeta := FALSE]

  dt[, temCodigoInstituicao := nzchar0(Ctrl_institutionCode)]
  dt[, temNumeroCatalogo    := nzchar0(Ctrl_catalogNumber)]
  dt[, temColetor           := nzchar0(Ctrl_recordedBy)]
  dt[, temNumeroColeta      := nzchar0(Ctrl_recordNumber)]

  if ("COUNTRY_INVALID" %in% names(dt)) {
    dt[, temPais := !(COUNTRY_INVALID == TRUE)]
    dt[is.na(temPais), temPais := FALSE]
  } else {
    dt[, temPais := TRUE]
  }

  dt[, temUF         := nzchar0(Ctrl_stateProvince)]
  dt[, temMunicipio  := nzchar0(Ctrl_municipality)]
  dt[, temLocalidade := nzchar0(Ctrl_locality)]
  dt[, temNotas      := nzchar0(Ctrl_fieldNotes)]

  # -----------------------------
  # 4) Scores geoespaciais e verbatim (calcula 1x)
  # -----------------------------
  dt[, `:=`(
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

  # coordenadas validadas: rowSums(score3)==0
  if (length(cols3) > 0) {
    m3 <- as.matrix(dt[, ..cols3])
    s3 <- rowSums(m3, na.rm = TRUE)
    dt[, Ctrl_coordinates_validated_by_gbif_issue := (s3 == 0)]
    rm(m3, s3)
  } else {
    dt[, Ctrl_coordinates_validated_by_gbif_issue := TRUE]
  }

  dt[Ctrl_hasCoordinate == FALSE | Ctrl_decimalLatitude == 0 | Ctrl_decimalLongitude == 0,
     Ctrl_coordinates_validated_by_gbif_issue := FALSE]
  dt[is.na(Ctrl_coordinates_validated_by_gbif_issue), Ctrl_coordinates_validated_by_gbif_issue := FALSE]

  # qualidade geoespacial (paridade com ifelse encadeado)
  has3 <- if (length(cols3) > 0) rowSums(as.matrix(dt[, ..cols3]), na.rm = TRUE) > 0 else rep(FALSE, n)
  has2 <- if (length(cols2) > 0) rowSums(as.matrix(dt[, ..cols2]), na.rm = TRUE) > 0 else rep(FALSE, n)
  has1 <- if (length(cols1) > 0) rowSums(as.matrix(dt[, ..cols1]), na.rm = TRUE) > 0 else rep(FALSE, n)

  dt[, Ctrl_geospatial_quality := 0L]
  dt[has1, Ctrl_geospatial_quality := -1L]
  dt[has2, Ctrl_geospatial_quality := -3L]
  dt[has3, Ctrl_geospatial_quality := -9L]
  dt[Ctrl_hasCoordinate == FALSE, Ctrl_geospatial_quality := -9L]

  dt[, Ctrl_verbatim_quality :=
       as.integer(temColetor) +
       as.integer(temNumeroColeta) +
       as.integer(temAnoColeta) +
       as.integer(temCodigoInstituicao) +
       as.integer(temNumeroCatalogo) +
       as.integer(temLocalidade) +
       as.integer(temMunicipio) +
       as.integer(temUF) +
       as.integer(temPais) +
       as.integer(temNotas)
  ]

  dt[, Ctrl_moreInformativeRecord := as.integer(Ctrl_geospatial_quality) + as.integer(Ctrl_verbatim_quality)]

  # -----------------------------
  # 5) reduz para as colunas que o loop realmente usa (igual ao seu select)
  # -----------------------------
  dt <- dt[, .(
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
  # 6) Corrige chave "_NA" (paridade)
  # -----------------------------
  idx_na <- stringr::str_sub(dt$Ctrl_key_family_recordedBy_recordNumber,
                             stringr::str_count(dt$Ctrl_key_family_recordedBy_recordNumber) - 2,
                             stringr::str_count(dt$Ctrl_key_family_recordedBy_recordNumber)) %in% "_NA"
  if (any(idx_na, na.rm = TRUE)) {
    dt[idx_na == TRUE,
       Ctrl_key_family_recordedBy_recordNumber :=
         stringr::str_sub(Ctrl_key_family_recordedBy_recordNumber, 1,
                          stringr::str_count(Ctrl_key_family_recordedBy_recordNumber) - 2)]
  }

  # -----------------------------
  # 7) AQUI está o ganho: processar por grupo SEM loop+%in%
  #    - usa gid por primeira ocorrência (paridade de ordem com unique())
  # -----------------------------
  dt[, .rowid__ := .I]
  keys_in_order <- unique(dt$Ctrl_key_family_recordedBy_recordNumber)
  dt[, gid__ := match(Ctrl_key_family_recordedBy_recordNumber, keys_in_order)]

  # Progress
  tot <- length(keys_in_order)
  s <- 0L

  dt[, {
    s <<- s + 1L
    if (!silence && (s %% 100L == 0L)) print(paste0(s, " de ", tot))

    r <- Ctrl_key_family_recordedBy_recordNumber[1]
    nr <- .N

    FAMILY__ <- FAMILY__recordNumber <- FAMILY_recordedBy_ <- FALSE

    if (stringr::str_sub(r, stringr::str_count(r), stringr::str_count(r)) == "_" ||
        grepl("__", r, fixed = TRUE) ||
        grepl("UNKNOWN-COLLECTOR", r, fixed = TRUE)) {

      FAMILY__ <- (grepl("__", r, fixed = TRUE) &&
                     stringr::str_locate(r, "__")[2] == stringr::str_count(r)) %>%
        ifelse(is.na(.), FALSE, .)

      if (FAMILY__ == FALSE) {

        FAMILY_recordedBy_ <- ((grepl("__", r, fixed = TRUE) &&
                                  stringr::str_locate(r, "__")[2] != stringr::str_count(r)) ||
                                 grepl("UNKNOWN-COLLECTOR", r, fixed = TRUE)) %>%
          ifelse(is.na(.), FALSE, .)

        if (FAMILY_recordedBy_ == FALSE) {
          FAMILY__recordNumber <- (stringr::str_sub(r, stringr::str_count(r), stringr::str_count(r)) == "_" &&
                                     !stringr::str_sub(r, stringr::str_count(r)-1, stringr::str_count(r)-1) == "_") %>%
            ifelse(is.na(.), FALSE, .)
        }
      }
    }

    non_groupable <- (FAMILY__ == TRUE | FAMILY__recordNumber == TRUE | FAMILY_recordedBy_ == TRUE)

    if (non_groupable) {

      sp_name <- ifelse(wcvp_taxon_status == "Accepted", as.character(wcvp_taxon_name), "")
      sp_id   <- ifelse(wcvp_taxon_status == "Accepted", as.character(wcvp_plant_name_id), "")

      grp_status <- if (FAMILY__ == TRUE) {
        "not groupable: no recordedBy and no recordNumber"
      } else if (FAMILY__recordNumber == TRUE) {
        "not groupable: no recordNumber "
      } else if (FAMILY_recordedBy_ == TRUE) {
        "not groupable: no recordedBy"
      } else {
        "not groupable"
      }

      list(
        parseGBIF_digital_voucher = rep(TRUE, nr),
        parseGBIF_non_groupable_duplicates = rep(TRUE, nr),
        parseGBIF_duplicates = rep(FALSE, nr),
        parseGBIF_num_duplicates = rep(1L, nr),

        parseGBIF_wcvp_plant_name_id = sp_id,
        parseGBIF_sample_taxon_name = sp_name,
        parseGBIF_unidentified_sample = (sp_name == ""),

        parseGBIF_duplicates_grouping_status = rep(grp_status, nr),
        parseGBIF_number_taxon_names = ifelse(sp_name == "", 0L, 1L),
        parseGBIF_sample_taxon_name_status = ifelse(sp_name == "", "unidentified", "identified"),

        parseGBIF_decimalLatitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE, Ctrl_decimalLatitude, NA_real_),
        parseGBIF_decimalLongitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue == TRUE, Ctrl_decimalLongitude, NA_real_),
        parseGBIF_useful_for_spatial_analysis = Ctrl_coordinates_validated_by_gbif_issue
      )

    } else {
      # voucher: mesma regra + empate resolve pelo primeiro (which.max já faz isso)
      winner <- which.max(Ctrl_moreInformativeRecord)
      dv <- rep(FALSE, nr)
      dv[winner] <- TRUE

      # duplicates
      dup_flag <- (nr > 1L)

      # Taxon logic (igual ao original, mas sem criar/reatribuir data.frame gigante fora do grupo)
      wcvp_taxon_name_and_id <- paste0(wcvp_taxon_name, ";", wcvp_plant_name_id)

      if (!any(is.na(wcvp_taxon_name) == FALSE)) {
        p_num_tax <- rep(0L, nr)
        p_id <- rep("", nr)
        p_name <- rep("", nr)
        p_status <- rep("unidentified", nr)
        p_unid <- rep(TRUE, nr)
      } else {
        tax_tab <- as.data.frame(table(wcvp_taxon_name_and_id, wcvp_taxon_status, exclude = NA),
                                 stringsAsFactors = FALSE)
        names(tax_tab) <- c("Var1","Var2","Freq")
        tax_tab <- tax_tab[tax_tab$Freq > 0, , drop = FALSE]
        tax_tab <- tax_tab[order(-tax_tab$Freq, tax_tab$Var1), , drop = FALSE]

        num_tax <- nrow(tax_tab)

        if (num_tax == 0) {
          p_num_tax <- rep(0L, nr)
          p_id <- rep("", nr)
          p_name <- rep("", nr)
          p_status <- rep("unidentified", nr)
          p_unid <- rep(TRUE, nr)
        } else if (num_tax == 1 && tax_tab$Var2[1] %in% c("Accepted")) {
          sp <- stringr::str_split(tax_tab$Var1[1], ";", simplify = TRUE)
          p_num_tax <- rep(num_tax, nr)
          p_id <- rep(sp[,2], nr)
          p_name <- rep(sp[,1], nr)
          p_status <- rep("identified", nr)
          p_unid <- rep(FALSE, nr)
        } else if (num_tax > 1) {
          chosen <- NA_integer_
          for (ii in seq_len(num_tax)) {
            if (tax_tab$Var2[ii] %in% c("Accepted")) { chosen <- ii; break }
          }
          if (!is.na(chosen)) {
            sp <- stringr::str_split(tax_tab$Var1[chosen], ";", simplify = TRUE)
            p_num_tax <- rep(num_tax, nr)
            p_id <- rep(sp[,2], nr)
            p_name <- rep(sp[,1], nr)
            p_status <- rep("divergent identifications", nr)
            p_unid <- rep(FALSE, nr)
          } else {
            p_num_tax <- rep(num_tax, nr)
            p_id <- rep("", nr)
            p_name <- rep("", nr)
            p_status <- rep("unidentified", nr)
            p_unid <- rep(TRUE, nr)
          }
        } else {
          p_num_tax <- rep(num_tax, nr)
          p_id <- rep("", nr)
          p_name <- rep("", nr)
          p_status <- rep("unidentified", nr)
          p_unid <- rep(TRUE, nr)
        }
      }

      # Coordenadas (paridade do seu bloco)
      use_lat <- NA_real_
      use_lon <- NA_real_
      use_spatial <- FALSE

      if (isTRUE(Ctrl_coordinates_validated_by_gbif_issue[winner] == TRUE)) {
        use_lat <- Ctrl_decimalLatitude[winner]
        use_lon <- Ctrl_decimalLongitude[winner]
        use_spatial <- TRUE
      } else {
        idx_ok <- which(Ctrl_coordinates_validated_by_gbif_issue == TRUE)
        if (length(idx_ok) == 1) {
          use_lat <- Ctrl_decimalLatitude[idx_ok]
          use_lon <- Ctrl_decimalLongitude[idx_ok]
          use_spatial <- TRUE
        } else if (length(idx_ok) > 1) {
          gbest <- max(Ctrl_geospatial_quality[idx_ok], na.rm = TRUE)
          idx_best <- idx_ok[which(Ctrl_geospatial_quality[idx_ok] == gbest)]
          use_lat <- Ctrl_decimalLatitude[idx_best[1]]
          use_lon <- Ctrl_decimalLongitude[idx_best[1]]
          use_spatial <- TRUE
        }
      }

      list(
        parseGBIF_duplicates_grouping_status = rep("groupable", nr),
        parseGBIF_duplicates = rep(dup_flag, nr),
        parseGBIF_num_duplicates = rep(as.integer(nr), nr),
        parseGBIF_digital_voucher = dv,

        parseGBIF_number_taxon_names = p_num_tax,
        parseGBIF_wcvp_plant_name_id = p_id,
        parseGBIF_sample_taxon_name = p_name,
        parseGBIF_sample_taxon_name_status = p_status,
        parseGBIF_unidentified_sample = p_unid,

        parseGBIF_decimalLatitude = rep(use_lat, nr),
        parseGBIF_decimalLongitude = rep(use_lon, nr),
        parseGBIF_useful_for_spatial_analysis = rep(use_spatial, nr)
      )
    }

  }, by = gid__]

  dt[, c("gid__", ".rowid__") := NULL]

  # -----------------------------
  # 8) occ_results (mesmo select final)
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
  # 9) Reconstrói occ_all como o original (useable/unusable/duplicate + join xn)
  # -----------------------------
  occ_all_base <- as.data.table(occ_in)
  for (nm in names(dt_wcvp))    occ_all_base[, (nm) := dt_wcvp[[nm]]]
  for (nm in names(dt_collDic)) occ_all_base[, (nm) := dt_collDic[[nm]]]
  for (nm in names(occ_results)) occ_all_base[, (nm) := occ_results[[nm]]]

  occ_in_2 <- occ_all_base[parseGBIF_digital_voucher == TRUE &
                             parseGBIF_unidentified_sample == FALSE &
                             parseGBIF_useful_for_spatial_analysis == TRUE][
                               , parseGBIF_dataset_result := "useable"
                             ]

  occ_dup <- occ_all_base[parseGBIF_digital_voucher == FALSE][
    , parseGBIF_dataset_result := "duplicate"
  ]

  occ_out_to_recover <- occ_all_base[parseGBIF_digital_voucher == TRUE &
                                       (parseGBIF_unidentified_sample == TRUE |
                                          parseGBIF_useful_for_spatial_analysis == FALSE)][
                                            , parseGBIF_dataset_result := "unusable"
                                          ]

  occ_all <- rbindlist(list(occ_in_2, occ_out_to_recover, occ_dup), use.names = TRUE, fill = TRUE)

  xn <- as.data.table(occ_wcvp_check_name)[, .(
    wcvp_plant_name_id = as.character(wcvp_plant_name_id),
    wcvp_taxon_rank,
    wcvp_taxon_status,
    wcvp_family,
    wcvp_taxon_name,
    wcvp_taxon_authors,
    wcvp_reviewed
  )]
  xn <- unique(xn)
  setnames(xn, old = names(xn), new = paste0("parseGBIF_", names(xn)))

  # left join por parseGBIF_wcvp_plant_name_id
  if ("parseGBIF_wcvp_plant_name_id" %in% names(occ_all) && "parseGBIF_wcvp_plant_name_id" %in% names(xn)) {
    occ_all <- merge(occ_all, xn, by = "parseGBIF_wcvp_plant_name_id", all.x = TRUE, sort = FALSE)
  }

  # Seleção final de colunas (igual ao seu select) — mantém apenas as que existirem
  final_cols <- c(
    "Ctrl_gbifID","Ctrl_bibliographicCitation","Ctrl_language","Ctrl_institutionCode","Ctrl_collectionCode",
    "Ctrl_datasetName","Ctrl_basisOfRecord","Ctrl_catalogNumber","Ctrl_recordNumber","Ctrl_recordedBy",
    "Ctrl_georeferenceVerificationStatus","Ctrl_occurrenceStatus","Ctrl_eventDate","Ctrl_year","Ctrl_month","Ctrl_day",
    "Ctrl_habitat","Ctrl_fieldNotes","Ctrl_eventRemarks","Ctrl_locationID","Ctrl_higherGeography","Ctrl_islandGroup","Ctrl_island",
    "Ctrl_countryCode","Ctrl_stateProvince","Ctrl_municipality","Ctrl_county","Ctrl_locality","Ctrl_verbatimLocality","Ctrl_locationRemarks",
    "Ctrl_level0Name","Ctrl_level1Name","Ctrl_level2Name","Ctrl_level3Name","Ctrl_identifiedBy","Ctrl_dateIdentified","Ctrl_scientificName",
    "Ctrl_decimalLatitude","Ctrl_decimalLongitude","Ctrl_identificationQualifier",
    "Ctrl_typeStatus","Ctrl_family","Ctrl_taxonRank","Ctrl_issue",
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

  return(list(
    occ_digital_voucher = as.data.frame(occ_all),
    occ_results = as.data.frame(occ_results)
  ))
}
