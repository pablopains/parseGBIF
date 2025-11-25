#' @title Generate Collection Event Key Version 2
#' @name generate_collection_event_key_v2
#'
#' @description
#' Generates standardized collection event keys for biological occurrence data using
#' optimized vectorized operations. Matches recorded collector names against collector
#' dictionaries and creates unique identifiers based on family, collector, record number,
#' and year for efficient duplicate grouping.
#'
#' @param occ
#' Data frame. GBIF occurrence data containing required columns:
#' `Ctrl_recordNumber`, `Ctrl_family`, `Ctrl_recordedBy`, `Ctrl_year`.
#'
#' @param collectorDictionary_checked_file
#' Character. Path to verified collector dictionary CSV file.
#'
#' @param collectorDictionary_checked
#' Data frame. Pre-loaded verified collector dictionary.
#'
#' @param collectorDictionary_file
#' Character. Path to base collector dictionary CSV file. If provided, loads
#' default dictionary from parseGBIF GitHub repository.
#'
#' @param collectorDictionary
#' Data frame. Pre-loaded base collector dictionary.
#'
#' @param silence
#' Logical. If `TRUE`, suppresses progress messages. Default is `TRUE`.
#'
#' @details
#' ## Key Improvements from Version 1:
#' - Vectorized operations replacing loops for better performance
#' - Efficient data.table joins for large datasets
#' - Memory-optimized dictionary loading
#' - Batch processing of collector name matching
#'
#' ## Processing Steps:
#' 1. Loads and validates collector dictionaries
#' 2. Performs vectorized matching of collector names
#' 3. Standardizes record numbers (numeric extraction)
#' 4. Generates unique collection event keys
#' 5. Identifies new collectors for dictionary updates
#'
#' @return
#' A list with three components:
#' - `occ_collectorsDictionary`: Occurrence data with standardized names and collection keys
#' - `summary`: Frequency summary of collection keys
#' - `collectorsDictionary_add`: New collector entries for dictionary updates
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @encoding UTF-8
#'
#' @examples
#' \donttest{
#' # Generate collection event keys with optimized processing
#' result <- generate_collection_event_key_v2(
#'   occ = occ_data,
#'   collectorDictionary_checked_file = 'collectorDictionary_checked.csv',
#'   silence = FALSE
#' )
#'
#' # View optimized results
#' names(result)
#' head(result$occ_collectorsDictionary)
#' head(result$summary)
#' }
#'
#' @importFrom dplyr bind_rows mutate select rename distinct left_join coalesce
#' @importFrom dplyr if_else arrange desc count all_of case_when
#' @importFrom readr read_csv locale
#' @importFrom stringr str_replace_all
#' @importFrom data.table as.data.table
#' @importFrom utils rm
#' @export
generate_collection_event_key_v2 <- function(occ = NA,
                                             collectorDictionary_checked_file = NULL,
                                             collectorDictionary_checked = NULL,
                                             collectorDictionary_file = NULL,
                                             collectorDictionary = NULL,
                                             silence = TRUE) {

  # 1. Efficient dictionary loading
  if (!silence) message('Loading collectorDictionary...')

  if (is.null(collectorDictionary) && !is.null(collectorDictionary_file)) {
    if (!silence) message("Loading parseGBIF GitHub dictionaries...")

    # Load dictionaries in parallel (conceptually)
    dict1 <- readr::read_csv(
      'https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_1.csv',
      locale = readr::locale(encoding = 'UTF-8'),
      show_col_types = FALSE
    )
    dict2 <- readr::read_csv(
      'https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_2.csv',
      locale = readr::locale(encoding = 'UTF-8'),
      show_col_types = FALSE
    )
    collectorDictionary <- dplyr::bind_rows(dict1, dict2)
  }

  # Validation
  required_cols <- c('Ctrl_nameRecordedBy_Standard', 'Ctrl_recordedBy')
  if (NROW(collectorDictionary) == 0 || !all(required_cols %in% colnames(collectorDictionary))) {
    stop("Invalid Collector's Dictionary structure!")
  }

  # 2. Optimized dictionary processing
  collectorDictionary <- collectorDictionary %>%
    dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard) %>%
    dplyr::rename(Ctrl_nameRecordedBy_Standard_CNCFlora = Ctrl_nameRecordedBy_Standard) %>%
    data.table::as.data.table()

  # 3. Load verified dictionary
  if (!is.null(collectorDictionary_checked_file) && is.null(collectorDictionary_checked)) {
    collectorDictionary_checked <- readr::read_csv(
      collectorDictionary_checked_file,
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )
  }

  if (NROW(collectorDictionary_checked) == 0 || !all(required_cols %in% colnames(collectorDictionary_checked))) {
    stop("Invalid verified Collector's Dictionary structure!")
  }

  collectorDictionary_checked <- data.table::as.data.table(collectorDictionary_checked)

  # 4. Identify new collectors using anti-join
  collectorDictionary_checked_new <- dplyr::anti_join(
    collectorDictionary_checked,
    collectorDictionary,
    by = "Ctrl_recordedBy"
  ) %>%
    dplyr::select(dplyr::all_of(required_cols))

  # 5. Pre-process occurrence data
  if (NROW(occ) == 0) stop("Empty occurrence data!")

  occ <- occ %>%
    dplyr::select(Ctrl_recordNumber, Ctrl_family, Ctrl_recordedBy, Ctrl_year) %>%
    dplyr::mutate(
      Ctrl_recordedBy = toupper(Ctrl_recordedBy),
      Ctrl_nameRecordedBy_Standard = NA_character_
    ) %>%
    data.table::as.data.table()

  # 6. Vectorized collector name matching (replaces loop)
  collectorDictionary_lookup <- collectorDictionary_checked %>%
    dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard) %>%
    dplyr::distinct(Ctrl_recordedBy, .keep_all = TRUE)

  occ <- occ %>%
    dplyr::left_join(collectorDictionary_lookup, by = "Ctrl_recordedBy", suffix = c("", ".y")) %>%
    dplyr::mutate(
      Ctrl_nameRecordedBy_Standard = dplyr::coalesce(Ctrl_nameRecordedBy_Standard.y, Ctrl_nameRecordedBy_Standard),
      Ctrl_nameRecordedBy_Standard = dplyr::if_else(
        is.na(Ctrl_nameRecordedBy_Standard),
        "NOT-FOUND-COLLECTOR",
        Ctrl_nameRecordedBy_Standard
      )
    ) %>%
    dplyr::select(-Ctrl_nameRecordedBy_Standard.y)

  # 7. Vectorized final processing
  occ <- occ %>%
    dplyr::mutate(
      Ctrl_recordNumber_Standard = stringr::str_replace_all(Ctrl_recordNumber, "[^0-9]", ""),
      Ctrl_recordNumber_Standard = dplyr::case_when(
        is.na(Ctrl_recordNumber_Standard) | Ctrl_recordNumber_Standard == "" ~ "",
        TRUE ~ as.character(as.integer(Ctrl_recordNumber_Standard))
      ),
      Ctrl_recordNumber_Standard = dplyr::if_else(
        is.na(Ctrl_recordNumber_Standard), "", Ctrl_recordNumber_Standard
      ),
      Ctrl_key_family_recordedBy_recordNumber = paste(
        toupper(trimws(Ctrl_family)),
        Ctrl_nameRecordedBy_Standard,
        Ctrl_recordNumber_Standard,
        sep = '_'
      ),
      Ctrl_key_year_recordedBy_recordNumber = paste(
        dplyr::if_else(is.na(Ctrl_year), "noYear", as.character(Ctrl_year)),
        Ctrl_nameRecordedBy_Standard,
        Ctrl_recordNumber_Standard,
        sep = '_'
      )
    )

  # 8. Optimized summary
  summary <- occ %>%
    dplyr::count(Ctrl_key_family_recordedBy_recordNumber, name = "numberOfRecords") %>%
    dplyr::arrange(dplyr::desc(numberOfRecords))

  # 9. Final output
  result_cols <- c(
    'Ctrl_nameRecordedBy_Standard', 'Ctrl_recordNumber_Standard',
    'Ctrl_key_family_recordedBy_recordNumber', 'Ctrl_key_year_recordedBy_recordNumber'
  )

  list(
    occ_collectorsDictionary = occ %>% dplyr::select(dplyr::all_of(result_cols)),
    summary = summary,
    collectorsDictionary_add = collectorDictionary_checked_new
  )
}
# generate_collection_event_key_v2 <- function(occ = NA,
#                                           collectorDictionary_checked_file = NULL,
#                                           collectorDictionary_checked = NULL,
#                                           collectorDictionary_file = NULL,
#                                           collectorDictionary = NULL,
#                                           silence = TRUE) {
#
#   # 1. Carregamento mais eficiente dos dicionários
#   if (!silence) print('Loading collectorDictionary...')
#
#   if (is.null(collectorDictionary)) {
#     if (!is.null(collectorDictionary_file)) {
#
#       if (!silence) print("Lendo Github parseGBIF...")
#
#       # Carregamento paralelo dos arquivos
#       dict1 <- readr::read_csv('https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_1.csv',
#                                locale = readr::locale(encoding = 'UTF-8'),
#                                show_col_types = FALSE)
#       dict2 <- readr::read_csv('https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_2.csv',
#                                locale = readr::locale(encoding = 'UTF-8'),
#                                show_col_types = FALSE)
#       collectorDictionary <- dplyr::bind_rows(dict1, dict2)
#       rm(dict1, dict2) # Libera memória
#     }
#   }
#
#   # Validação mais rápida
#   required_cols <- c('Ctrl_nameRecordedBy_Standard', 'Ctrl_recordedBy')
#
#   if (NROW(collectorDictionary) == 0 || !all(required_cols %in% colnames(collectorDictionary))) {
#     stop("Empty or invalid Collector's Dictionary!")
#   }
#
#   # 2. Otimização: processamento em lote do dicionário
#   collectorDictionary <- collectorDictionary %>%
#     # dplyr::mutate(Ctrl_recordedBy = toupper(Ctrl_recordedBy)) %>%
#     dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy) %>%
#     dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard) %>%
#     dplyr::rename(Ctrl_nameRecordedBy_Standard_CNCFlora = Ctrl_nameRecordedBy_Standard) %>%
#     data.table::as.data.table() # Conversão para data.table para joins mais rápidos
#
#   # 3. Carregamento e processamento do dicionário verificado
#   if (!is.null(collectorDictionary_checked_file)) {
#     if (!silence) print("Carregamento e processamento do dicionário verificado...")
#
#     collectorDictionary_checked <- readr::read_csv(collectorDictionary_checked_file,
#                                                    locale = readr::locale(encoding = "UTF-8"),
#                                                    show_col_types = FALSE)
#   }
#
#   if (NROW(collectorDictionary_checked) == 0 || !all(required_cols %in% colnames(collectorDictionary_checked))) {
#     stop("Empty Collector's Dictionary checked!")
#   }
#
#   # 4. Processamento mais eficiente do dicionário verificado
#   collectorDictionary_checked <- collectorDictionary_checked %>%
#     data.table::as.data.table()
#
#   # 5. Cálculo dos novos coletores ANTES do loop principal
#   collectorDictionary_checked_new <- collectorDictionary_checked[
#     !collectorDictionary, on = "Ctrl_recordedBy"
#   ] %>%
#     dplyr::select(dplyr::all_of(required_cols))
#
#   # 6. Pré-processamento da ocorrência
#   if (NROW(occ) == 0) stop("Occurrence is empty!")
#
#   occ <- occ %>%
#     dplyr::select(Ctrl_recordNumber, Ctrl_family, Ctrl_recordedBy, Ctrl_year) %>%
#     dplyr::mutate(
#       Ctrl_recordedBy = toupper(Ctrl_recordedBy),
#       Ctrl_nameRecordedBy_Standard = NA_character_
#     ) %>%
#     data.table::as.data.table()
#
#   # 7. **SUBSTITUIÇÃO DO LOOP POR JOIN VETORIZADO** - Maior ganho de performance
#
#   collectorDictionary_lookup <- collectorDictionary_checked %>%
#     dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard) %>%
#     dplyr::distinct(Ctrl_recordedBy, .keep_all = TRUE) # Remove duplicatas
#
#   # Join vetorizado em vez do loop
#   occ <- occ %>%
#     dplyr::left_join(collectorDictionary_lookup,
#                      by = "Ctrl_recordedBy",
#                      suffix = c("", ".y")) %>%
#     dplyr::mutate(
#       Ctrl_nameRecordedBy_Standard = dplyr::coalesce(Ctrl_nameRecordedBy_Standard.y,
#                                                      Ctrl_nameRecordedBy_Standard),
#       Ctrl_nameRecordedBy_Standard = dplyr::if_else(
#         is.na(Ctrl_nameRecordedBy_Standard),
#         "NOT-FOUND-COLLECTOR",
#         Ctrl_nameRecordedBy_Standard
#       )
#     ) %>%
#     dplyr::select(-Ctrl_nameRecordedBy_Standard.y)
#
#   # 8. Processamento final vetorizado
#   occ <- occ %>%
#     dplyr::mutate(
#       Ctrl_recordNumber_Standard = stringr::str_replace_all(Ctrl_recordNumber, "[^0-9]", ""),
#       Ctrl_recordNumber_Standard = dplyr::case_when(
#         is.na(Ctrl_recordNumber_Standard) | Ctrl_recordNumber_Standard == "" ~ "",
#         TRUE ~ as.character(as.integer(Ctrl_recordNumber_Standard))
#       ),
#       Ctrl_recordNumber_Standard = dplyr::if_else(
#         is.na(Ctrl_recordNumber_Standard), "", Ctrl_recordNumber_Standard
#       ),
#
#       # Chaves de coleção
#       Ctrl_key_family_recordedBy_recordNumber = paste(
#         toupper(trimws(Ctrl_family)),
#         Ctrl_nameRecordedBy_Standard,
#         Ctrl_recordNumber_Standard,
#         sep = '_'
#       ),
#
#       Ctrl_key_year_recordedBy_recordNumber = paste(
#         dplyr::if_else(is.na(Ctrl_year), "noYear", as.character(Ctrl_year)),
#         Ctrl_nameRecordedBy_Standard,
#         Ctrl_recordNumber_Standard,
#         sep = '_'
#       )
#     )
#
#   # 9. Resumo otimizado
#   res_in <- occ %>%
#     dplyr::count(Ctrl_key_family_recordedBy_recordNumber, name = "numberOfRecords") %>%
#     dplyr::arrange(dplyr::desc(numberOfRecords))
#
#   # 10. Retorno final
#   result_cols <- c('Ctrl_nameRecordedBy_Standard', 'Ctrl_recordNumber_Standard',
#                    'Ctrl_key_family_recordedBy_recordNumber', 'Ctrl_key_year_recordedBy_recordNumber')
#
#   list(
#     occ_collectorsDictionary = occ %>% dplyr::select(dplyr::all_of(result_cols)),
#     summary = res_in,
#     collectorsDictionary_add = collectorDictionary_checked_new
#   )
# }
