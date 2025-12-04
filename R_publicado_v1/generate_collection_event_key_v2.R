#' @title Generate Collection Event Key Version 2
#'
#' @description
#' Generates standardized collection event keys for biological occurrence data by
#' matching recorded collector names against a collector dictionary and creating
#' unique identifiers based on family, collector, record number, and year.
#'
#' @param occ A data.frame containing occurrence records with required columns:
#'   \code{Ctrl_recordNumber}, \code{Ctrl_family}, \code{Ctrl_recordedBy},
#'   \code{Ctrl_year}. Default is NA.
#' @param collectorDictionary_checked_file Character string specifying the file path
#'   \code{Ctrl_recordNumber}, \code{Ctrl_family}, \code{Ctrl_recordedBy},
#'   \code{Ctrl_year}. Default is NA.
#' @param collectorDictionary_checked_file Character string specifying the file path
#'   to a pre-checked collector dictionary CSV file. Optional.
#' @param collectorDictionary_checked A data.frame containing a pre-loaded checked
#'   collector dictionary. Optional.
#' @param collectorDictionary_file Character string specifying the file path to a
#'   custom collector dictionary CSV file. Optional.
#' @param collectorDictionary A data.frame containing a pre-loaded collector
#'   dictionary. Optional.
#' @param silence Logical indicating whether to suppress progress messages.
#'   Default is TRUE (silent operation).
#'
#' @return A list with three components:
#' \itemize{
#'   \item \code{occ_collectorsDictionary}: A data.frame with standardized collector
#'     names and generated collection keys
#'   \item \code{summary}: A summary data.frame showing the frequency of each
#'     collection key
#'   \item \code{collectorsDictionary_add}: A data.frame containing new collector
#'     entries not found in the original dictionary
#' }
#'
#' @details
#' This function performs the following operations:
#' \enumerate{
#'   \item Loads and validates collector dictionaries from various sources
#'   \item Standardizes collector names using a reference dictionary
#'   \item Generates unique collection event keys based on taxonomic family,
#'         collector, and record number
#'   \item Provides summary statistics and identifies new collector entries
#' }
#'
#' The function uses vectorized operations for better performance and can handle
#' large datasets efficiently.
#'
#' @examples
#' \dontrun{
#' # Basic usage with occurrence data
#' occ_data <- data.frame(
#'   Ctrl_recordNumber = c("123", "456", "789"),
#'   Ctrl_family = c("Orchidaceae", "Fabaceae", "Poaceae"),
#'   Ctrl_recordedBy = c("JOHN SMITH", "MARIA SILVA", "CARLOS SANTOS"),
#'   Ctrl_year = c(2020, 2021, 2022)
#' )
#'
#' result <- generate_collection_event_key_v2(occ = occ_data, silence = FALSE)
#'
#' # Using a pre-checked collector dictionary file
#' result <- generate_collection_event_key_v2(
#'   occ = occ_data,
#'   collectorDictionary_checked_file = "checked_collectors.csv"
#' )
#' }
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @encoding UTF-8
#'
#' @importFrom dplyr bind_rows mutate select rename distinct left_join coalesce
#' @importFrom dplyr if_else arrange desc count all_of
#' @importFrom readr read_csv locale
#' @importFrom stringr str_replace_all
#' @importFrom data.table as.data.table
#' @importFrom stats complete.cases
#'
#' @export
#'
#' @seealso
#' \code{\link{read_csv}} for reading CSV files, \code{\link{data.table}} for
#' efficient data manipulation
#'
#' @keywords biology collections taxonomy standardization
#'
#' @references
#' GBIF Data Standards: \url{https://www.gbif.org/data-standards}
generate_collection_event_key_v2 <- function(occ = NA,
                                          collectorDictionary_checked_file = NULL,
                                          collectorDictionary_checked = NULL,
                                          collectorDictionary_file = NULL,
                                          collectorDictionary = NULL,
                                          silence = TRUE) {

  # 1. Carregamento mais eficiente dos dicionários
  if (!silence) print('Loading collectorDictionary...')

  if (is.null(collectorDictionary)) {
    if (!is.null(collectorDictionary_file)) {

      if (!silence) print("Lendo Github parseGBIF...")

      # Carregamento paralelo dos arquivos
      dict1 <- readr::read_csv('https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_1.csv',
                               locale = readr::locale(encoding = 'UTF-8'),
                               show_col_types = FALSE)
      dict2 <- readr::read_csv('https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_2.csv',
                               locale = readr::locale(encoding = 'UTF-8'),
                               show_col_types = FALSE)
      collectorDictionary <- dplyr::bind_rows(dict1, dict2)
      rm(dict1, dict2) # Libera memória
    }
  }

  # Validação mais rápida
  required_cols <- c('Ctrl_nameRecordedBy_Standard', 'Ctrl_recordedBy')

  if (NROW(collectorDictionary) == 0 || !all(required_cols %in% colnames(collectorDictionary))) {
    stop("Empty or invalid Collector's Dictionary!")
  }

  # 2. Otimização: processamento em lote do dicionário
  collectorDictionary <- collectorDictionary %>%
    # dplyr::mutate(Ctrl_recordedBy = toupper(Ctrl_recordedBy)) %>%
    dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy) %>%
    dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard) %>%
    dplyr::rename(Ctrl_nameRecordedBy_Standard_CNCFlora = Ctrl_nameRecordedBy_Standard) %>%
    data.table::as.data.table() # Conversão para data.table para joins mais rápidos

  # 3. Carregamento e processamento do dicionário verificado
  if (!is.null(collectorDictionary_checked_file)) {
    if (!silence) print("Carregamento e processamento do dicionário verificado...")

    collectorDictionary_checked <- readr::read_csv(collectorDictionary_checked_file,
                                                   locale = readr::locale(encoding = "UTF-8"),
                                                   show_col_types = FALSE)
  }

  if (NROW(collectorDictionary_checked) == 0 || !all(required_cols %in% colnames(collectorDictionary_checked))) {
    stop("Empty Collector's Dictionary checked!")
  }

  # 4. Processamento mais eficiente do dicionário verificado
  collectorDictionary_checked <- collectorDictionary_checked %>%
    data.table::as.data.table()

  # 5. Cálculo dos novos coletores ANTES do loop principal
  collectorDictionary_checked_new <- collectorDictionary_checked[
    !collectorDictionary, on = "Ctrl_recordedBy"
  ] %>%
    dplyr::select(dplyr::all_of(required_cols))

  # 6. Pré-processamento da ocorrência
  if (NROW(occ) == 0) stop("Occurrence is empty!")

  occ <- occ %>%
    dplyr::select(Ctrl_recordNumber, Ctrl_family, Ctrl_recordedBy, Ctrl_year) %>%
    dplyr::mutate(
      Ctrl_recordedBy = toupper(Ctrl_recordedBy),
      Ctrl_nameRecordedBy_Standard = NA_character_
    ) %>%
    data.table::as.data.table()

  # 7. **SUBSTITUIÇÃO DO LOOP POR JOIN VETORIZADO** - Maior ganho de performance

  collectorDictionary_lookup <- collectorDictionary_checked %>%
    dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard) %>%
    dplyr::distinct(Ctrl_recordedBy, .keep_all = TRUE) # Remove duplicatas

  # Join vetorizado em vez do loop
  occ <- occ %>%
    dplyr::left_join(collectorDictionary_lookup,
                     by = "Ctrl_recordedBy",
                     suffix = c("", ".y")) %>%
    dplyr::mutate(
      Ctrl_nameRecordedBy_Standard = dplyr::coalesce(Ctrl_nameRecordedBy_Standard.y,
                                                     Ctrl_nameRecordedBy_Standard),
      Ctrl_nameRecordedBy_Standard = dplyr::if_else(
        is.na(Ctrl_nameRecordedBy_Standard),
        "NOT-FOUND-COLLECTOR",
        Ctrl_nameRecordedBy_Standard
      )
    ) %>%
    dplyr::select(-Ctrl_nameRecordedBy_Standard.y)

  # 8. Processamento final vetorizado
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

      # Chaves de coleção
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

  # 9. Resumo otimizado
  res_in <- occ %>%
    dplyr::count(Ctrl_key_family_recordedBy_recordNumber, name = "numberOfRecords") %>%
    dplyr::arrange(dplyr::desc(numberOfRecords))

  # 10. Retorno final
  result_cols <- c('Ctrl_nameRecordedBy_Standard', 'Ctrl_recordNumber_Standard',
                   'Ctrl_key_family_recordedBy_recordNumber', 'Ctrl_key_year_recordedBy_recordNumber')

  list(
    occ_collectorsDictionary = occ %>% dplyr::select(dplyr::all_of(result_cols)),
    summary = res_in,
    collectorsDictionary_add = collectorDictionary_checked_new
  )
}
