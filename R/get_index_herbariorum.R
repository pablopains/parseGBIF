#' @title Download Index Herbariorum data
#' @name get_index_herbariorum
#'
#' @description Download the complete Index Herbariorum dataset containing
#' information about herbaria worldwide, including codes, locations,
#' and collection statistics.
#'
#' @param save_file Logical indicating whether to save the data to a CSV file.
#' Default is FALSE.
#' @param file_path Path where to save the CSV file if save_file is TRUE.
#' If NULL, uses a temporary file.
#' @param quiet Logical indicating whether to suppress progress messages.
#' Default is FALSE.
#'
#' @details
#' This function downloads the complete Index Herbariorum dataset from
#' the New York Botanical Garden's official CSV endpoint. The dataset
#' contains information about herbaria institutions worldwide, including
#' their standard codes, locations, contact information, and collection
#' statistics.
#'
#' The function uses multiple download methods to ensure reliability and
#' handles various HTTP issues that may occur during the download process.
#'
#' @return
#' A tibble containing the Index Herbariorum data with the following
#' typical columns:
#' \itemize{
#'   \item{Code}{Herbarium code (e.g., "NY", "RB", "K")}
#'   \item{Name}{Full name of the herbarium}
#'   \item{Country}{Country where the herbarium is located}
#'   \item{City}{City where the herbarium is located}
#'   \item{No. Specimens}{Number of specimens in the collection}
#'   \item{Date Founded}{Date when the herbarium was founded}
#'   \item{URL}{Website URL of the herbarium}
#'   \item{...}{Additional columns with detailed information}
#' }
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' \code{\link[parseGBIF]{preprocess_data}},
#' \code{\link[parseGBIF]{get_gbif_data}}
#'
#' @examples
#' \donttest{
#' # Download Index Herbariorum data
#' herbarium_data <- get_index_herbariorum()
#'
#' # View the structure of the data
#' head(herbarium_data)
#'
#' # Count herbaria by country
#' table(herbarium_data$Country)
#'
#' # Download and save to a specific file
#' herbarium_data <- get_index_herbariorum(
#'   save_file = TRUE,
#'   file_path = "index_herbariorum_data.csv"
#' )
#' }
#'
#' @importFrom httr GET content add_headers status_code
#' @importFrom readr read_csv
#' @importFrom dplyr tibble
#' @importFrom utils download.file
#' @export
get_index_herbariorum <- function(save_file = FALSE,
                                  file_path = NULL,
                                  quiet = FALSE) {
  
  # URL do CSV oficial do Index Herbariorum
  url_csv <- "https://sweetgum.nybg.org/science/ih/herbarium-csv/"
  
  if (!quiet) {
    message("Downloading Index Herbariorum data from: ", url_csv)
  }
  
  # Método 1: Tentar com httr GET primeiro
  tryCatch({
    headers <- httr::add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      `Accept` = "text/csv, text/plain, */*",
      `Accept-Language` = "en-US,en;q=0.5",
      `Connection` = "keep-alive",
      `Referer` = "https://sweetgum.nybg.org/science/ih/"
    )
    
    resposta <- httr::GET(url_csv, headers)
    
    if (httr::status_code(resposta) == 200) {
      conteudo_csv <- httr::content(resposta, as = "text", encoding = "UTF-8")
      
      if (nchar(conteudo_csv) > 0) {
        herbarium_df <- readr::read_csv(conteudo_csv, show_col_types = FALSE)
        
        if (!quiet) {
          message("✅ Successfully downloaded ", nrow(herbarium_df),
                  " records using HTTP GET method")
        }
        
        # Salvar arquivo se solicitado
        if (save_file) {
          if (is.null(file_path)) {
            file_path <- paste0("index_herbariorum_", format(Sys.Date(), "%Y%m%d"), ".csv")
          }
          utils::write.csv(herbarium_df, file_path, row.names = FALSE)
          if (!quiet) {
            message("💾 Data saved to: ", file_path)
          }
        }
        
        return(herbarium_df)
      }
    }
  }, error = function(e) {
    if (!quiet) {
      message("❌ HTTP GET method failed: ", e$message)
      message("Trying alternative download method...")
    }
  })
  
  # Método 2: Usar download.file como fallback
  temp_file <- tempfile(fileext = ".csv")
  
  tryCatch({
    utils::download.file(url_csv, temp_file, mode = "wb", quiet = quiet)
    
    if (file.exists(temp_file) && file.size(temp_file) > 0) {
      herbarium_df <- readr::read_csv(temp_file, show_col_types = FALSE)
      
      if (!quiet) {
        message("✅ Successfully downloaded ", nrow(herbarium_df),
                " records using download.file method")
      }
      
      # Salvar arquivo se solicitado
      if (save_file) {
        if (is.null(file_path)) {
          file_path <- paste0("index_herbariorum_", format(Sys.Date(), "%Y%m%d"), ".csv")
        }
        utils::write.csv(herbarium_df, file_path, row.names = FALSE)
        if (!quiet) {
          message("💾 Data saved to: ", file_path)
        }
      }
      
      # Limpar arquivo temporário
      unlink(temp_file)
      
      return(herbarium_df)
    }
  }, error = function(e) {
    if (!quiet) {
      message("❌ download.file method failed: ", e$message)
    }
    # Limpar arquivo temporário em caso de erro
    if (file.exists(temp_file)) {
      unlink(temp_file)
    }
  })
  
  # Se ambos os métodos falharem
  stop("Failed to download Index Herbariorum data. ",
       "Please check your internet connection and try again. ",
       "If the problem persists, the server may be temporarily unavailable.")
}