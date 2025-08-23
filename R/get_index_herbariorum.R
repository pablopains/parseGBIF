#' @title Download Index Herbariorum data
#' @name get_index_herbariorum
#'
#' @description Download the complete Index Herbariorum dataset containing
#' information about herbaria worldwide, including codes, locations,
#' and collection statistics.
#'
#' @details
#' This function downloads the complete Index Herbariorum dataset from
#' the New York Botanical Garden's official CSV endpoint. The dataset
#' contains information about herbaria institutions worldwide, including
#' their standard codes, locations, contact information, and collection
#' statistics.
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
#' }
#'
#' @importFrom readr read_csv
#' @importFrom utils download.file
#' @export
get_index_herbariorum <- function() {

  url_csv <- "https://sweetgum.nybg.org/science/ih/herbarium-csv/"
  temp_file <- tempfile(fileext = ".csv")

  tryCatch({
    # Download do arquivo CSV
    utils::download.file(url_csv, temp_file, mode = "wb", quiet = TRUE)

    if (file.exists(temp_file) && file.size(temp_file) > 0) {
      herbarium_df <- readr::read_csv(temp_file, show_col_types = FALSE)
      message("✅ Successfully downloaded ", nrow(herbarium_df), " herbarium records")

      # Limpar arquivo temporário
      unlink(temp_file)

      return(herbarium_df)
    } else {
      stop("Downloaded file is empty or does not exist")
    }
  }, error = function(e) {
    # Limpar arquivo temporário em caso de erro
    if (file.exists(temp_file)) {
      unlink(temp_file)
    }
    stop("Failed to download Index Herbariorum data: ", e$message)
  })
}
