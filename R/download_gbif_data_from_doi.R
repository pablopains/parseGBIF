#' @title Download GBIF occurrence data from DOI
#' @name download_gbif_data_from_doi
#'
#' @description
#' Download and unzip GBIF occurrence data from DOI to be used by parseGBIF functions.
#' This function automates the process of accessing GBIF occurrence datasets through their
#' DOI URLs, downloading the compressed data, and extracting the relevant files.
#'
#' @param gbif_doi_url
#' Character. A GBIF download identifier in one of these formats:
#' `https://doi.org/10.15468/dl.xxxxxx`, bare DOI `10.15468/dl.xxxxxx`,
#' `https://www.gbif.org/occurrence/download/XXXXXXX`, or the direct
#' GBIF API ZIP URL.
#'
#' @param folder
#' Character. Directory path where files will be saved. If the folder doesn't exist,
#' it will be created automatically. Use forward slashes (/) for cross-platform compatibility.
#'
#' @param keep_only_occurrence_file
#' Logical. If `TRUE` (default), keeps only the occurrence.txt file and removes
#' auxiliary files (citations.txt, meta.xml, multimedia.txt, rights.txt, verbatim.txt,
#' and dataset folder) to save disk space.
#'
#' @param overwrite
#' Logical. If `TRUE`, overwrites existing files when re-downloading.
#' Default is `FALSE` to prevent accidental data loss.
#'
#' @details
#' ## Workflow:
#' 1. Resolves a GBIF DOI or extracts the GBIF download request key
#' 2. Builds the direct GBIF API ZIP download URL
#' 3. Extracts all files to the specified directory
#' 4. Optionally removes auxiliary files to keep only the main occurrence data
#'
#' ## File Management:
#' - Creates target directory recursively if it doesn't exist
#' - Skips download if occurrence.txt already exists (unless overwrite=TRUE)
#' - Uses cross-platform file paths for Windows, Mac, and Linux compatibility
#' - Returns list of all extracted files with full paths
#'
#' @return
#' Character vector. List of downloaded and unzipped files with full paths,
#' typically containing occurrence.txt and any other retained files.
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`prepare_gbif_occurrence_data()`] for preparing downloaded GBIF data,
#' [`extract_gbif_issue()`] for extracting GBIF data quality issues
#'
#' @examples
#' \donttest{
#' # Download GBIF data from DOI to temporary directory
#' tryCatch({
#'   downloaded_files <- download_gbif_data_from_doi(
#'     gbif_doi_url = 'https://doi.org/10.15468/dl.nbcqc6',
#'     folder = tempdir(),  # Use temporary directory for safe example
#'     keep_only_occurrence_file = TRUE,
#'     overwrite = FALSE
#'   )
#'
#'   # List downloaded files
#'   print(downloaded_files)
#' }, error = function(e) {
#'   message("Download failed: ", e$message)
#'   message("This example requires an active internet connection and valid GBIF DOI")
#' })
#' }
#'
#' @importFrom httr HEAD headers status_code user_agent timeout config
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_detect
#' @importFrom downloader download
#' @importFrom utils unzip
#' @export
download_gbif_data_from_doi <- function(gbif_doi_url,
                                        folder = '',
                                        keep_only_occurrence_file = TRUE,
                                        overwrite = FALSE) {

  gbif_doi_url <- trimws(gbif_doi_url)

  if (!nzchar(gbif_doi_url)) {
    stop("`gbif_doi_url` is empty.")
  }

  download_key <- NULL
  url_file_zip_GBIF <- NULL

  if (grepl("^10\\.15468/dl\\.[A-Za-z0-9]+/?$", gbif_doi_url)) {
    gbif_doi_url <- paste0("https://doi.org/", sub("/$", "", gbif_doi_url))
  }

  if (grepl("^https?://api\\.gbif\\.org/v1/occurrence/download/request/[^/?#]+\\.zip/?$", gbif_doi_url)) {
    url_file_zip_GBIF <- sub("/$", "", gbif_doi_url)
  } else if (grepl("^https?://occurrence-download\\.gbif\\.org/occurrence/download/request/[^/?#]+\\.zip/?$", gbif_doi_url)) {
    url_file_zip_GBIF <- sub("/$", "", gbif_doi_url)
  } else if (grepl("^https?://www\\.gbif\\.org/occurrence/download/[^/?#]+/?$", gbif_doi_url)) {
    download_key <- sub("^https?://www\\.gbif\\.org/occurrence/download/([^/?#]+)/?$", "\\1", gbif_doi_url)
  } else if (grepl("^https?://doi\\.org/10\\.15468/dl\\.[A-Za-z0-9]+/?$", gbif_doi_url)) {
    doi_response <- httr::HEAD(
      gbif_doi_url,
      httr::user_agent("parseGBIF download_gbif_data_from_doi"),
      httr::timeout(60),
      httr::config(followlocation = FALSE)
    )

    redirect_location <- httr::headers(doi_response)[["location"]]

    if (httr::status_code(doi_response) < 300 || httr::status_code(doi_response) >= 400 ||
        is.null(redirect_location) ||
        !grepl("^https?://www\\.gbif\\.org/occurrence/download/[^/?#]+/?$", redirect_location)) {
      stop("Could not resolve the GBIF DOI to an occurrence download page.")
    }

    download_key <- sub("^https?://www\\.gbif\\.org/occurrence/download/([^/?#]+)/?$", "\\1", redirect_location)
  } else {
    stop(
      paste0(
        "Invalid GBIF download identifier. Accepted formats: ",
        "`https://doi.org/10.15468/dl.xxxxxx`, bare DOI `10.15468/dl.xxxxxx`, ",
        "`https://www.gbif.org/occurrence/download/XXXXXXX`, or the direct GBIF API ZIP URL."
      )
    )
  }

  if (is.null(url_file_zip_GBIF)) {
    url_file_zip_GBIF <- paste0("https://api.gbif.org/v1/occurrence/download/request/", download_key, ".zip")
  }

  # Create folder if it doesn't exist
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  # Use file.path for cross-platform compatibility
  path_file_zip_GBIF <- file.path(folder, "dataGBIF.zip")
  occurrence_file <- file.path(folder, "occurrence.txt")

  # Download and extract if needed
  if (!file.exists(occurrence_file) | overwrite) {
    downloader::download(url_file_zip_GBIF, path_file_zip_GBIF, mode = "wb")
    utils::unzip(path_file_zip_GBIF, exdir = folder)

    if (keep_only_occurrence_file) {
      files_tmp <- list.files(path = folder, full.names = TRUE)
      ind_del <- grepl('citations.txt|meta.xml|metadata.xml|multimedia.txt|rights.txt|verbatim.txt|dataGBIF.zip',
                       files_tmp)
      unlink(files_tmp[ind_del == TRUE], recursive = TRUE)
      unlink(file.path(folder, "dataset"), recursive = TRUE)
    }
  }

  list.files(path = folder, full.names = TRUE, recursive = TRUE)
}

