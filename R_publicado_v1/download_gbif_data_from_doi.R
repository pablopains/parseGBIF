#' @title Download GBIF occurrence data from DOI
#' @name download_gbif_data_from_doi
#'
#' @description
#' Download and unzip GBIF occurrence data from DOI to be used by parseGBIF functions.
#'
#' @param gbif_doi_url
#' Character. The URL of the GBIF DOI for the occurrence dataset.
#'
#' @param folder
#' Character. Directory path where files will be saved. If the folder doesn't exist, it will be created.
#'
#' @param keep_only_occurrence_file
#' Logical. If `TRUE` (default), keeps only the occurrence.txt file and removes auxiliary files.
#'
#' @param overwrite
#' Logical. If `TRUE`, overwrites existing files. Default is `FALSE`.
#'
#' @details
#' Downloads GBIF occurrence data from a DOI URL, extracts the compressed files,
#' and optionally cleans up auxiliary files to keep only the main occurrence data.
#'
#' @return
#' Character vector. List of downloaded and unzipped files with full paths.
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
#' library(parseGBIF)
#'
#' # Download GBIF data from DOI
#' downloaded_files <- download_gbif_data_from_doi(
#'   gbif_doi_url = 'https://www.gbif.org/occurrence/download/0151470-230224095556074',
#'   folder = 'c:/dataGBIF/Achatocarpaceae'
#' )
#'
#' # List downloaded files
#' print(downloaded_files)
#' }
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_detect
#' @importFrom downloader download
#' @importFrom utils unzip
#' @export
download_gbif_data_from_doi <- function(gbif_doi_url,
                                        folder = '',
                                        keep_only_occurrence_file = TRUE,
                                        overwrite = FALSE)
{

  if(!dir.exists(folder))
  {
    dir.create(folder)

    # if(!dir.exists(folder) & subfolder != '')
    # {dir.create(folder)}
  }

  li_dr <- xml2::read_html(gbif_doi_url) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # li_dr_txt <- li_dr[seq(1, length(li_dr), 2)]
  #
  # url_file_zip_GBIF <- li_dr_txt[42]


  index <- str_detect(li_dr,  "https://api.gbif.org/v1/occurrence/download/request/")
  index <- ifelse(is.na(index), FALSE, index)
  sum(index)

  url_file_zip_GBIF <- li_dr[index==TRUE]

  path_file_zip_GBIF <- paste0(folder,'\\dataGBIF.zip')

  # if( ( !file.exists(path_file_zip_GBIF)) | (!file.exists(paste0(folder,'\\occurrence.txt'))) |
  #    overwrite == TRUE)
  if( (!file.exists(paste0(folder,'\\occurrence.txt'))) |
      overwrite == TRUE)
  {
    downloader::download(url_file_zip_GBIF, path_file_zip_GBIF, mode = "wb")

    utils::unzip(path_file_zip_GBIF, exdir = folder)

    if(keep_only_occurrence_file == TRUE)
    {
      files_tmp <- list.files(path =  folder, full.names = TRUE)
      # ind_files <- grepl(paste0('occurrence.txt|','dataGBIF.zip'), files_tmp)

      ind_del <- grepl('citations.txt|meta.xml|metadata.xml|multimedia.txt|rights.txt|verbatim.txt|dataGBIF.zip',
                       files_tmp)
      unlink(files_tmp[ind_del==TRUE],recursive = TRUE)

      unlink(paste0(folder,'\\dataset'),recursive = TRUE)

    }

  }

  files_tmp <- list.files(path =  folder, full.names = TRUE, recursive = TRUE)

  return(files_tmp)
}

