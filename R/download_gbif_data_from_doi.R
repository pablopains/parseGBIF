#' @title Download GBIF occurrence data from DOI
#'
#' @name download_gbif_data_from_doi
#'
#' @description Download and unzip GBIF occurrence data from DOI to be used by ParsGBIF functions
#'
#' @param gbif_doi_url The url of the GBIF DOI
#' @param folder Save folder
#' @param subfolder Save in subfolder
#' @param keep_only_occurrence_file Keep only occurrence.txt file
#' @param overwrite overwrite files
#'
#' @details Download GBIF occurrence data downloaded from DOI
#'
#' @return
#' list of downloaded and unzipped files
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{prepare_gbif_occurrence_data}}, \code{\link[parseGBIF]{extract_gbif_issue}}
#'
#' @importFrom downloader
#'
#' @examples
#' \donttest{
#'
#' library(ParsGBIF)
#'
#' help(download_gbif_data_from_doi)
#'
#' download_gbif_data_from_doi(gbif_doi_url='https://www.gbif.org/occurrence/download/0151470-230224095556074',
#'                             folder = 'c://dataGBIF//Achatocarpaceae')
#'
#' }
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

  li_dr_txt <- li_dr[seq(1, length(li_dr), 2)]

  url_file_zip_GBIF <- li_dr_txt[41]
  path_file_zip_GBIF <- paste0(folder,'\\dataGBIF.zip')

  if((!file.exists(path_file_zip_GBIF)) |
     overwrite == TRUE)
  {
    downloader::download(url_file_zip_GBIF, path_file_zip_GBIF, mode = "wb")

    utils::unzip(path_file_zip_GBIF, exdir = folder)

    if(keep_only_occurrence_file == TRUE)
    {
      files_tmp <- list.files(path =  folder, full.names = TRUE)
      # ind_files <- grepl(paste0('occurrence.txt|','dataGBIF.zip'), files_tmp)

      ind_del <- grepl('citations.txt|meta.xml|metadata.xml|multimedia.txt|rights.txt|verbatim.txt',
                       files_tmp)
      unlink(files_tmp[ind_del==TRUE],recursive = TRUE)

      unlink(paste0(folder,'\\dataset'),recursive = TRUE)

    }

  }

  files_tmp <- list.files(path =  folder, full.names = TRUE, recursive = TRUE)

  return(files_tmp)
}

