#' @title Get WCVP database
#'
#' @name wcvp_get_data
#'
#' @description Download World Checklist of Vascular Plants (WCVP) database
#'
#' @param url_source http://sftp.kew.org/pub/data-repositories/WCVP/
#' @param read_only_to_memory TRUE to in-memory read-only, not writing a copy to local disk
#' @param path_results download destination folder, if read_only_to_memory FALSE
#' @param update TRUE to update and load files, FALSE to keep local version and load files, if read_only_to_memory FALSE
#' @param load_distribution TRUE to load file with geographical distribution of species, if read_only_to_memory FALSE
#' @param load_rda_data Load the WCVP name database from the rda file distributed with the package. To ensure updates, it is recommended to reinstall the package frequently.
#'
#' @details http://sftp.kew.org/pub/data-repositories/WCVP/ This is the public SFTP (Secure File Transfer Protocol) site of the Royal Botanic Gardens, Kew. This space contains data resources publicly accessible to the user `anonymous'.  No password required for access. Use of data made available via this site may be subject to legal and licensing restrictions. The README in the top-level directory for each data resource provides specific information about its terms of use.
#'
#' @return list with two data frames: wcvp_names and wcvp_distribution
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{wcvp_check_name}}, \code{\link[parseGBIF]{wcvp_check_name_batch}}
#'
#' @examples
#' \donttest{
#' # load package
#' library(parseGBIF)
#'
#' help(wcvp_get_data)
#'
#' # Download wcvp database to local disk
#' path_data <- tempdir() # you can change this folder
#'
#' wcvp <- wcvp_get_data(url_source = 'http://sftp.kew.org/pub/data-repositories/WCVP/',
#'                  read_only_to_memory = FALSE,
#'                  path_results = path_data,
#'                  update = FALSE,
#'                  load_distribution = TRUE)
#'
#' names(wcvp)
#'
#' head(wcvp$wcvp_names)
#' colnames(wcvp$wcvp_names)
#'
#' head(wcvp$wcvp_distribution)
#' colnames(wcvp$wcvp_distribution)
#' }
#'
#' @import utils
#'
#' @export
wcvp_get_data <- function(url_source = "http://sftp.kew.org/pub/data-repositories/WCVP/",
                     read_only_to_memory = FALSE,
                     path_results = 'C:/parseGBIF',
                     update = FALSE,
                     load_distribution = FALSE,
                     load_rda_data = FALSE)
{
  require(dplyr)

  if(read_only_to_memory==FALSE)
  {
    # criar pasta para salvar raultados do dataset
    if (!dir.exists(path_results)){dir.create(path_results)}
    path_results <- paste0(path_results,'/dataWCVP')
    if (!dir.exists(path_results)){dir.create(path_results)}
  }else
  {
    path_results <- tempdir()
  }

  # ultima versao
  nomes <- 'wcvp.zip'
  destfile <- paste0(path_results,'/',nomes)


  # update?

  if(update == TRUE | read_only_to_memory == TRUE | (!file.exists(destfile)))
  {
    url_d <- paste0(url_source,'/',nomes)

    print(paste0('downloading: ',url_d))

    downloader::download(url = url_d, destfile = destfile, mode = "wb")

  }

  files <- paste0(path_results,'/','wcvp_names.csv')

  if(!file.exists(files))
  {

    utils::unzip(destfile, exdir = path_results)

  }

  print(paste0('loading: ', files))

  wcvp_names <- utils::read.table(files, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") %>%
    data.frame(stringsAsFactors = F) %>%
    dplyr::mutate(TAXON_NAME_U = taxon_name %>% toupper(),
                  TAXON_AUTHORS_U = taxon_authors %>% toupper() %>% gsub ("\\s+", "", .))


  print(paste0(' wcvp_names :', NROW(wcvp_names)))

  if(load_distribution == TRUE)
  {
    files <- paste0(path_results,'/','wcvp_distribution.csv')
    print(paste0('loading: ', files))
    wcvp_distribution <- utils::read.table(files, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") %>%
      data.frame(stringsAsFactors = F)
    print(paste0('wcvp_distribution :', NROW(wcvp_distribution)))
  }
  else
  {wcvp_distribution = NA}


  # if(read_only_to_memory==TRUE)
  # {
  #   files.rem <- list.files(path = path_results, full.names = TRUE)
  #   file.remove(files.rem)
  # }

  return(list(wcvp_names = wcvp_names,
              wcvp_distribution =  wcvp_distribution))

}
