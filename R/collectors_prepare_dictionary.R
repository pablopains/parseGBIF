#' @title Prepare the list with the main collector's last name
#' @name collectors_prepare_dictionary
#'
#' @description Returns the list with the last name of the main collector associated with the unique key recordedBy.
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param collectorDictionary_file Collector dictionary file - point to a file on your local disk or upload via git at https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary.csv.
#' @details If recordedBy is present in the collector's dictionary, it returns the checked name, if not, it returns the last name of the main collector, extracted from the recordedBy field.
#' If recordedBy is present in the collector's dictionary, returns the main collector's last name associated with the single recordedBy key,
#' otherwise, returns the main collector's last name, extracted from the recordedBy field.
#' It is recommended to curate the main collector's surname, automatically extracted from the recordedBy field.
#' The objective is to standardize the last name of the main collector.
#' That the primary botanical collector of a sample is always recognized by the same last name, standardized in capital letters and non-ascii characters replaced
#'
#' @return
#' Ctrl_nameRecordedBy_Standard,
#' Ctrl_recordedBy,
#' Ctrl_notes,
#' collectorDictionary,
#' Ctrl_update,
#' collectorName,
#' Ctrl_fullName,
#' Ctrl_fullNameII,
#' CVStarrVirtualHerbarium_PersonDetails
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{collectors_get_name}}, \code{\link[parseGBIF]{update_collectorsDictionary}}
#'
#' @examples
#' \donttest{
#' help(collectors_prepare_dictionary)
#'
#' occ <- prepare_gbif_occurrence_data(gbif_occurrece_file =  'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt',
#'                                     columns = 'standard')
#'
#' collectorsDictionaryFromDataset <- collectors_prepare_dictionary(occ = occ,
#'                                                                 collectorDictionary_file =  'https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary.csv')
#'
#' colnames(collectorsDictionaryFromDataset)
#' head(collectorsDictionaryFromDataset)
#'
#' collectorDictionary_checked_file <- paste0(tempdir(),'/','collectorsDictionaryFromDataset.csv')
#'
#' collectorDictionary_checked_file
#'
#' write.csv(collectorsDictionaryFromDataset,
#'           collectorDictionary_checked_file,
#'           row.names = FALSE,
#'           fileEncoding = "UTF-8",
#'           na = "")
#'}
#' @export
collectors_prepare_dictionary <- function(occ=NA,
                                       collectorDictionary_file = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary.csv')
{

  require(stringr)
  # require(googlesheets4)
  require(dplyr)

  print('Loading collectorDictionary...')

  if(collectorDictionary_file=='' | is.na(collectorDictionary_file) )
  {
    stop("Invalid Collector's Dictionary!")

  }

  collectorDictionary <- readr::read_csv(collectorDictionary_file,
                                         locale = readr::locale(encoding = 'UTF-8'),
                                         show_col_types = FALSE)


  if(NROW(collectorDictionary)==0 | any(colnames(collectorDictionary) %in% c('Ctrl_nameRecordedBy_Standard',
                                                         'Ctrl_recordedBy',
                                                         'Ctrl_notes',
                                                         'collectorDictionary',
                                                         'Ctrl_update',
                                                         'collectorName',
                                                         'Ctrl_fullName',
                                                         'Ctrl_fullNameII',
                                                         'CVStarrVirtualHerbarium_PersonDetails'))==FALSE)
  {
    stop("CollectorDictionary is empty!")
  }

  collectorDictionary <- collectorDictionary %>%
    dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy %>% toupper()) %>%
    data.frame()

  if(NROW(occ)==0)
  {
    stop("Occurrence is empty!")
  }

   collectorDictionary <- collectorDictionary %>%
      dplyr::rename(Ctrl_nameRecordedBy_Standard_x = Ctrl_nameRecordedBy_Standard)

   print("Extracting the main collector's surname....")

   Ctrl_lastNameRecordedBy <- lapply(occ$Ctrl_recordedBy %>%
                                        toupper() %>%
                                        unique(),
                                     collectors_get_name) %>%
      do.call(rbind.data.frame, .)

   recordedBy_Standart <- data.frame(
      Ctrl_nameRecordedBy_Standard =  textclean::replace_non_ascii(toupper(Ctrl_lastNameRecordedBy[,1])),
      Ctrl_recordedBy = occ$Ctrl_recordedBy %>% toupper() %>% unique(),
      stringsAsFactors = FALSE)

   recordedBy_Standart <- dplyr::left_join(recordedBy_Standart,
                   collectorDictionary,
                   by = c('Ctrl_recordedBy')) %>%
      dplyr::mutate(collectorDictionary=ifelse(!is.na(Ctrl_nameRecordedBy_Standard_x),
                                       'checked',
                                       '')) %>%
      dplyr::mutate(Ctrl_nameRecordedBy_Standard = ifelse(collectorDictionary=='checked',
                                                          Ctrl_nameRecordedBy_Standard_x,
                                                          Ctrl_nameRecordedBy_Standard)) %>%
      dplyr::arrange(collectorDictionary, Ctrl_nameRecordedBy_Standard, Ctrl_recordedBy) %>%
   dplyr::mutate(Ctrl_notes = Ctrl_notes %>% as.character(),
                 Ctrl_update = Ctrl_update %>% as.character(),
                 Ctrl_nameRecordedBy_Standard = Ctrl_nameRecordedBy_Standard %>% as.character(),
                 Ctrl_recordedBy = Ctrl_recordedBy %>% as.character(),
                 collectorName = collectorName %>% as.character(),
                 Ctrl_fullName = Ctrl_fullName %>% as.character(),
                 Ctrl_fullNameII = Ctrl_fullNameII %>% as.character(),
                 CVStarrVirtualHerbarium_PersonDetails = CVStarrVirtualHerbarium_PersonDetails %>% as.character()) %>%
     # dplyr::select(Ctrl_notes,
     #               Ctrl_update,
     #               Ctrl_nameRecordedBy_Standard,
     #               Ctrl_recordedBy,
     #               collectorDictionary,
     #
     #               collectorName,
     #               Ctrl_fullName,
     #               Ctrl_fullNameII,
     #               CVStarrVirtualHerbarium_PersonDetails)
     dplyr::select(Ctrl_nameRecordedBy_Standard,
                   Ctrl_recordedBy,
                   Ctrl_notes,
                   collectorDictionary,
                   Ctrl_update,
                   collectorName,
                   Ctrl_fullName,
                   Ctrl_fullNameII,
                   CVStarrVirtualHerbarium_PersonDetails)

   return(recordedBy_Standart)
}
