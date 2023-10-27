#' @title Generating the collection event key
#' @name generate_collection_event_key
#'
#' @description This generates a key to identify the physical and digital duplicates, of a given collection event.
#' It combines the primary collector's surname, the collector's number and the botanical family, a key is created
#' (family + recordByStandardized + recordNumber_Standard) that allows grouping the duplicates of the same unique
#' collection event.
#'
#' It also identifiesnew collectors to be added to the collector dictionary and that can be reused in the future.
#'
#' Include recordedByStandardized field with verified main collector's last name.
#' Include recordNumber_Standard field with only numbers from recordNumber.
#' Create the collection event key to group duplicates in the key_family_recordedBy_recordNumber field,
#' composed of the fields: family + recordedByStandardized + recordNumber_Standard.
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param collectorDictionary_checked_file Verified collector dictionary file - point to a file on your local disk (use file or data frame)
#' @param collectorDictionary_checked Verified collector dictionary data frame (use file or data frame)
#' @param collectorDictionary_file Collector dictionary file - point to a file on your local disk or upload via git at https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary.csv.
#' @param silence if TRUE does not display progress messages
#'
#' @details Fields created for each incident record:
#' nameRecordedBy_Standard,
#' recordNumber_Standard,
#' key_family_recordedBy_recordNumber,
#' key_year_recordedBy_recordNumber
#'
#' @return list with three data frames:
#' occ_collectorsDictionary, with update result fields only,
#' summary and
#' CollectorsDictionary_add, with new collectors that can be added to the
#' collector dictionary that can be reused in the future.
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link[parseGBIF]{collectors_get_name}}, \code{\link[parseGBIF]{prepare_collectorsDictionary}}
#'
#' @examples
#' \donttest{
#' collectorsDictionaryFromDataset <- prepare_lastNameRecordedBy(occ=occ,
#'                                                               collectorDictionary_checked_file='collectorDictionary_checked.csv')
#'
#' names(collectorsDictionaryFromDataset)
#'
#' }
#'
#' @import stringr
#' @import dplyr
#' @import rscopus
#'
#' @export
generate_collection_event_key <- function(occ=NA,
                                      collectorDictionary_checked_file = NA,
                                      collectorDictionary_checked = NA,
                                      collectorDictionary_file = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary.csv',
                                      silence = TRUE)
{

  print('Loading collectorDictionary...')

  if(collectorDictionary_file=='' | is.na(collectorDictionary_file) )
  {
    stop("Invalid Collector's Dictionary!")
  }

  collectorDictionary <- readr::read_csv(collectorDictionary_file,
                                         locale = readr::locale(encoding = "UTF-8"),
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
    stop("Empty Collector's Dictionary!")
  }

  collectorDictionary_tmp <- collectorDictionary <- collectorDictionary %>%
    dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy %>% toupper()) %>%
    data.frame()

  if(! silence == TRUE)
  {
    print('Loading collectorDictionary checked...')
  }

  if( !is.na(collectorDictionary_checked_file) )
  {
    collectorDictionary_checked <- readr::read_csv(collectorDictionary_checked_file,
                                                   locale = readr::locale(encoding = "UTF-8"),
                                                   show_col_types = FALSE)
  }


  if(NROW(collectorDictionary_checked)==0 | any(colnames(collectorDictionary_checked) %in% c('Ctrl_nameRecordedBy_Standard',
                                                                             'Ctrl_recordedBy',
                                                                             'Ctrl_notes',
                                                                             'collectorDictionary',
                                                                             'Ctrl_update',
                                                                             'collectorName',
                                                                             'Ctrl_fullName',
                                                                             'Ctrl_fullNameII',
                                                                             'CVStarrVirtualHerbarium_PersonDetails'))==FALSE)
  {
    stop("Empty Collector's Dictionary checked!")
  }


  collectorDictionary_checked <- collectorDictionary_checked %>%
    dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy %>% toupper(),
                  Ctrl_nameRecordedBy_Standard = Ctrl_nameRecordedBy_Standard %>% toupper()) %>%
    data.frame()


  if(NROW(occ)==0)
  {
    stop("Occurrence is empty!")
  }


   colunas <- colnames(collectorDictionary)

   collectorDictionary <- collectorDictionary %>%
      dplyr::rename(Ctrl_nameRecordedBy_Standard_CNCFlora = Ctrl_nameRecordedBy_Standard) %>%
      dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard_CNCFlora)

   collectorDictionary_checked$Ctrl_recordedBy <- collectorDictionary_checked$Ctrl_recordedBy %>%
      toupper() %>% as.character()

   collectorDictionary$Ctrl_recordedBy <- collectorDictionary$Ctrl_recordedBy %>%
      toupper() %>% as.character()

   collectorDictionary_checked_new <- anti_join(collectorDictionary_checked,
                                       collectorDictionary,
                                       by = c('Ctrl_recordedBy')) %>%
      dplyr::select(colunas)

   ####
   collectorDictionary_checked_new <- rbind( collectorDictionary_checked_new %>%
                                               dplyr::select(colunas),
                                             collectorDictionary_tmp %>%
                                               dplyr::select(colunas)) %>%
     dplyr::arrange(Ctrl_nameRecordedBy_Standard)
   ####

   occ <- occ %>%
     dplyr::select(Ctrl_recordNumber, Ctrl_family, Ctrl_recordedBy, Ctrl_year)

   occ <- occ %>%
      dplyr::mutate(Ctrl_nameRecordedBy_Standard='')

   recordedBy_unique <- occ$Ctrl_recordedBy %>% unique() %>%  as.factor()
   recordedBy_unique <- recordedBy_unique %>% toupper()
   # NROW(recordedBy_unique)

   if(! silence == TRUE)
   {
     print("let's go...")
     # print(NROW(recordedBy_unique))
   }

   # atualizando tabela de occorencias

   rt <- NROW(recordedBy_unique)
   ri <- 0

   occ$Ctrl_recordedBy <- occ$Ctrl_recordedBy %>% toupper()

   r=recordedBy_unique[1]
   s <- 0

   for (r in recordedBy_unique)
   {
      ri <- ri + 1
      s <- s+1


      if (is.na(r)) {next}
      # index_occ <- (occ$Ctrl_recordedBy %>% toupper() %in% r) %>% ifelse(is.na(.), FALSE,.)
      index_occ <- (occ$Ctrl_recordedBy %in% r) %>% ifelse(is.na(.), FALSE,.)
      num_records <- NROW(occ[index_occ==TRUE,])
      index_ajusted <- (collectorDictionary_checked$Ctrl_recordedBy == r) %>% ifelse(is.na(.), FALSE,.)

      if(! silence == TRUE)
      {
        # print(paste0(ri, ' de ', rt, ' - ', r,' : ',num_records, ' registros' ))
        if(s%%1000==0){print(paste0(s, ' de ',rt))}

      }



      if (NROW(collectorDictionary_checked[index_ajusted==TRUE,]) == 0)
      {
         # occ[index_occ==TRUE, c('Ctrl_nameRecordedBy_Standard')] =
         #    data.frame(Ctrl_nameRecordedBy_Standard  = 'undefined collector')
         print(r)
         print('in ajusted')
         next
      }

      if (num_records == 0)
      {
         print(r)
         print('table')
         break
      }

      collectorDictionary_checked_tmp <- collectorDictionary_checked %>%
         dplyr::filter(index_ajusted) %>%
         dplyr::select(Ctrl_nameRecordedBy_Standard)

      # 09-09-2022
      collectorDictionary_checked_tmp <- collectorDictionary_checked_tmp[1,]

      # 18-10-21
      #pode-se ajustar aqui as duplicações

      occ[index_occ==TRUE, c('Ctrl_nameRecordedBy_Standard')] =
         data.frame(Ctrl_nameRecordedBy_Standard  = collectorDictionary_checked_tmp)

   }

   if(! silence == TRUE)
   {
      print('...finished!')
   }

   occ$Ctrl_recordNumber_Standard <- str_replace_all(occ$Ctrl_recordNumber, "[^0-9]", "")

   occ$Ctrl_recordNumber_Standard <- ifelse(is.na(occ$Ctrl_recordNumber_Standard) |
                                                   occ$Ctrl_recordNumber_Standard=='',"",occ$Ctrl_recordNumber_Standard  %>% strtoi())
   # tirar o NA do numero
   occ$Ctrl_recordNumber_Standard <- ifelse(is.na(occ$Ctrl_recordNumber_Standard),'',occ$Ctrl_recordNumber_Standard)

   occ$Ctrl_key_family_recordedBy_recordNumber <- ""
   occ <- occ %>%
      dplyr::mutate(Ctrl_key_family_recordedBy_recordNumber =
                       paste(Ctrl_family %>% toupper() %>% glue::trim(),
                             Ctrl_nameRecordedBy_Standard,
                             Ctrl_recordNumber_Standard,
                             sep='_'))

   occ$Ctrl_key_year_recordedBy_recordNumber <- ""
   occ <- occ %>%
      dplyr::mutate(Ctrl_key_year_recordedBy_recordNumber =
                       paste(ifelse(Ctrl_year %>% is.na() == TRUE, 'noYear',Ctrl_year)  %>% glue::trim(),
                             Ctrl_nameRecordedBy_Standard,
                             Ctrl_recordNumber_Standard,
                             sep='_'))

   # # numero de registros por frase saída in
   res_in <- occ %>% dplyr::count(paste0(Ctrl_key_family_recordedBy_recordNumber))
   colnames(res_in) <- c('Key',
                         'numberOfRecords')
   res_in <- res_in %>% dplyr::arrange_at(c('numberOfRecords'), desc )

   # print(occ$Ctrl_key_family_recordedBy_recordNumber %>% unique())
   return(list(occ_collectorsDictionary = occ %>%
                 dplyr::select(Ctrl_nameRecordedBy_Standard, Ctrl_recordNumber_Standard, Ctrl_key_family_recordedBy_recordNumber, Ctrl_key_year_recordedBy_recordNumber),
               summary  = res_in,
               collectorsDictionary_add = collectorDictionary_checked_new))

}

