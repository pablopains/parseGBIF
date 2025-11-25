#' @title Generate Collection Event Key for Duplicate Grouping
#' @name generate_collection_event_key
#'
#' @description
#' Generates a unique key to identify physical and digital duplicates of collection events.
#' Combines the primary collector's surname, collector number, and botanical family to create
#' a standardized key (family + recordedByStandardized + recordNumber_Standard) that enables
#' grouping duplicates from the same unique collection event.
#'
#' @param occ
#' Data frame. GBIF occurrence table with selected columns as returned by
#' `select_gbif_fields(columns = 'standard')`.
#'
#' @param collectorDictionary_checked_file
#' Character. Path to verified collector dictionary file.
#'
#' @param collectorDictionary_checked
#' Data frame. Verified collector dictionary data.
#'
#' @param collectorDictionary_file
#' Character. Path to base collector dictionary file. If empty, uses the default
#' dictionary from the parseGBIF GitHub repository.
#'
#' @param collectorDictionary
#' Data frame. Base collector dictionary data.
#'
#' @param silence
#' Logical. If `TRUE`, suppresses progress messages. Default is `TRUE`.
#'
#' @details
#' ## Fields Created:
#' - `Ctrl_nameRecordedBy_Standard`: Standardized collector surname
#' - `Ctrl_recordNumber_Standard`: Numeric-only collector number
#' - `Ctrl_key_family_recordedBy_recordNumber`: Primary collection event key
#' - `Ctrl_key_year_recordedBy_recordNumber`: Alternative key with year
#'
#' ## Process:
#' 1. Loads and merges collector dictionaries
#' 2. Standardizes collector names using verified dictionary
#' 3. Extracts numeric components from collector numbers
#' 4. Generates unique keys for collection event grouping
#' 5. Identifies new collectors for dictionary updates
#'
#' @return
#' A list with three components:
#' - `occ_collectorsDictionary`: Occurrence data with generated keys and standardized fields
#' - `summary`: Summary of record counts per collection event key
#' - `collectorsDictionary_add`: New collectors identified for dictionary updates
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @encoding UTF-8
#'
#' @seealso
#' [`collectors_get_name()`] for extracting collector names from recordedBy fields,
#' [`collectors_prepare_dictionary()`] for creating collector dictionaries
#'
#' @examples
#' \donttest{
#' # Generate collection event keys
#' result <- generate_collection_event_key(
#'   occ = occ_data,
#'   collectorDictionary_checked_file = 'collectorDictionary_checked.csv'
#' )
#'
#' # View results
#' names(result)
#' head(result$occ_collectorsDictionary)
#' head(result$summary)
#' }
#'
#' @importFrom dplyr select mutate filter count arrange anti_join
#' @importFrom readr read_csv
#' @importFrom stringr str_replace_all str_trim
#' @importFrom data.table as.data.table
#' @export
generate_collection_event_key <- function(occ=NA,
                                      collectorDictionary_checked_file = NULL,
                                      collectorDictionary_checked = NULL,
                                      collectorDictionary_file = NULL,
                                      collectorDictionary = NULL,
                                      silence = TRUE)
{

  print('Loading collectorDictionary...')

  # if(collectorDictionary_file=='' | is.na(collectorDictionary_file) )
  # {
  #   stop("Invalid Collector's Dictionary!")
  # }
  #
  # collectorDictionary <- readr::read_csv(collectorDictionary_file,
  #                                        locale = readr::locale(encoding = "UTF-8"),
  #                                        show_col_types = FALSE)

  if (!is.null(collectorDictionary_file))
  {

    if (is.null(collectorDictionary))
    {
      collectorDictionary <- rbind(readr::read_csv('https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_1.csv',
                                                   locale = readr::locale(encoding = 'UTF-8'),
                                                   show_col_types = FALSE),
                                   readr::read_csv('https://raw.githubusercontent.com/pablopains/parseGBIF/refs/heads/main/collectorDictionary/CollectorsDictionary_2.csv',
                                                   locale = readr::locale(encoding = 'UTF-8'),
                                                   show_col_types = FALSE))
    }
  }


  if(NROW(collectorDictionary)==0 | any(colnames(collectorDictionary) %in% c('Ctrl_nameRecordedBy_Standard',
                                                                             'Ctrl_recordedBy'
                                                                             # 'Ctrl_notes',
                                                                             # 'collectorDictionary',
                                                                             # 'Ctrl_update',
                                                                             # 'collectorName',
                                                                             # 'Ctrl_fullName',
                                                                             # 'Ctrl_fullNameII',
                                                                             # 'CVStarrVirtualHerbarium_PersonDetails'
                                                                             ))==FALSE)
  {
    stop("Empty Collector's Dictionary!")
  }

  collectorDictionary <- collectorDictionary %>%
    # dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy %>% toupper()) %>%
    data.table::as.data.table()


  if(! silence == TRUE)
  {
    print('Loading collectorDictionary checked...')
  }

  if( !is.null(collectorDictionary_checked_file) )
  {
    collectorDictionary_checked <- readr::read_csv(collectorDictionary_checked_file,
                                                   locale = readr::locale(encoding = "UTF-8"),
                                                   show_col_types = FALSE)
  }


  if(NROW(collectorDictionary_checked)==0 | any(colnames(collectorDictionary_checked) %in% c('Ctrl_nameRecordedBy_Standard',
                                                                             'Ctrl_recordedBy'
                                                                             # 'Ctrl_notes',
                                                                             # 'collectorDictionary',
                                                                             # 'Ctrl_update',
                                                                             # 'collectorName',
                                                                             # 'Ctrl_fullName',
                                                                             # 'Ctrl_fullNameII',
                                                                             # 'CVStarrVirtualHerbarium_PersonDetails'
                                                                             ))==FALSE)
  {
    stop("Empty Collector's Dictionary checked!")
  }


  collectorDictionary_checked <- collectorDictionary_checked %>%
    # dplyr::mutate(Ctrl_recordedBy = Ctrl_recordedBy %>% toupper(),
    #               Ctrl_nameRecordedBy_Standard = Ctrl_nameRecordedBy_Standard %>% toupper()) %>%
    data.table::as.data.table()


  if(NROW(occ)==0)
  {
    stop("Occurrence is empty!")
  }


   colunas <- colnames(collectorDictionary)

   collectorDictionary <- collectorDictionary %>%
      dplyr::rename(Ctrl_nameRecordedBy_Standard_CNCFlora = Ctrl_nameRecordedBy_Standard) %>%
      dplyr::select(Ctrl_recordedBy, Ctrl_nameRecordedBy_Standard_CNCFlora)

   # # collectorDictionary_checked$Ctrl_recordedBy <- collectorDictionary_checked$Ctrl_recordedBy %>%
   # #    toupper() %>% as.character()
   #
   # collectorDictionary$Ctrl_recordedBy <- collectorDictionary$Ctrl_recordedBy %>%
   #    toupper() %>% as.character()

   collectorDictionary_checked_new <- anti_join(collectorDictionary_checked,
                                       collectorDictionary,
                                       by = c('Ctrl_recordedBy')) %>%
      dplyr::select(colunas)

   ####

   occ <- occ %>%
     dplyr::select(Ctrl_recordNumber, Ctrl_family, Ctrl_recordedBy, Ctrl_year)

   occ <- occ %>%
      dplyr::mutate(Ctrl_nameRecordedBy_Standard='')

   recordedBy_unique <- occ$Ctrl_recordedBy %>% unique() %>%  as.factor()
   # recordedBy_unique <- recordedBy_unique %>% toupper()
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
        if(s%%10==0){print(paste0(s, ' de ',rt))}

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

