#if (!require("pacman")) install.packages("pacman")
#pacman::p_load_gh(    
#    "utf8", 
#    "purrr", 
#    "slam", 
#    "data.table", 
#    "vctrs", 
#    "tibble", 
#    "cli", 
#    "rlang", 
#    "dplyr", 
#    "zoo", 
#    "textshape", 
#    "syuzhet, lexicon",
#    "trinker/lexicon",    
#    "trinker/textclean",
#    "downloader",
#    "dplyr"
#)

#devtools::install_version(
# package = "dplyr",
#   repos   = "http://cran.us.r-project.org")


devtools::install_version(
 package = "downloader",
   repos   = "http://cran.us.r-project.org")

devtools::install_version(
 package = "rscopus",
   repos   = "http://cran.us.r-project.org")


  
  source(paste0(getwd(),'/r/collectors_get_name.R'))
  source(paste0(getwd(),'/r/collectors_prepare_dictionary.R'))
  source(paste0(getwd(),'/r/collectors_prepare_dictionary_v2.R'))
  source(paste0(getwd(),'/r/EnumOccurrenceIssue.R'))
  source(paste0(getwd(),'/r/export_data_v2.3.R'))
  source(paste0(getwd(),'/r/extract_gbif_issue.R'))
  source(paste0(getwd(),'/r/generate_collection_event_key.R'))
  source(paste0(getwd(),'/r/parseGBIF_summary.R'))
  source(paste0(getwd(),'/r/prepare_gbif_occurrence_data.R'))
  source(paste0(getwd(),'/r/select_digital_voucher_v2.1.R'))
  source(paste0(getwd(),'/r/select_gbif_fields.R'))
  source(paste0(getwd(),'/r/standardize_scientificName.R'))
  source(paste0(getwd(),'/r/wcvp_check_name.R'))
  source(paste0(getwd(),'/r/wcvp_check_name_batch.R'))
  source(paste0(getwd(),'/r/wcvp_get_data.R'))

  source(paste0(getwd(),'/r/wcvp_get_data_v2.1.R'))

  library(dplyr)

  library(downloader)

  #library('textclean')



family_name <- 'Myrtaceae'#'Achatocarpaceae'

path_data <- paste0(getwd(),'/data')
path_dataGBIF <- paste0(getwd(),'/dataGBIF/',family_name)
path_dataWCVP <- paste0(getwd(),'/dataWCVP')

occ_file <- paste0(path_dataGBIF,'/occurrence.txt')
if(!file.exists(occ_file))
{
    occ_file_zip <- paste0(path_dataGBIF,'/occurrence.zip')
    utils::unzip(occ_file_zip, exdir = path_dataGBIF)
}

#occ_file <-'https://drive.google.com/file/d/18oQsGWPRJDcSJDr-ttrEZZ_gHUvnGjt2/view?usp=sharing'
occ <- prepare_gbif_occurrence_data(gbif_occurrece_file = occ_file, columns = 'standard')
  
#head(occ)
NROW(occ)

# EnumOccurrenceIssue.rda

load(paste0(path_data, '/EnumOccurrenceIssue.rda'))

NROW(EnumOccurrenceIssue)

gbif_issue <- extract_gbif_issue(occ = occ,
                                    enumOccurrenceIssue = EnumOccurrenceIssue)

names(gbif_issue)

file.name <- paste0(path_dataGBIF,'/parseGBIF_1_occ_issue.csv')
write.csv(gbif_issue$occ_gbif_issue,
            file.name, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")

names(gbif_issue)

#wcvp_names <-  wcvp_get_data_v2.1(read_only_to_memory = FALSE,
#                                     load_rda_data = TRUE,
#                                 path_results = path_dataWCVP)$wcvp_names


wcvp_names <-  wcvp_get_data_v2.1(read_only_to_memory = FALSE,
                             path_results = path_dataWCVP)$wcvp_names


#occ_file_zip <- paste0(path_dataWCVP,'/wcvp_names.zip')
#utils::unzip(occ_file_zip, exdir = path_dataWCVP, overwrite = TRUE)

# file_csv <- paste0(path_dataWCVP,'/wcvp_names.csv')
# wcvp_names <- wcvp_get_data_v2.1(path_results = path_dataWCVP)$wcvp_names
# NROW(wcvp_names)
# remove(file_csv)



 NROW(wcvp_names)

names.checked <- wcvp_check_name_batch(occ = occ, 
                                       wcvp_names =  wcvp_names,
                                       if_author_fails_try_without_combinations = TRUE,
                                       wcvp_selected_fields = 'standard',
                                       silence = FALSE)

names(names.checked)
NROW(names.checked)

file.name <- paste0(path_dataGBIF,'/parseGBIF_2_occ_wcvp_check_name.csv')

write.csv(names.checked$occ_wcvp_check_name,
            file.name, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")

  collectorsDictionary.dataset <- collectors_prepare_dictionary_v2(occ = occ,
                                                                           collectorDictionary_file = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary_parseGBIF.csv')
  
  file.name <- paste0(path_dataGBIF,'/','parseGBIF_3_collectorsDictionary_dataset.csv')
  write.csv(collectorsDictionary.dataset, 
            file.name, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")

  collectorsDictionary <- generate_collection_event_key(occ=occ,
                                                        # collectorDictionary_checked_file = file.collectorsDictionary.dataset.checked,
                                                        collectorDictionary_checked = collectorsDictionary.dataset,
                                                        collectorDictionary_file = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/collectorDictionary/CollectorsDictionary_parseGBIF.csv',
                                                        silence = FALSE)


file.name <- paste0(path_dataGBIF,'/parseGBIF_3_occ_collectorsDictionary.csv')
  write.csv(collectorsDictionary$occ_collectorsDictionary, file.name, 
            row.names = FALSE, fileEncoding = "UTF-8", na = "")
  
  
  file.name <- paste0(path_dataGBIF,'/parseGBIF_3_summary_collectorsDictionary.csv')
  write.csv(collectorsDictionary$summary, file.name, 
            row.names = FALSE, fileEncoding = "UTF-8", na = "")
  
  file.name <- paste0(path_dataGBIF,'/parseGBIF_3_collectorsDictionary_add.csv')
  write.csv(collectorsDictionary$collectorsDictionary_add, file.name, 
            row.names = FALSE, fileEncoding = "UTF-8", na = "")

#  file.name <- paste0(path_dataGBIF,'/parseGBIF_1_occ_data.csv')
#  occ <- readr::read_delim(file = file.name,
#                           delim = ',',
#                           locale = readr::locale(encoding = "UTF-8"),
#                           show_col_types = FALSE) %>% data.frame()
  
  occ <- prepare_gbif_occurrence_data(gbif_occurrece_file = occ_file, columns = 'standard')


  file.name <-paste0(path_dataGBIF,'/parseGBIF_1_occ_issue.csv')
  occ_gbif_issue <- readr::read_delim(file = file.name,
                                      delim = ',',
                                      locale = readr::locale(encoding = "UTF-8"),
                                      show_col_types = FALSE) %>% data.frame()
  
  file.name <- paste0(path_dataGBIF,'/parseGBIF_2_occ_wcvp_check_name.csv')
  names.checked <- readr::read_delim(file = file.name,
                                     delim = ',',
                                     locale = readr::locale(encoding = "UTF-8"),
                                     show_col_types = FALSE) %>% data.frame()
  
  file.name <- paste0(path_dataGBIF,'/parseGBIF_3_occ_collectorsDictionary.csv')
  occ_collectorsDictionary <- readr::read_delim(file = file.name,
                                                delim = ',', 
                                                locale = readr::locale(encoding = "UTF-8"),
                                                show_col_types = FALSE) %>%
    data.frame()

  key <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %>% unique()
  n_k <- NROW(key)
  i_key <- 1:n_k
  
  ind_k1 <- i_key <= (n_k/3)
  ind_k2 <- i_key > (n_k/3) & i_key <= ((n_k/3)*2)
  ind_k3 <- i_key > (n_k/3)*2
  n_k
  sum(ind_k1==TRUE) + sum(ind_k2==TRUE) + sum(ind_k3==TRUE)
  
  i_k_1 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k1==TRUE]
  i_k_2 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k2==TRUE]
  i_k_3 <- occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k3==TRUE]
  
  occ_collectorsDictionary[i_k_1==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% 
    unique() %>% NROW() +
    
    occ_collectorsDictionary[i_k_2==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% 
    unique() %>% NROW() +
    
    occ_collectorsDictionary[i_k_3==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% 
    unique() %>% NROW()
  
  occ_collectorsDictionary$Ctrl_key_family_recordedBy_recordNumber %>% 
    unique() %>% NROW()

 occ_digital_voucher_t1 <- select_digital_voucher_v2.1(occ = occ[i_k_1==TRUE,],
                                                        occ_gbif_issue = occ_gbif_issue[i_k_1==TRUE,],
                                                        occ_wcvp_check_name = names.checked[i_k_1==TRUE,],
                                                        occ_collectorsDictionary = occ_collectorsDictionary[i_k_1==TRUE,],
                                                        silence = FALSE)

  occ_digital_voucher_t2 <- select_digital_voucher_v2.1(occ = occ[i_k_2==TRUE,],
                                                        occ_gbif_issue = occ_gbif_issue[i_k_2==TRUE,],
                                                        occ_wcvp_check_name = names.checked[i_k_2==TRUE,],
                                                        occ_collectorsDictionary = occ_collectorsDictionary[i_k_2==TRUE,],
                                                        silence = FALSE)


  occ_digital_voucher_t3 <- select_digital_voucher_v2.1(occ = occ[i_k_3==TRUE,],
                                                        occ_gbif_issue = occ_gbif_issue[i_k_3==TRUE,],
                                                        occ_wcvp_check_name = names.checked[i_k_3==TRUE,],
                                                        occ_collectorsDictionary = occ_collectorsDictionary[i_k_3==TRUE,],
                                                        silence = FALSE)

  occ_digital <- list(all_data =  {} %>% data.frame(stringsAsFactors = FALSE),
                      useable_data_raw = {},
                      duplicates = {},
                      unusable_data_raw = {},
                      occ_digital_voucher = {},
                      occ_results = {})
  
  occ_digital$all_data <- rbind(occ_digital_voucher_t1$all_data,
                                occ_digital_voucher_t2$all_data,
                                occ_digital_voucher_t3$all_data)
  
  
  occ_digital$useable_data_raw <- rbind(occ_digital_voucher_t1$useable_data_raw,
                                        occ_digital_voucher_t2$useable_data_raw,
                                        occ_digital_voucher_t3$useable_data_raw)
  
  
  occ_digital$unusable_data_raw <- rbind(occ_digital_voucher_t1$unusable_data_raw,
                                         occ_digital_voucher_t2$unusable_data_raw,
                                         occ_digital_voucher_t3$unusable_data_raw)
  
  occ_digital$occ_digital_voucher <- rbind(occ_digital_voucher_t1$occ_digital_voucher,
                                           occ_digital_voucher_t2$occ_digital_voucher,
                                           occ_digital_voucher_t3$occ_digital_voucher)
  
  occ_digital$occ_results <- rbind(occ_digital_voucher_t1$occ_results,
                                   occ_digital_voucher_t2$occ_results,
                                   occ_digital_voucher_t3$occ_results)

NROW(occ_digital$all_data)

  file.name <- paste0(path_dataGBIF,'/parseGBIF_4_occ_digital_voucher.csv')
  write.csv(occ_digital$all_data,
            file.name, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")

  file.name <- paste0(path_dataGBIF,'/parseGBIF_4_occ_digital_voucher.csv')
  occ_digital_voucher <- readr::read_delim(file = file.name,
                                                delim = ',',
                                                locale = readr::locale(encoding = "UTF-8"),
                                                show_col_types = FALSE) %>% data.frame()

   key <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %>% unique()
   n_k <- NROW(key)
   i_key <- 1:n_k
    
   ind_k1 <- i_key <= (n_k/3)
   ind_k2 <- i_key > (n_k/3) & i_key <= ((n_k/3)*2)
   ind_k3 <- i_key > (n_k/3)*2
   n_k
   sum(ind_k1==TRUE) + sum(ind_k2==TRUE) + sum(ind_k3==TRUE)
    
   i_k_1 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k1==TRUE]
   i_k_2 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k2==TRUE]
   i_k_3 <- occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %in% key[ind_k3==TRUE]
    
   occ_digital_voucher[i_k_1==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% 
     unique() %>% NROW() +
      
   occ_digital_voucher[i_k_2==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% 
     unique() %>% NROW() +
      
    occ_digital_voucher[i_k_3==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% 
     unique() %>% NROW()
    
    occ_digital_voucher$Ctrl_key_family_recordedBy_recordNumber %>% 
      unique() %>% NROW()

  results_t1 <- export_data_v2.3(occ_digital_voucher_file = '',
                                occ_digital_voucher = occ_digital_voucher[i_k_1==TRUE,],
                                merge_unusable_data = TRUE,
                                silence = FALSE)


 results_t2 <- export_data_v2.3(occ_digital_voucher_file = '',
                                   occ_digital_voucher = occ_digital_voucher[i_k_2==TRUE,],
                                   merge_unusable_data = TRUE,
                                   silence = TRUE)

 results_t3 <- export_data_v2.3(occ_digital_voucher_file = '',
                                   occ_digital_voucher = occ_digital_voucher[i_k_3==TRUE,],
                                   merge_unusable_data = TRUE,
                                   silence = TRUE)

results <- list(    occ_all = {},
                        useable_data_merge = {},
                        useable_data_raw = {},
                        duplicates = {},
                        unusable_data_merge = {},
                        unusable_data_raw = {})
    
    results$all_data <- rbind(results_t1$all_data,
                              results_t2$all_data,
                              results_t3$all_data)  
    
    results$useable_data_merge <- rbind(results_t1$useable_data_merge,
                              results_t2$useable_data_merge,
                              results_t3$useable_data_merge)
    
    results$useable_data_raw <- rbind(results_t1$useable_data_raw,
                              results_t2$useable_data_raw,
                              results_t3$useable_data_raw)
    
    results$duplicates <- rbind(results_t1$duplicates,
                              results_t2$duplicates,
                              results_t3$duplicates)
    
    results$unusable_data_raw <- rbind(results_t1$unusable_data_raw,
                              results_t2$unusable_data_raw,
                              results_t3$unusable_data_raw)
    
    results$unusable_data_merge <- rbind(results_t1$unusable_data_merge,
                              results_t2$unusable_data_merge,
                              results_t3$unusable_data_merge)

  file.name <-  file.name <- paste0(path_dataGBIF,'/parseGBIF_5_occ_all_data.csv')
    write.csv(results$all_data,
              file.name, 
              row.names = FALSE, 
              fileEncoding = "UTF-8", 
              na = "")
    
        
    file.name <- paste0(path_dataGBIF,'/parseGBIF_5_occ_useable_data_merge.csv')
    write.csv(results$useable_data_merge,
              file.name, 
              row.names = FALSE, 
              fileEncoding = "UTF-8", 
              na = "")
    
    
    file.name <-  paste0(path_dataGBIF,'/parseGBIF_5_occ_useable_data_raw.csv')
    write.csv(results$useable_data_raw,
              file.name, 
              row.names = FALSE, 
              fileEncoding = "UTF-8", 
              na = "")
    
    
    file.name <-  paste0(path_dataGBIF,'/parseGBIF_5_occ_duplicates.csv')
    write.csv(results$duplicates,
              file.name, 
              row.names = FALSE, 
              fileEncoding = "UTF-8", 
              na = "")
    
    
    file.name <-  paste0(path_dataGBIF,'/parseGBIF_5_occ_unusable_data_merge.csv')
    write.csv(results$unusable_data_merge,
              file.name, 
              row.names = FALSE, 
              fileEncoding = "UTF-8", 
              na = "")
    
    file.name <-  paste0(path_dataGBIF,'/parseGBIF_5_occ_unusable_data_raw.csv')
    write.csv(results$results$unusable_data_raw,
              file.name, 
              row.names = FALSE, 
              fileEncoding = "UTF-8", 
              na = "")

names(gbif_issue)
head(gbif_issue$occ_gbif_issue)
NROW(occ)
print(path_data)
head(EnumOccurrenceIssue)
