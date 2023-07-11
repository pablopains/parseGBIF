#' @title Export of results
#' @name export_data
#'
#' @description from the selected digital voucher:
#' * select the taxonomic identification of the sample,
#' * Select geographic coordinates,
#' * Merge information between fields of duplicates of a sample,
#' * Compare the frequency of content in fields
#' * Generate data summary
#' * Export results
#'
#' @param occ_digital_voucher_file CSV fila result of function select_digital_voucher()$occ_digital_voucher
#' @param occ_digital_voucher data frame result of function select_digital_voucher()$occ_digital_voucher
#' @param merge_occ_out include records flagged out_to_recover in merge processing
#' @param fields_to_merge fields to merge
#' @param fields_to_compare fields to compare content frequency
#' @param fields_to_parse all fields
#'
#' @details Each data frame should be used as needed
#'
#' @return list with six or seven (merge_occ_out == TRUE) data frames
#' occ_all - all records processed with parseGBIF_dataset_result field indicating records flagged as in, out_to_recover, and duplicates.
#' occ_in_merge - records inside - samples with data merged between duplicates
#' occ_in_raw  - records inside - samples with original digital voucher data
#' occ_dup - duplicates
#' occ_out_to_recover_raw - records out - samples with original digital voucher data
#' occ_out_to_recover_merge  - records out - (only if lala == TRUE) samples with data merged between duplicates
#' summary
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @examples
#' \donttest{
#' help(export_data)
#' 
#' occ_results <- export_data(occ_digital_voucher_file = file.occ_digital_voucher, merge_occ_out = T)
#' 
#' names(occ_results)
#' 
#' NROW(occ_results$occ_all)
#' NROW(occ_results$occ_in_merge)
#' NROW(occ_results$occ_in_raw)
#' NROW(occ_results$occ_out_to_recover_merge)
#' NROW(occ_results$occ_out_to_recover_raw)
#' NROW(occ_results$occ_dup)
#' 
#' occ_results$summary
#' 
#'
#' }
#' @export
export_data <- function(occ_digital_voucher_file = '',
                        occ_digital_voucher = NA,
                        merge_occ_out = FALSE,
                        fields_to_merge = c('Ctrl_fieldNotes',
                                            'Ctrl_year',
                                            'Ctrl_stateProvince',
                                            'Ctrl_municipality',
                                            'Ctrl_locality',
                                            'Ctrl_countryCode',
                                            'Ctrl_eventDate',
                                            'Ctrl_habitat',
                                            'Ctrl_level0Name',
                                            'Ctrl_level1Name',
                                            'Ctrl_level2Name',
                                            'Ctrl_level3Name'),
                        
                        fields_to_compare = c('Ctrl_gbifID',
                                              'Ctrl_scientificName',
                                           'Ctrl_recordedBy',
                                           'Ctrl_recordNumber',
                                           'Ctrl_identifiedBy',
                                           'Ctrl_dateIdentified',
                                           'Ctrl_institutionCode',
                                           'Ctrl_collectionCode',
                                           'Ctrl_datasetName',
                                           'Ctrl_datasetName',
                                           'Ctrl_language',
                                           "wcvp_plant_name_id",
                                           "wcvp_taxon_rank",                                  
                                           "wcvp_taxon_status",
                                           "wcvp_family",
                                           "wcvp_taxon_name",
                                           "wcvp_taxon_authors",
                                           "wcvp_reviewed",
                                           "wcvp_searchNotes"),
                        
                        fields_to_parse = c('Ctrl_gbifID',
                                            'Ctrl_bibliographicCitation',
                                            'Ctrl_language',
                                            'Ctrl_institutionCode',
                                            'Ctrl_collectionCode',
                                            'Ctrl_datasetName',
                                            'Ctrl_basisOfRecord',
                                            'Ctrl_catalogNumber',	
                                            'Ctrl_recordNumber',
                                            'Ctrl_recordedBy',
                                            'Ctrl_occurrenceStatus',
                                            'Ctrl_eventDate',	
                                            'Ctrl_year',	
                                            'Ctrl_month',	
                                            'Ctrl_day',
                                            'Ctrl_habitat',
                                            'Ctrl_fieldNotes',
                                            'Ctrl_eventRemarks',
                                            'Ctrl_countryCode',	
                                            'Ctrl_stateProvince',	
                                            'Ctrl_municipality',
                                            'Ctrl_county',
                                            'Ctrl_locality',
                                            
                                            'Ctrl_level0Name',
                                            'Ctrl_level1Name',
                                            'Ctrl_level2Name',
                                            'Ctrl_level3Name',

                                            'Ctrl_identifiedBy',
                                            'Ctrl_dateIdentified',
                                            'Ctrl_scientificName',
                                            'Ctrl_decimalLatitude',
                                            'Ctrl_decimalLongitude',

                                            'Ctrl_nameRecordedBy_Standard',	
                                            'Ctrl_recordNumber_Standard',	
                                            'Ctrl_key_family_recordedBy_recordNumber',
                                            'Ctrl_geospatial_quality',
                                            'Ctrl_verbatim_quality',
                                            'Ctrl_moreInformativeRecord',
                                            'Ctrl_coordinates_validated_by_gbif_issue',	
                                            
                                            "wcvp_plant_name_id",
                                            "wcvp_taxon_rank",                                  
                                            "wcvp_taxon_status",
                                            "wcvp_family",
                                            "wcvp_taxon_name",
                                            "wcvp_taxon_authors",
                                            "wcvp_reviewed",
                                            "wcvp_searchNotes",
                                            
                                            'parseGBIF_digital_voucher',	
                                            'parseGBIF_duplicates',
                                            'parseGBIF_num_duplicates',
                                            'parseGBIF_non_groupable_duplicates',	
                                            'parseGBIF_duplicates_grouping_status')
                        )
{
  
  print('Loading occurrence file...')
  if(occ_digital_voucher_file !=''  )
  {
    if(!file.exists(occ_digital_voucher_file))
    {
      stop("Invalid occurrence file!")
    }
    
    occ_tmp <- readr::read_csv(occ_digital_voucher_file,
                               locale = readr::locale(encoding = "UTF-8"),
                               show_col_types = FALSE) %>% as.data.frame()
  }else
  {
    
    if (NROW(occ_digital_voucher)==0)
    {
      stop("Empty occurrence data frame!")
    }
    
    occ_tmp <- occ_digital_voucher %>% as.data.frame()
    rm(occ_digital_voucher)
  }
  
  
  # preparacao dos dados
  {
    occ_tmp <- occ_tmp %>%
      dplyr::select(fields_to_parse)
    
    occ_tmp <- occ_tmp %>%
      dplyr::mutate(
        parseGBIF_unidentified_sample = TRUE,
        parseGBIF_sample_taxon_name = '',
        parseGBIF_sample_taxon_name_status = '',
        parseGBIF_number_taxon_names = 0,
        parseGBIF_useful_for_spatial_analysis = FALSE,
        parseGBIF_decimalLatitude = NA,
        parseGBIF_decimalLongitude = NA,
        # parseGBIF_notes = '',
        # parseGBIF_status = FALSE,
        parseGBIF_freq_duplicate_or_missing_data = '')
    
    occ_tmp <- occ_tmp %>%
      dplyr::arrange(Ctrl_key_family_recordedBy_recordNumber)
  }
  
  
  print('Selecting...')
  recordedBy_unique <- occ_tmp$Ctrl_key_family_recordedBy_recordNumber %>% unique()
  tot <- NROW(recordedBy_unique)
  s<-0
  
  # r <- 'ACHATOCARPACEAE_ZARDINI_5592'
  # r <- 'ACHATOCARPACEAE_AGUILAR_1486'
  
  for (r in recordedBy_unique)
  {
    s <- s+1
    
    if (s%%100==0){print(paste0(s, ' de ',tot))}
    
    sp_name <- ''
    
    index_occ <- (occ_tmp$Ctrl_key_family_recordedBy_recordNumber %in% r) %>% ifelse(is.na(.), FALSE,.)
    
    # nomes e coorrdenadas para não agrupaveis
    if(any(occ_tmp$parseGBIF_non_groupable_duplicates[index_occ==TRUE]) == TRUE)
    {
      # incluir filtro espacial
      
      # nomes
      sp_name <- ifelse(ifelse(is.na(occ_tmp[index_occ==TRUE, ]$wcvp_taxon_status == 'Accepted'), FALSE,TRUE),
                        occ_tmp[index_occ==TRUE, ]$wcvp_taxon_name %>% as.character(),
                        '')
      
      # incluir familia, autor...
      
      occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
        dplyr::mutate(
          parseGBIF_sample_taxon_name = sp_name,
          parseGBIF_unidentified_sample = ifelse(sp_name %in% '', TRUE,FALSE),
          parseGBIF_number_taxon_names = ifelse(sp_name %in% '',
                                                0,
                                                1),
          parseGBIF_sample_taxon_name_status = ifelse(sp_name %in% '',
                                                      'unidentified',
                                                      'identified'),
          
          parseGBIF_decimalLatitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue==TRUE,
                                             Ctrl_decimalLatitude,
                                             NA),
          
          parseGBIF_decimalLongitude = ifelse(Ctrl_coordinates_validated_by_gbif_issue==TRUE,
                                              Ctrl_decimalLongitude,
                                              NA),
          
          parseGBIF_useful_for_spatial_analysis = Ctrl_coordinates_validated_by_gbif_issue
          
        )
      
      
      # occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
      #   dplyr::mutate( parseGBIF_notes = ifelse(parseGBIF_useful_for_spatial_analysis ==FALSE & parseGBIF_unidentified_sample==TRUE,
      #                                           'no name for species and no coordinates', 
      #                  ifelse(parseGBIF_useful_for_spatial_analysis ==FALSE & parseGBIF_unidentified_sample==FALSE,
      #                         'no coordinates', 
      #                  ifelse(parseGBIF_useful_for_spatial_analysis == TRUE & parseGBIF_unidentified_sample==TRUE,
      #                         'no name for species', 'OK'))),
      #                  parseGBIF_status = parseGBIF_useful_for_spatial_analysis & !(parseGBIF_unidentified_sample))
      
      # occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
      #   dplyr::mutate( parseGBIF_notes = ifelse(parseGBIF_useful_for_spatial_analysis ==FALSE & parseGBIF_unidentified_sample==TRUE,
      #                                           'no tax. ident. and no geo coord.', 
      #                                           ifelse(parseGBIF_useful_for_spatial_analysis ==FALSE & parseGBIF_unidentified_sample==FALSE,
      #                                                  'no geo coord.', 
      #                                                  ifelse(parseGBIF_useful_for_spatial_analysis == TRUE & parseGBIF_unidentified_sample==TRUE,
      #                                                         'no tax. ident.', 'OK'))),
      #                  parseGBIF_status = parseGBIF_useful_for_spatial_analysis & !(parseGBIF_unidentified_sample))
      
      # print( occ_tmp[index_occ==TRUE, ]$parseGBIF_sample_taxon_name_status)
      # print( occ_tmp[index_occ==TRUE, ]$parseGBIF_number_taxon_names)
      # print( occ_tmp[index_occ==TRUE, ]$parseGBIF_sample_taxon_name)
      
      # print('1 - Unmatched samples')
      next
    } # cabe um else
    
    # nomes para agrupaveis
    { 
      
      num_records <- NROW(occ_tmp[index_occ==TRUE,])
      
      if(!any(is.na(occ_tmp[index_occ==TRUE, ]$wcvp_taxon_name) == FALSE)) # any ???
      {
        
        # nomes
        sp_name <- rep('',num_records)
        
        occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
          dplyr::mutate(
            parseGBIF_number_taxon_names = 0,
            parseGBIF_sample_taxon_name = sp_name,
            parseGBIF_sample_taxon_name_status = 'unidentified')
        
      }else
      {
        
        taxon_name_sample <- table(occ_tmp[index_occ==TRUE, ]$wcvp_taxon_name,
                                   occ_tmp[index_occ==TRUE, ]$wcvp_taxon_status,
                                   exclude = NA) %>%
          data.frame() %>%
          dplyr::filter(Freq > 0) %>%
          dplyr::arrange(desc(Freq),Var1)
        
        num_taxon_name <- NROW(taxon_name_sample)
        
        if(num_taxon_name==0)
        {
          
          print(occ_tmp[index_occ==TRUE, ]$wcvp_taxon_name)
          
          print('0 - Error')
          
          break
        }
        
        
        if(num_taxon_name==1 & taxon_name_sample$Var2[1] %in% c('Accepted'))#,'Updated'))
        {
          sp_name <- taxon_name_sample$Var1[1] %>% as.character()
          
          occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
            dplyr::mutate(
              parseGBIF_number_taxon_names = num_taxon_name,
              parseGBIF_sample_taxon_name = sp_name,
              parseGBIF_sample_taxon_name_status = 'identified',
              parseGBIF_unidentified_sample = FALSE
            )
          
          # print('3 - Identified sample 100 %')
          
        }
        
        
        if(num_taxon_name>1)
        {
          ii=1
          for(ii in 1:NROW(taxon_name_sample))
          {
            
            if(taxon_name_sample$Var2[ii] %in% c('Accepted'))#,'Updated'))
            {
              sp_name <- taxon_name_sample$Var1[ii] %>% as.character()
              
              occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
                dplyr::mutate(
                  parseGBIF_number_taxon_names = num_taxon_name,
                  parseGBIF_sample_taxon_name = sp_name,
                  parseGBIF_sample_taxon_name_status = 'divergent identifications',
                  parseGBIF_unidentified_sample = FALSE)
              
              
              # print(paste0('4 - Identified sample ', 100/num_taxon_name,' %'))
              
              break
            }
          }
        }
      }
    }
    
    # coorrdenadas para agrupaveis
    {
      
      index_voucher <- occ_tmp[index_occ==TRUE,]$parseGBIF_digital_voucher == TRUE
      
      # se voucher tem coordennadas válidas
      if(occ_tmp$Ctrl_coordinates_validated_by_gbif_issue[index_occ==TRUE][index_voucher == TRUE] == TRUE)
      {
        
        occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
          dplyr::mutate(parseGBIF_decimalLatitude = occ_tmp$Ctrl_decimalLatitude[index_occ==TRUE][index_voucher == TRUE],
                        parseGBIF_decimalLongitude = occ_tmp$Ctrl_decimalLongitude[index_occ==TRUE][index_voucher == TRUE],
                        parseGBIF_useful_for_spatial_analysis = TRUE)
        
      }else # se voucher não tem coordennadas válidas, tentar pegar das coordenadas
      { 
        
        index_no_voucher_useful_for_spatial_analysis <- occ_tmp[index_occ==TRUE,]$Ctrl_coordinates_validated_by_gbif_issue == TRUE 
        
        if(sum(index_no_voucher_useful_for_spatial_analysis)==1)
        {
          
          occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
            dplyr::mutate(parseGBIF_decimalLatitude = occ_tmp$Ctrl_decimalLatitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE],
                          parseGBIF_decimalLongitude = occ_tmp$Ctrl_decimalLongitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE],
                          parseGBIF_useful_for_spatial_analysis = TRUE)
          
        }
        
        # pegar a com maior score
        if(sum(index_no_voucher_useful_for_spatial_analysis)>1)
        {
          
          geospatial_quality_tmp <- max(occ_tmp$Ctrl_geospatial_quality[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE])
          
          index_geospatial_quality <- occ_tmp$Ctrl_geospatial_quality[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE] == geospatial_quality_tmp
          
          occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
            dplyr::mutate(parseGBIF_decimalLatitude = occ_tmp$Ctrl_decimalLatitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE][index_geospatial_quality == TRUE][1],
                          parseGBIF_decimalLongitude = occ_tmp$Ctrl_decimalLongitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE][index_geospatial_quality == TRUE][1],
                          parseGBIF_useful_for_spatial_analysis = TRUE)
          
        }  
        
      }
      
    }
    
    # occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
    #   dplyr::mutate( parseGBIF_notes = ifelse(parseGBIF_useful_for_spatial_analysis ==FALSE & parseGBIF_unidentified_sample==TRUE,
    #                                           'no tax. ident. and no geo coord.', 
    #                                           ifelse(parseGBIF_useful_for_spatial_analysis ==FALSE & parseGBIF_unidentified_sample==FALSE,
    #                                                  'no geo coord.', 
    #                                                  ifelse(parseGBIF_useful_for_spatial_analysis == TRUE & parseGBIF_unidentified_sample==TRUE,
    #                                                         'no tax. ident.', 'OK'))),
    #                  parseGBIF_status = parseGBIF_useful_for_spatial_analysis & !(parseGBIF_unidentified_sample))
    
    
  }
  
  # in dup out_to_recover
  {
    # NROW(occ_tmp)
    occ_tmp <- occ_tmp %>%
      dplyr::mutate(parseGBIF_duplicates_map = rep('',NROW(occ_tmp)),
                    parseGBIF_merged_fields = rep('',NROW(occ_tmp)),
                    parseGBIF_merged = rep(FALSE,NROW(occ_tmp)))
    
    occ_in <- occ_tmp %>%
      dplyr::filter(parseGBIF_digital_voucher == TRUE,
                    parseGBIF_unidentified_sample == FALSE, # parseGBIF_sample_taxon_name_status %in% c('identified','divergent identifications'),
                    parseGBIF_useful_for_spatial_analysis == TRUE) # %>%
    # dplyr::mutate(parseGBIF_origin='occ in')
    
    # NROW(occ_in)
    # occ_dup <- dplyr::anti_join(occ_tmp %>%
    #                               dplyr::filter(parseGBIF_digital_voucher == FALSE), occ_in, "Ctrl_gbifID")
    
    occ_dup <- occ_tmp %>%
      dplyr::filter(parseGBIF_digital_voucher == FALSE) #%>%
    # dplyr::mutate(parseGBIF_origin='occ dup')
    
    # sum(occ_dup$Ctrl_key_family_recordedBy_recordNumber %in% 'ACHATOCARPACEAE_ZARDINI_5592')
    
    occ_out_to_recover <- occ_tmp %>%
      dplyr::filter(parseGBIF_digital_voucher == TRUE,
                    (parseGBIF_unidentified_sample == TRUE |
                       parseGBIF_useful_for_spatial_analysis == FALSE)) #%>%
    # dplyr::mutate(parseGBIF_origin='occ out to recover')
    
    # remove(occ_tmp)
    
  }
  
  print('Merging...')
  
  # merge and compare
  {
    
    # fields_to_merge
    # fields_to_compare
    
    if(merge_occ_out==TRUE)
    {
      occ_res_full <-  rbind(occ_in,occ_out_to_recover)
      # occ_res_full <-  occ_res_full %>%
      #   dplyr::mutate(parseGBIF_duplicates_map = rep('',NROW(occ_res_full)),
      #                 parseGBIF_merged_fields = rep('',NROW(occ_res_full)),
      #                 parseGBIF_merged = rep(FALSE,NROW(occ_res_full)))
    }else
    {
      occ_res_full <-  occ_in
      # occ_res_full <-  occ_res_full %>%
      #   dplyr::mutate(parseGBIF_duplicates_map = rep('',NROW(occ_res_full)),
      #                 parseGBIF_merged_fields = rep('',NROW(occ_res_full)),
      #                 parseGBIF_merged = rep(FALSE,NROW(occ_res_full)))    
    }
    
    key <- occ_res_full$Ctrl_key_family_recordedBy_recordNumber %>%
      unique()
    
    tot <- NROW(key)
    fields_to_all <- c(fields_to_compare,fields_to_merge)
    
    i = 52
    i=1
    for(i in 1:tot)
    {
      
      index <- occ_res_full$Ctrl_key_family_recordedBy_recordNumber %in% key[i]
      
      if(occ_res_full$parseGBIF_duplicates[index==TRUE][1]==TRUE)
      {
        print(paste0(i, ' - ', tot, ' - ', key[i]))
        
        index_dup <- occ_dup$Ctrl_key_family_recordedBy_recordNumber %in% key[i]
        
        if(sum(index_dup)==0)
        {next}
        
        ic <- 1
        
        x_jonsom_full <- ""
        parseGBIF_merged_fields <- ""
        
        freq_data_col_full <- ""
        
        for ( ic in 1:length(fields_to_all))
        {
          # print(ic)
          
          data_col <- occ_res_full[index==TRUE,
                                   fields_to_all[ic]] 
          
          if(is.na(data_col))
          {
            data_col <- ""
          }
          
          x_data_col <- toupper(data_col)
          
          data_col_dup <- occ_dup[index_dup==TRUE,
                                  fields_to_all[ic]] 
          
          freq_data_col <- table(freq_tmp <- data.frame(value= c(occ_res_full[index==TRUE,fields_to_all[ic]], 
                                                                 occ_dup[index_dup==TRUE, fields_to_all[ic]])),
                                 exclude = NA) %>%
            data.frame() %>%
            dplyr::arrange(desc(Freq)) 
          
          if(NROW(freq_data_col)==0)
          {
            freq_data_col <- data.frame(value = 'empty',
                                        freq = occ_dup[index_dup==TRUE, 'parseGBIF_num_duplicates'][1])
          }else
          {
            colnames(freq_data_col) <- c('value','freq')
            freq_tmp <- occ_dup[index_dup==TRUE, 'parseGBIF_num_duplicates'][1] - sum(freq_data_col$freq) 
            if(freq_tmp>0) #freq_data_col$freq[freq_data_col$value=='empty']>0)
            {
              freq_data_col <- freq_data_col %>%
                dplyr::add_row(value = 'empty',
                               freq = freq_tmp)
            }
          }
          
          # x <- paste0('{"',fields_to_all[ic],'":',jsonify::to_json( freq_data_col),"}" )
          x <- paste0('"',fields_to_all[ic],'":',jsonify::to_json( freq_data_col))
          
          freq_data_col_full <- paste0(freq_data_col_full,
                                       ifelse(freq_data_col_full=="","",","),
                                       x)
          
          # stop()
          
          ix <- 1
          # x <- ""
          x_jonsom <- ""
          
          for(ix in 1:NROW(data_col_dup))
          {
            
            if(is.na(data_col_dup[ix]))
            {
              next
            }
            
            x_Ctrl_gbifID_dup <- occ_dup[index_dup==TRUE, 'Ctrl_gbifID'][ix]
            
            x_data_col_dup <- toupper(data_col_dup[ix])
            
            if(x_data_col == x_data_col_dup)
            {
              next
            }
            
            # # nao armazear dados repetidos entre duplicatas
            # if(any(data_col_dup[ix] == data_col_dup[-ix] %>%
            #        ifelse(is.na(.), FALSE,.) )==TRUE )
            if(grepl(data_col_dup[ix], x_jonsom) %>% ifelse(is.na(.), FALSE,.))
            {
              next
            }
            
            
            # armazena informações novas para o campo
            # cabeçalho
            
            if(x_jonsom=="") #aqui
            {
              # # if(data_col %>% as.character() !="")
              # if(fields_to_all[ic] == "Ctrl_gbifID" )
              # {
              x_jonsom <-  paste0('"',fields_to_all[ic],'"',":[",'"', gsub('"','',data_col),'"')
              # }else
              # {
              # x_jonsom <-  paste0('"',fields_to_all[ic],'"',":[")
              # }
            }
            
            # dados
            if(substr(x_jonsom,str_count(x_jonsom),str_count(x_jonsom)) == '[')
            {
              x_jonsom <- paste0(x_jonsom, '"', gsub('"','',data_col_dup[ix]),';[',x_Ctrl_gbifID_dup,']"')
            }else
            {
              x_jonsom <- paste0(x_jonsom, ",", '"', gsub('"','',data_col_dup[ix]),';[',x_Ctrl_gbifID_dup,']"')
            }
            
            
            # combina dasdos de uma amostra, adiciona informações novas à culunas vazias
            # referencia a infoamação com Ctrl_gbifID em parseGBIF_merged_fields
            if(x_data_col=="" &
               fields_to_all[ic] %in% fields_to_merge)
            {
              # print('merge')
              
              occ_res_full[index==TRUE,
                           fields_to_all[ic]] <- data_col_dup[ix]
              
              
              # x_Ctrl_gbifID <- occ_res_full[index==TRUE, 'Ctrl_gbifID']
              
              
              
              if(parseGBIF_merged_fields=="")
              {
                # parseGBIF_merged_fields <- paste0('"', fields_to_all[ic],'":[','"', ix+1,'"]')
                # parseGBIF_merged_fields <- paste0('"', fields_to_all[ic],'":[','"', ix,'"]') 
                parseGBIF_merged_fields <- paste0('"', fields_to_all[ic],'":[','"', x_Ctrl_gbifID_dup,'"]')
                
              }else
              {
                # parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', fields_to_all[ic],'":[','"', ix+1,'"]')
                # parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', fields_to_all[ic],'":[','"', ix,'"]') 
                parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', fields_to_all[ic],'":[','"', x_Ctrl_gbifID_dup,'"]')
              }
            }
            
          }
          
          if (x_jonsom != "")
          {
            x_jonsom <- paste0(x_jonsom, ']')
            
            if (x_jonsom_full=="")
            {
              x_jonsom_full <- x_jonsom
            }else
            {
              x_jonsom_full <- paste0(x_jonsom_full,',',x_jonsom)
            }
          }
          
          
        }
        
        if(x_jonsom_full != "")
        {
          occ_res_full$parseGBIF_duplicates_map[index==TRUE] <- paste0('{',x_jonsom_full,'}')
        }
        
        if (parseGBIF_merged_fields != "")
        {
          occ_res_full[index==TRUE,
                       c('parseGBIF_merged_fields',
                         'parseGBIF_merged')] <- c(paste0('{',parseGBIF_merged_fields,'}'),
                                                   TRUE)
        }
        
        if (freq_data_col_full != "")
        {
          occ_res_full[index==TRUE,'parseGBIF_freq_duplicate_or_missing_data'] <- paste0('{',freq_data_col_full,'}') 
        }
        
        
        
      }
    }
  }
  
  if(merge_occ_out==TRUE)
  {
    # in dup out_to_recover
    {
      occ_out_to_recover_merge <- occ_res_full %>%
        dplyr::filter(parseGBIF_digital_voucher == TRUE,
                      (parseGBIF_unidentified_sample == TRUE |
                         parseGBIF_useful_for_spatial_analysis == FALSE))
      
      occ_res_full <- occ_res_full %>%
        dplyr::filter(parseGBIF_digital_voucher == TRUE,
                      parseGBIF_unidentified_sample == FALSE, # parseGBIF_sample_taxon_name_status %in% c('identified','divergent identifications'),
                      parseGBIF_useful_for_spatial_analysis == TRUE) 
      
      
    }
    
    
    occ_all <- rbind(occ_res_full %>%
                       dplyr::mutate(parseGBIF_dataset_result='in'),
                     occ_out_to_recover_merge %>%
                       dplyr::mutate(parseGBIF_dataset_result='out_to_recover'),
                     occ_dup %>%
                       dplyr::mutate(parseGBIF_dataset_result='dup'))
    
  }else
  {
    
    occ_all <- rbind(occ_res_full %>%
                       dplyr::mutate(parseGBIF_dataset_result='in'),
                     occ_out_to_recover %>%
                       dplyr::mutate(parseGBIF_dataset_result='out_to_recover'),
                     occ_dup %>%
                       dplyr::mutate(parseGBIF_dataset_result='dup'))
    
    
  }
  {
    occ_tmp <- occ_results$occ_all
    merge_occ_out = T
    
    {
      fields_to_merge <- c('Ctrl_fieldNotes',
                           'Ctrl_year',
                           'Ctrl_stateProvince',
                           'Ctrl_municipality',
                           'Ctrl_locality',
                           'Ctrl_countryCode',
                           'Ctrl_eventDate',
                           'Ctrl_habitat',
                           'Ctrl_level0Name',
                           'Ctrl_level1Name',
                           'Ctrl_level2Name',
                           'Ctrl_level3Name')
      
      fields_to_compare = c('Ctrl_gbifID',
                            'Ctrl_scientificName',
                            'Ctrl_recordedBy',
                            'Ctrl_recordNumber',
                            'Ctrl_identifiedBy',
                            'Ctrl_dateIdentified',
                            'Ctrl_institutionCode',
                            'Ctrl_collectionCode',
                            'Ctrl_datasetName',
                            'Ctrl_datasetName',
                            'Ctrl_language',
                            "wcvp_plant_name_id",
                            "wcvp_taxon_rank",                                  
                            "wcvp_taxon_status",
                            "wcvp_family",
                            "wcvp_taxon_name",
                            "wcvp_taxon_authors",
                            "wcvp_reviewed",
                            "wcvp_searchNotes")
      
      fields_to_all <- c(fields_to_compare,fields_to_merge)
      
      parseGBIF_summary <<- data.frame(question='',
                                       value=0,
                                       unit='')[-1,]
      
      add_summary <- function(question='',
                              value=0,
                              unit='',
                              show=FALSE)
      { 
        if(question[1] !='')
        {
          parseGBIF_summary <<- parseGBIF_summary %>%
            dplyr::add_row(question=question,
                           value=value,
                           unit=unit)
        }
        if(show==TRUE | question[1] ==''){print(parseGBIF_summary)}
      }
      
      freq_merged_fields <- function(fields=NA,
                                     occ_tmp=NA)
      {
        
        freq_fields <- data.frame(id='',val=0)[-1,]
        freq_fields <- freq_fields %>%
          dplyr::add_row(id=fields,
                         val=rep(0,NROW(fields)))
        
        for(i in 1:NROW(occ_tmp))
        {
          if(occ_tmp$parseGBIF_merged[i] == FALSE)
          {
            next
          }
          
          x <- jsonlite::fromJSON(occ_tmp$parseGBIF_merged_fields[i])
          
          ic=1
          
          for(ic in 1:NROW(fields_to_merge))
          {
            if(fields_to_merge[ic] %in% names(x))
            {
              freq_fields[ic,2]  <- freq_fields[ic,2]+1
            }
          }
        }  
        
        freq_fields <- freq_fields %>%
          dplyr::arrange(desc(val))
        return(freq_fields)
      }
      
    }
    
    question <- 'total number of records'
    value <- NROW(occ_tmp)
    unit <- 'records'
    add_summary(question, value, unit)
    
    question <- 'total number of samples'
    value <- NROW(occ_tmp %>% dplyr::filter(parseGBIF_digital_voucher==TRUE))
    add_summary(question, value, 'samples')
    
    question <- 'total number of duplicates'
    value <- sum(occ_tmp$parseGBIF_dataset_result=='dup')
    add_summary(question, value, unit)
    
    
    question <- 'total number of non-groupable records'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates &
                   occ_tmp$parseGBIF_digital_voucher == TRUE)
    add_summary(question, value, unit)
    
    question <- 'total number of non-groupable samples'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates == TRUE &
                   occ_tmp$parseGBIF_digital_voucher == TRUE)
    add_summary(question, value, 'samples')
    
    question <- 'in: number of non-groupable samples'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates == TRUE&
                   occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='in')
    add_summary(question, value, 'samples')
    
    
    question <- 'out_to_recover: number of non-groupable samples'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates == TRUE&
                   occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='out_to_recover')
    add_summary(question, value, 'samples')
    
    
    question <- 'total number of groupable records'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates==FALSE)
    add_summary(question, value, unit)
    
    
    question <- 'total number of groupable samples'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates==FALSE &
                   occ_tmp$parseGBIF_digital_voucher == TRUE)
    add_summary(question, value, 'samples')
    
    question <- 'in: number of groupable samples'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates == FALSE&
                   occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='in')
    add_summary(question, value, 'samples')
    
    question <- 'out_to_recover: number of groupable samples'
    value <- sum(occ_tmp$parseGBIF_non_groupable_duplicates == FALSE&
                   occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='out_to_recover')
    add_summary(question, value, 'samples')
    
    
    question <- 'total number of samples with duplicates'
    value <- NROW(occ_tmp %>%
                    dplyr::filter(parseGBIF_duplicates==TRUE &
                                    parseGBIF_dataset_result!='dup'))
    add_summary(question, value, 'samples')
    
    
    question <- 'in: number of samples with duplicates'
    value <- NROW(occ_tmp %>%
                    dplyr::filter(parseGBIF_duplicates==TRUE &
                                    parseGBIF_dataset_result=='in'))
    add_summary(question, value, 'samples')
    
    
    question <- 'out_to_recover: number of samples with duplicates'
    value <- NROW(occ_tmp %>%
                    dplyr::filter(parseGBIF_duplicates==TRUE &
                                    parseGBIF_dataset_result=='out_to_recover'))
    sum(occ_tmp$parseGBIF_duplicates)
    add_summary(question, value, 'samples')
    
    if(merge_occ_out==TRUE)
    {
      question <- 'total number of number of unidentified samples'
      value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'unidentified' &
                     occ_tmp$parseGBIF_digital_voucher == TRUE)
      add_summary(question, value, 'samples')
      
      question <- 'total number of number of identified samples'
      value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'identified' &
                     occ_tmp$parseGBIF_digital_voucher == TRUE)
      add_summary(question, value, 'samples')
      
      question <- 'total number of number of samples with divergent identifications'
      value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'divergent identifications' &
                     occ_tmp$parseGBIF_digital_voucher == TRUE)
      add_summary(question, value, 'samples')
    }  
    
    
    question <- 'in: number of unidentified samples'
    value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'unidentified' &
                   occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='in')
    add_summary(question, value, 'samples')
    
    question <- 'in: number of identified samples'
    value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'identified' &
                   occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='in')
    add_summary(question, value, 'samples')
    
    question <- 'in: number of samples with divergent identifications'
    value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'divergent identifications' &
                   occ_tmp$parseGBIF_digital_voucher == TRUE &
                   occ_tmp$parseGBIF_dataset_result=='in')
    add_summary(question, value, 'samples')
    
    if(merge_occ_out==TRUE)
    {
      question <- 'out_to_recover: number of unidentified samples'
      value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'unidentified' &
                     occ_tmp$parseGBIF_digital_voucher == TRUE &
                     occ_tmp$parseGBIF_dataset_result=='out_to_recover')
      add_summary(question, value, 'samples')
      
      question <- 'out_to_recover: number of identified samples'
      value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'identified' &
                     occ_tmp$parseGBIF_digital_voucher == TRUE &
                     occ_tmp$parseGBIF_dataset_result=='out_to_recover')
      add_summary(question, value, 'samples')
      
      question <- 'out_to_recover: number of samples with divergent identifications'
      value <- sum(occ_tmp$parseGBIF_sample_taxon_name_status == 'divergent identifications' &
                     occ_tmp$parseGBIF_digital_voucher == TRUE &
                     occ_tmp$parseGBIF_dataset_result=='out_to_recover')
      add_summary(question, value, 'samples')
      
    }
    
    question <- 'number samples: out_to_recover'
    value <- sum(occ_tmp$parseGBIF_dataset_result=='out_to_recover')
    add_summary(question, value, 'samples')
    
    # question <- 'number samples: out_to_recover: unidentified sample'
    # value <- sum(occ_tmp$parseGBIF_dataset_result=='out_to_recover' &
    #              (occ_tmp$parseGBIF_unidentified_sample == TRUE & occ_tmp$parseGBIF_useful_for_spatial_analysis == TRUE) )
    # add_summary(question, value, 'samples')
    # 
    # question <- 'number samples: out_to_recover: not useful for spatial analysis'
    # value <- sum(occ_tmp$parseGBIF_dataset_result=='out_to_recover' &
    #                (occ_tmp$parseGBIF_unidentified_sample == FALSE & occ_tmp$parseGBIF_useful_for_spatial_analysis == FALSE) )
    # add_summary(question, value, 'samples')
    # 
    # question <- 'number samples: out_to_recover: unidentified sample and not useful for spatial analysis'
    # value <- sum(occ_tmp$parseGBIF_dataset_result=='out_to_recover' &
    #                (occ_tmp$parseGBIF_useful_for_spatial_analysis == FALSE &
    #                   occ_tmp$parseGBIF_unidentified_sample == TRUE) )
    # add_summary(question, value, 'samples')
    
    if(merge_occ_out==TRUE)
    {
      question <- 'total samples with any field merged'
      value <- NROW(occ_tmp %>%
                      dplyr::filter(parseGBIF_merged==TRUE))
      add_summary(question, value, 'samples')
    }
    
    question <- 'number samples with any field merged: in'
    value <- NROW(occ_tmp %>%
                    dplyr::filter(parseGBIF_merged==TRUE &
                                    parseGBIF_dataset_result=='in'))
    add_summary(question, value, 'samples')
    
    if(merge_occ_out==TRUE)
    {
      x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp)
      x_freq_merged_fields$id <- paste0('total merge events in the field: ',x_freq_merged_fields$id)
      add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, 'merge events')
    }
    
    x_freq_merged_fields <- freq_merged_fields(fields_to_merge, occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='in'))
    x_freq_merged_fields$id <- paste0('merge events in the field - in : ',x_freq_merged_fields$id)
    add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, 'merge events')
    
    if(merge_occ_out==TRUE)
    {
      question <- 'number samples with any field merged: out_to_recover'
      value <- NROW(occ_tmp %>%
                      dplyr::filter(parseGBIF_merged==TRUE &
                                      parseGBIF_dataset_result=='out_to_recover'))
      add_summary(question, value, 'samples')
      
      x_freq_merged_fields <- freq_merged_fields(fields = fields_to_merge, occ_tmp = occ_tmp %>% dplyr::filter(parseGBIF_dataset_result=='out_to_recover'))
      x_freq_merged_fields$id <- paste0('merge events in the field - out_to_recover : ',x_freq_merged_fields$id)
      add_summary(x_freq_merged_fields$id, x_freq_merged_fields$val, 'merge events')
    }
    
  }
  
  add_summary()
  
  if(merge_occ_out==TRUE)
  {
    
    
    return(list(occ_all = occ_all,
                occ_in_merge = occ_res_full,
                occ_in_raw = occ_in,
                occ_dup = occ_dup,
                occ_out_to_recover_raw = occ_out_to_recover,
                occ_out_to_recover_merge = occ_out_to_recover_merge,
                summary = parseGBIF_summary))
  }else
  {
    
    return(list(occ_all = occ_all,
                occ_in_merge = occ_res_full,
                occ_in_raw = occ_in,
                occ_dup = occ_dup,
                occ_out_to_recover_raw = occ_out_to_recover,
                summary = parseGBIF_summary))
  }
  
  
}
