#' @title Export of results
#' @name export_data
#'
#' @description Separate records into three data frames
#' Export of results:
#' * Useful data for spatial and taxonomic analysis
#' * Data in need of revision of spatial information or without identification
#' * Duplicates of the previous two datasets
#'
#' @param occ_digital_voucher_file CSV fila result of function select_digital_voucher()$occ_digital_voucher
#' @param occ_digital_voucher data frame result of function select_digital_voucher()$occ_digital_voucher
#'
#' @details Each data frame should be used as needed
#'
#' @return list with three data frames:
#' occ_in, Useful data for spatial and taxonomic analysis,
#' occ_out_to_recover, data in need of spatial data revision or without identification and
#' occ_dup, duplicates.
#'
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
#' }
#' @export
#' occ_digital_voucher_file <- 'occ_digital_voucher.csv'
export_data <- function(occ_digital_voucher_file = '',
                        occ_digital_voucher = NA,
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
    
  }
  
  # in dup out_to_recover
  {
    # NROW(occ_tmp)
    
    occ_in <- occ_tmp %>%
      dplyr::filter(parseGBIF_digital_voucher == TRUE,
                    parseGBIF_unidentified_sample == FALSE, # parseGBIF_sample_taxon_name_status %in% c('identified','divergent identifications'),
                    parseGBIF_useful_for_spatial_analysis == TRUE) 
    
    # NROW(occ_in)
    # occ_dup <- dplyr::anti_join(occ_tmp %>%
    #                               dplyr::filter(parseGBIF_digital_voucher == FALSE), occ_in, "Ctrl_gbifID")
    
    occ_dup <- occ_tmp %>%
      dplyr::filter(parseGBIF_digital_voucher == FALSE)
    
    # sum(occ_dup$Ctrl_key_family_recordedBy_recordNumber %in% 'ACHATOCARPACEAE_ZARDINI_5592')

    occ_out_to_recover <- occ_tmp %>%
      dplyr::filter(parseGBIF_digital_voucher == TRUE,
                    (parseGBIF_unidentified_sample == TRUE |
                       parseGBIF_useful_for_spatial_analysis == FALSE))
    
    # remove(occ_tmp)
    
  }

  print('Merging...')
  
  # merge and compare
  {

    # fields_to_merge
    # fields_to_compare
    
    occ_res_full <- occ_in %>%
      dplyr::mutate(parseGBIF_duplicates_map = rep('',NROW(occ_in)),
                    parseGBIF_merged_fields = rep('',NROW(occ_in)),
                    parseGBIF_merged = rep(FALSE,NROW(occ_in)))
    
    key <- occ_in$Ctrl_key_family_recordedBy_recordNumber %>%
      unique()
    
    tot <- NROW(key)
    fields_to_all <- c(fields_to_compare,fields_to_merge)
    
    i = 52
    i=1
    for(i in 1:tot)
    {
  
      index <- occ_in$Ctrl_key_family_recordedBy_recordNumber %in% key[i]
      
      if(occ_in$parseGBIF_duplicates[index==TRUE][1]==TRUE)
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
          
          data_col <- occ_in[index==TRUE,
                             fields_to_all[ic]] 
          
          if(is.na(data_col))
          {
            data_col <- ""
          }
          
          x_data_col <- toupper(data_col)
          
          data_col_dup <- occ_dup[index_dup==TRUE,
                                  fields_to_all[ic]] 
          
          freq_data_col <- table(freq_tmp <- data.frame(value= c(occ_in[index==TRUE,fields_to_all[ic]], 
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

  return(list(occ_merge = occ_res_full,
              occ_raw = occ_in,
              occ_dup = occ_dup,
              occ_out_to_recover = occ_out_to_recover))
  
}
