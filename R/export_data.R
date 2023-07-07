#' @title Export of results
#' @name export_data
#'
#' @description Separate records into three data frames
#' Export of results:
#' * Useful data for spatial and taxonomic analysis
#' * Data in need of revision of spatial information or without identification
#' * Duplicates of the previous two datasets
#'
#' @param occ_digital_voucher_file CSV fila result of function select_digital_voucher_and_sample_identification()$occ_digital_voucher_and_sample_identification
#' @param occ_digital_voucher data frame result of function select_digital_voucher_and_sample_identification()$occ_digital_voucher_and_sample_identification
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

# library(ParsGBIF)
# library(dplyr)
# occ_digital_voucher_file <- 'occ_digital_voucher.csv'
export_data <- function(occ_digital_voucher_file = '',
                        occ_digital_voucher = NA)
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
  
  occ_tmp <- occ_tmp %>%
    dplyr::select(Ctrl_gbifID,
                  Ctrl_bibliographicCitation,
                  Ctrl_language,
                  Ctrl_institutionCode,
                  Ctrl_collectionCode,
                  Ctrl_datasetName,
                  Ctrl_basisOfRecord,
                  Ctrl_catalogNumber,	
                  Ctrl_recordNumber,
                  Ctrl_recordedBy,
                  Ctrl_occurrenceStatus,
                  Ctrl_eventDate,	
                  Ctrl_year,	
                  Ctrl_month,	
                  Ctrl_day,
                  Ctrl_habitat,
                  Ctrl_fieldNotes,
                  Ctrl_eventRemarks,
                  Ctrl_countryCode,	
                  Ctrl_stateProvince,	
                  Ctrl_municipality,
                  Ctrl_county,
                  Ctrl_locality,
                  
                  Ctrl_level0Name,
                  Ctrl_level1Name,
                  Ctrl_level2Name,
                  Ctrl_level3Name,
                  
                  
                  Ctrl_identifiedBy,
                  Ctrl_dateIdentified,
                  Ctrl_scientificName,
                  Ctrl_decimalLatitude,
                  Ctrl_decimalLongitude,
                  
                  
                  Ctrl_nameRecordedBy_Standard,	
                  Ctrl_recordNumber_Standard,	
                  Ctrl_key_family_recordedBy_recordNumber,
                  Ctrl_geospatial_quality,
                  Ctrl_verbatim_quality,
                  Ctrl_moreInformativeRecord,
                  Ctrl_coordinates_validated_by_gbif_issue,	
                  
                  
                  wcvp_taxon_name,
                  wcvp_taxon_status,
                  
                  parseGBIF_digital_voucher,	
                  parseGBIF_duplicates,
                  parseGBIF_non_groupable_duplicates,	
                  parseGBIF_duplicates_grouping_status)
  
  occ_tmp <- occ_tmp %>%
    dplyr::mutate(
      parseGBIF_unidentified_sample = TRUE,
      parseGBIF_sample_taxon_name = '',
      parseGBIF_sample_taxon_name_status = '',
      parseGBIF_number_taxon_names = 0,
      parseGBIF_useful_for_spatial_analysis = FALSE,
      parseGBIF_decimalLatitude = NA,
      parseGBIF_decimalLongitude = NA)
  
  
  recordedBy_unique <- occ_tmp$Ctrl_key_family_recordedBy_recordNumber %>% unique()
  tot <- NROW(recordedBy_unique)
  s<-0
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
          # parseGBIF_digital_voucher = TRUE,
          # parseGBIF_non_groupable_duplicates = TRUE,
          # parseGBIF_duplicates = FALSE,
          parseGBIF_sample_taxon_name = sp_name,
          parseGBIF_unidentified_sample = ifelse(sp_name %in% '', TRUE,FALSE),
          
          # parseGBIF_duplicates_grouping_status = ifelse(FAMILY__==TRUE,
          #                                               'not groupable: no recordedBy and no recordNumber',
          #                                               ifelse(FAMILY__recordNumber==TRUE,
          #                                                      'not groupable: no recordNumber ',
          #                                                      ifelse(FAMILY_recordedBy_==TRUE,
          #                                                             'not groupable: no recordedBy', 'not groupable'))),
          
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
      
      
      # ifelse(is.na(occ_tmp[index_occ==TRUE, ]$wcvp_taxon_status == 'Accepted'), FALSE,TRUE)
      
      if(!any(is.na(occ_tmp[index_occ==TRUE, ]$wcvp_taxon_name) == FALSE)) # any ???
      {
        
        # nomes
        sp_name <- rep('',num_records)
        
        occ_tmp[index_occ==TRUE, ] <- occ_tmp[index_occ==TRUE, ] %>%
          dplyr::mutate(
            # parseGBIF_duplicates_grouping_status = 'groupable',
            # parseGBIF_duplicates = num_records > 1,
            parseGBIF_number_taxon_names = 0,
            parseGBIF_sample_taxon_name = sp_name,
            parseGBIF_sample_taxon_name_status = 'unidentified')
        
      }else
      {
        
        taxon_name_sample <- table(occ_tmp[index_occ==TRUE, ]$wcvp_taxon_name,
                                   # occ[index_occ==TRUE, ]$wcvp_searchNotes,
                                   occ_tmp[index_occ==TRUE, ]$wcvp_taxon_status,
                                   # occ[index_occ==TRUE, ]$Ctrl_taxonRank,
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
              # parseGBIF_duplicates_grouping_status = 'groupable',
              # parseGBIF_duplicates = num_records > 1,
              parseGBIF_number_taxon_names = num_taxon_name,
              parseGBIF_sample_taxon_name = sp_name,
              parseGBIF_sample_taxon_name_status = 'identified',
              parseGBIF_unidentified_sample = FALSE
            )
          
          # print('3 - Identified sample 100 %')
          
          # next
          
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
                  # parseGBIF_duplicates_grouping_status = 'groupable',
                  # parseGBIF_duplicates = num_records > 1,
                  parseGBIF_number_taxon_names = num_taxon_name,
                  parseGBIF_sample_taxon_name = sp_name,
                  parseGBIF_sample_taxon_name_status = 'divergent identifications',
                  parseGBIF_unidentified_sample = FALSE)
              
              
              # print(paste0('4 - Identified sample ', 100/num_taxon_name,' %'))
              
              break
            }
            
            
          }
          
          # next
          
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
    
    occ_dup <- occ_tmp %>%
      dplyr::filter(parseGBIF_digital_voucher == FALSE)
    
    # NROW(occ_dup)

    # NROW(occ_in)+NROW(occ_dup)+NROW(occ_out_to_recover)
    # NROW(occ_tmp)
  }
  
  # merge
  {
    col_from_merge <- c('Ctrl_gbifID',
                        
                        'Ctrl_fieldNotes',       # check and merge - character   
                        'Ctrl_year',             # check and merge - character 
                        'Ctrl_stateProvince',    # check and merge - character 
                        'Ctrl_municipality',     # check and merge - character 
                        'Ctrl_locality',         # check and merge - character 
                        
                        
                        'Ctrl_countryCode',      # merge - character 
                        'Ctrl_eventDate',        # merge - data
                        'Ctrl_habitat',          # merge - character 
                        'Ctrl_level0Name',	      # merge - character 
                        'Ctrl_level1Name',	      # merge - character
                        'Ctrl_level2Name',	      # merge - character
                        'Ctrl_level3Name'       # merge - character
    )
    
    
    
    occ_res_full <- occ_in %>%
      dplyr::mutate(parseGBIF_duplicates_map = rep('',NROW(occ_in)),
                    parseGBIF_merged_fields = rep('',NROW(occ_in)),
                    parseGBIF_merged = rep(FALSE,NROW(occ_in)))
    
    key <- occ_in$Ctrl_key_family_recordedBy_recordNumber %>%
      unique()
    
    tot <- NROW(key)
    
    for(i in 1:tot)
    {
      
      
      index <- occ_in$Ctrl_key_family_recordedBy_recordNumber %in% key[i]
      
      
      if(occ_in$parseGBIF_duplicates[index==TRUE][1]==TRUE)
      {
        print(paste0(i, ' - ', tot, ' - ', key[i]))
        
        index_dup <- occ_dup$Ctrl_key_family_recordedBy_recordNumber %in% key[i]
        # sum(index_dup)
        
        ic <- 1
        
        x_jonsom_full <- ""
        parseGBIF_merged_fields <- ""
        
        for ( ic in 1:length(col_from_merge))
        {
          
          data_col <- occ_in[index==TRUE,
                             col_from_merge[ic]] 
          
          if(is.na(data_col))
          {
            data_col <- ""
          }
          
          x_data_col <- toupper(data_col)
          
          data_col_dup <- occ_dup[index_dup==TRUE,
                                  col_from_merge[ic]] 
          
          ix <- 1
          # x <- ""
          x_jonsom <- ""
          
          for(ix in 1:NROW(data_col_dup))
          {
            
            if(is.na(data_col_dup[ix]))
            {
              next
            }
            
            x_data_col_dup <- toupper(data_col_dup[ix])
            
            if(x_data_col == x_data_col_dup)
            {
              next
            }
            
            # # nao armazear dados repetidos entre duplicatas 
            # if(ix > 1 & data_col_dup[ix] %in% data_col_dup[1:ix] )
            # {
            #   next
            # }
            
            
            # armazena informações novas para o campo
            # cabeçalho
            
            if(x_jonsom=="") #aqui
            {
              # # if(data_col %>% as.character() !="")
              # if(col_from_merge[ic] == "Ctrl_gbifID" )
              # {
              #   x_jonsom <-  paste0('"',col_from_merge[ic],'"',":[",'"', gsub('"','',data_col),'"')
              # }else
              # {
              x_jonsom <-  paste0('"',col_from_merge[ic],'"',":[")
              # }
            }
            
            # dados
            if(substr(x_jonsom,str_count(x_jonsom),str_count(x_jonsom)) == '[')
            {
              x_jonsom <- paste0(x_jonsom, '"', gsub('"','',data_col_dup[ix]),'"')
            }else
            {
              x_jonsom <- paste0(x_jonsom, ",", '"', gsub('"','',data_col_dup[ix]),'"')
            }
            
            
            # combina dasdos de uma amostra, adiciona informações novas à culunas vazias
            # referencia a infoamação com Ctrl_gbifID em parseGBIF_merged_fields
            if (x_data_col=="")
            {
              occ_res_full[index==TRUE,
                           col_from_merge[ic]] <- data_col_dup[ix]
              
              if(parseGBIF_merged_fields=="")
              {
                # parseGBIF_merged_fields <- paste0('"', col_from_merge[ic],'":[','"', ix+1,'"]')
                parseGBIF_merged_fields <- paste0('"', col_from_merge[ic],'":[','"', ix,'"]')
                
              }else
              {
                # parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', col_from_merge[ic],'":[','"', ix+1,'"]')
                parseGBIF_merged_fields <- paste0(parseGBIF_merged_fields, ",", '"', col_from_merge[ic],'":[','"', ix,'"]')
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
      }
    }
  }
  
  
  occ_out_to_recover <- occ_tmp %>%
    dplyr::filter(parseGBIF_digital_voucher == TRUE,
                  (parseGBIF_unidentified_sample == TRUE |
                     parseGBIF_useful_for_spatial_analysis == FALSE))
  
  # NROW(occ_out_to_recover)
  
  return(list(occ_merge = occ_res_full,
              occ_raw = occ_in,
              occ_dup = occ_dup,
              occ_out_to_recover = occ_out_to_recover))
  
}




# View(occ_res) 
# View(occ_res_full) 
# 
# 
# jsonlite::fromJSON(occ_res_full$merged_fields[2154])
# 
# jsonlite::fromJSON(occ_res_full$merge_map[2154])
# 
# occ_res_full$Ctrl_language[2154]
# occ_res$Ctrl_language[2154]
# 
# occ_res_full$Ctrl_municipality[2154]
# occ_res$Ctrl_municipality[2154]
# occ_tmp$Ctrl_municipality[2154]
# 
# 
# jsonlite::fromJSON(occ_res_full$merge_map[2154])
# 
# 
# 
# jsonlite::fromJSON(occ_res_full$merge_map[192])
# occ_res_full$merged_fields[192]
# 
# 
# 
# jsonlite::fromJSON(occ_res_full$merge_map[7])
# occ_res_full$merged_fields[7]
# 
# jsonlite::fromJSON(occ_res_full$merged_fields[903])
# occ_res_full$Ctrl_eventRemarks[903]
# occ_res$Ctrl_eventRemarks[903]
# occ_tmp$Ctrl_eventRemarks[903]
# 
# jsonlite::fromJSON(occ_res_full$merge_map[903])
# 
# 
# 
# 
# 
# jsonlite::fromJSON(occ_res_full$merge_map[939])
# jsonlite::fromJSON(occ_res_full$merged_fields[939])
# 
# 
# jsonlite::fromJSON(occ_res_full$merge_map[940])
# jsonlite::fromJSON(occ_res_full$merged_fields[940])
# 
# 
# 
# jsonlite::fromJSON(occ_res_full$merge_map[903])$Ctrl_eventRemarks
# 
# 
# View(occ_res_full[,c(9,10,73)]) 
# 
# 
# View(occ_res) 
# View(occ_res_full) 
# 
# View(occ_res[,c(9,10,54)]) 
