#' @title Selecting the master digital voucher
#' @name select_digital_voucher
#'
#' @description To group duplicates and choose the digital voucher:
#' Unique collection events can result in many ‘duplicate’ GBIF records. We designate one of these ‘duplicate’ records
#' as the master digital voucher, to which data from other duplicate vouchers can be merged (see export_data):
#'
#' __Where the collection event key for grouping duplicates is complete__, then duplicates can be grouped / parsed.
#' To do so, we evaluate record completeness. Record completeness is calculated based on data-quality scores
#' for the information in the following  fields: recordedBy, recordNumber, year, institutionCode, catalogNumber, locality, municipality,
#' countryCode, stateProvince and fieldNotes. The spatial coordinates associated with each duplicate are ranked using a score for the
#' quality of the geospatial information. This score is calculated using the issues listed in the GBIF table, EnumOccurrenceIssue.
#' A score is calculated based on these issues (see above). The duplicate with the highest total score is assigned as the master voucher
#' for the unique collection event. Missing information contained in duplicate records of the unique collection event can then be merged
#' into the master digital voucher (see export_data).
#'
#' __Where the collection event key is incomplete__, unique collection event duplicates cannot be parsed. In this case,
#' each record is considered as a unique collection event, without duplicates. However, to know the integrity
#' of the information, record completeness and quality of the geospatial information, are evaluated as described above.
#'
#' __How is the quality score calculated?__
#' parseGBIF_digital_voucher = The duplicate with the highest total score, sum of record completeness + quality of geospatial information.
#'
#' __How is record completeness calculated?__
#' The quality of the duplicate records associated with each collection event key is measured as the
#' completeness of a record, using the sum of a number of flags (see below) equal to TRUE.
#'
#' __Flags used to calculate record completeness__
#'
#' * Is there information about the collector?
#' * Is there information about the collection number?
#' * Is there information about the year of collection?
#' * Is there information about the institution code?
#' * Is there information about the catalog number?
#' * Is there information about the locality?
#' * Is there information about the municipality of collection?
#' * Is there information about the state/province of collection?
#' * Is there information about the field notes?
#'
#' __The quality of geospatial information is based on geographic issues raised by GBIF.__
#' GIBF issues relating to geospatial data were classified into three classes based on the data quality
#' scores that we assigned to each of the following GBIF issues recorded in the EnumOccurrenceIssue.
#'
#' * Issue does not affect coordinating accuracy, with selection_score equal to -1
#' * Issue has potential to affect coordinate accuracy, with selection_score equal to -3
#' * Records with a selection_score equal to -9 are excluded.
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param occ_gbif_issue = result of function extract_gbif_issue()$occ_gbif_issue
#' @param occ_wcvp_check_name = result of function batch_checkName_wcvp()$occ_wcvp_check_name
#' @param occ_collectorsDictionary = result of function update_collectorsDictionary()$occ_collectorsDictionary
#' @param enumOccurrenceIssue An enumeration of validation rules for single occurrence records by GBIF file, if NA, will be used, data(EnumOccurrenceIssue)
#' @param silence if TRUE does not display progress messages
#'
#' @details
#' * parseGBIF_duplicates_grouping_status - "groupable", "not groupable: no recordedBy and no recordNumber",
#' "not groupable: no recordNumber" or "not groupable: no recordedBy"
#' * parseGBIF_num_duplicates number of duplicates records
#' * parseGBIF_duplicates TRUE/FALSE
#' * parseGBIF_non_groupable_duplicates TRUE/FALSE
#'
#'
#' @return list with two data frames: occ_digital voucher_and:
#' occ_digital_voucher,  with all data processing fields and
#' occ_results, only result fields.
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
#'
#' @examples
#' \donttest{
#' help(select_digital_voucher)
#'
#' head(occ)
#' head(res_gbif_issue$occ_gbif_issue)
#' head(res_checkName_wcvp$occ_wcvp_check_name)
#' head(res_collectorsDictionary$occ_collectorsDictionary)
#' res_digital_voucher_and_sample_identification <- select_digital_voucher(occ = occ,
#'                                                                         occ_gbif_issue = res_gbif_issue$occ_gbif_issue,
#'                                                                         occ_wcvp_check_name = res_checkName_wcvp$occ_wcvp_check_name,
#'                                                                         occ_collectorsDictionary = res_collectorsDictionary$occ_collectorsDictionary,
#'                                                                         enumOccurrenceIssue = EnumOccurrenceIssue)
#'
#' names(res_digital_voucher_and_sample_identification)
#'
#' head(res_digital_voucher_and_sample_identification$occ_digital_voucher)
#' colnames(res_digital_voucher_and_sample_identification$occ_digital_voucher)
#'
#' }
#' @export
select_digital_voucher <-  function(occ = NA,
                                    occ_gbif_issue = NA,
                                    occ_wcvp_check_name = NA,
                                    occ_collectorsDictionary = NA,
                                    enumOccurrenceIssue = NA,
                                    silence = TRUE)
{
  {
  require(dplyr)
  require(readr)
    if (is.na(enumOccurrenceIssue))
    {
      data(EnumOccurrenceIssue)
    }else
    {
      EnumOccurrenceIssue <- enumOccurrenceIssue
    }

    occ_in <- occ

    occ <- cbind(occ_gbif_issue, occ_in, occ_wcvp_check_name, occ_collectorsDictionary)


    # occ$wcvp_taxon_rank %>% unique()
    occ$wcvp_taxon_rank <- ifelse(is.na(occ$wcvp_taxon_rank),'',occ$wcvp_taxon_rank)

    # occ$wcvp_taxon_status %>% unique()
    occ$wcvp_taxon_status <- ifelse(is.na(occ$wcvp_taxon_status),'',occ$wcvp_taxon_status)

    occ_issue <- colnames(occ)

    # Ctrl_geospatial_quality
    index_tmp1 <- EnumOccurrenceIssue$score == 1 & EnumOccurrenceIssue$type == 'geospatial' %>%
      ifelse(is.na(.), FALSE,.)
    index_tmp2 <- EnumOccurrenceIssue$score == 2 & EnumOccurrenceIssue$type == 'geospatial'%>%
      ifelse(is.na(.), FALSE,.)
    index_tmp3 <- EnumOccurrenceIssue$score == 3 & EnumOccurrenceIssue$type == 'geospatial'%>%
      ifelse(is.na(.), FALSE,.)
  }

  # Ctrl_verbatim_quality
  {

    occ <- occ %>%
      dplyr::mutate(temAnoColeta =  ifelse( is.na(Ctrl_year) | Ctrl_year == ""  | Ctrl_year == 0 | Ctrl_year <= 10,
                                            FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),


                    temCodigoInstituicao = ifelse( is.na(Ctrl_institutionCode) | Ctrl_institutionCode=="",
                                                   FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),


                    temNumeroCatalogo = ifelse( is.na(Ctrl_catalogNumber) | Ctrl_catalogNumber=="",
                                                FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),


                    temColetor = ifelse( is.na(Ctrl_recordedBy) | Ctrl_recordedBy=="",
                                         FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),


                    temNumeroColeta = ifelse( is.na(Ctrl_recordNumber) | Ctrl_recordNumber=="",
                                              FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),

                    # COUNTRY_MISMATCH

                    temPais = ifelse( COUNTRY_INVALID==TRUE,
                                      FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),


                    temUF = ifelse( is.na(Ctrl_stateProvince) | Ctrl_stateProvince=="",
                                    FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),


                    temMunicipio = ifelse( is.na(Ctrl_municipality) | Ctrl_municipality=="",
                                           FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),


                    temLocalidade = ifelse( is.na(Ctrl_locality) | Ctrl_locality=="",
                                            FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.),

                    temNotas = ifelse( is.na(Ctrl_fieldNotes) | Ctrl_fieldNotes=="",
                                               FALSE, TRUE) %>%
                      ifelse(is.na(.), FALSE,.)

                    # temIdentificador = ifelse( is.na(Ctrl_identifiedBy) | Ctrl_identifiedBy=="",
                    #                            FALSE, TRUE) %>%
                    #   ifelse(is.na(.), FALSE,.),

                    # temDataIdentificacao = ifelse( is.na(Ctrl_dateIdentified) | Ctrl_dateIdentified=="",
                    #                                FALSE, TRUE) %>%
                    #   ifelse(is.na(.), FALSE,.),

                    # temCitacaoBibliografica = ifelse( is.na(Ctrl_bibliographicCitation) | Ctrl_bibliographicCitation=="",
                    #                                   FALSE, TRUE) %>%
                    #   ifelse(is.na(.), FALSE,.)

      )
  }

  # for
  {

    # occ$Ctrl_hasCoordinate[is.na(occ$Ctrl_hasCoordinate)==TRUE] <- FALSE

    occ <- occ %>%
      dplyr::mutate(Ctrl_geospatial_quality = 0,
                    Ctrl_verbatim_quality = 0,
                    Ctrl_moreInformativeRecord = 0,
                    parseGBIF_digital_voucher = FALSE,
                    parseGBIF_duplicates = FALSE,
                    parseGBIF_non_groupable_duplicates = FALSE,
                    parseGBIF_num_duplicates = 0,

                    # match status between duplicates
                    parseGBIF_duplicates_grouping_status = '',

                    Ctrl_coordinates_validated_by_gbif_issue = FALSE)


    occ <- occ %>%
      dplyr::mutate(Ctrl_coordinates_validated_by_gbif_issue = ifelse(rowSums(occ[,EnumOccurrenceIssue$constant[index_tmp3 == TRUE]])==0,TRUE,FALSE))

    occ <- occ %>%
      dplyr::mutate(Ctrl_coordinates_validated_by_gbif_issue = ifelse(Ctrl_hasCoordinate == FALSE | Ctrl_decimalLatitude==0 | Ctrl_decimalLongitude==0, FALSE,
                                                                   Ctrl_coordinates_validated_by_gbif_issue))

    # aqui 15-10-2023
    occ <- occ %>%
      dplyr::mutate(Ctrl_coordinates_validated_by_gbif_issue = ifelse(is.na(Ctrl_coordinates_validated_by_gbif_issue), FALSE,
                                                                      Ctrl_coordinates_validated_by_gbif_issue))

    occ <- occ %>%
      dplyr::mutate(Ctrl_geospatial_quality = ifelse(rowSums(occ[,EnumOccurrenceIssue$constant[index_tmp3 == TRUE]])>0, -9,
                                                ifelse(rowSums(occ[,EnumOccurrenceIssue$constant[index_tmp2 == TRUE]])>0, -3,
                                                       ifelse(rowSums(occ[,EnumOccurrenceIssue$constant[index_tmp1 == TRUE]])>0, -1, 0))))

    occ <- occ %>%
      dplyr::mutate(Ctrl_geospatial_quality = ifelse(Ctrl_hasCoordinate == FALSE,
                                                     -9, #EnumOccurrenceIssue$
                                                     Ctrl_geospatial_quality))
    # occ <- occ %>%
    #   dplyr::mutate(Ctrl_geospatial_quality = ifelse(Ctrl_coordinates_validated_by_gbif_issue == FALSE,
    #                                                  -9, #EnumOccurrenceIssue$
    #                                                  Ctrl_geospatial_quality))

    occ <- occ %>%
      dplyr::mutate(Ctrl_verbatim_quality = (temColetor +
                                            temNumeroColeta +
                                            temAnoColeta +
                                            temCodigoInstituicao +
                                            temNumeroCatalogo +
                                            temLocalidade +
                                            temMunicipio +
                                            temUF +
                                            temPais + # novo
                                            # temCitacaoBibliografica
                                            temNotas  # novo
                                            # Ctrl_hasCoordinate # nao testar
                                            ))

    occ <- occ %>%
      dplyr::mutate(Ctrl_moreInformativeRecord  = ( Ctrl_geospatial_quality + Ctrl_verbatim_quality))


    occ <- occ %>%
      dplyr::select(Ctrl_key_family_recordedBy_recordNumber,

                    wcvp_plant_name_id,
                    wcvp_taxon_name,
                    wcvp_taxon_status,
                    wcvp_searchNotes,
                    # aqui
                    Ctrl_taxonRank,
                    # aqui
                    Ctrl_geospatial_quality,
                    Ctrl_verbatim_quality,
                    Ctrl_moreInformativeRecord,
                    parseGBIF_digital_voucher,
                    parseGBIF_duplicates,
                    parseGBIF_num_duplicates,
                    parseGBIF_non_groupable_duplicates,
                    # parseGBIF_unidentified_sample,
                    # parseGBIF_sample_taxon_name,
                    parseGBIF_duplicates_grouping_status,
                    # parseGBIF_sample_taxon_name_status,
                    # parseGBIF_number_taxon_names,
                    Ctrl_coordinates_validated_by_gbif_issue,

                    Ctrl_decimalLatitude,
                    Ctrl_decimalLongitude)

    # aqui add nomes e coordenadas

    occ <- occ %>%
      dplyr::mutate(
        parseGBIF_unidentified_sample = TRUE,

        # new
        parseGBIF_wcvp_plant_name_id = '',

        parseGBIF_sample_taxon_name = '',
        parseGBIF_sample_taxon_name_status = '',
        parseGBIF_number_taxon_names = 0,
        parseGBIF_useful_for_spatial_analysis = FALSE,
        parseGBIF_decimalLatitude = NA,
        parseGBIF_decimalLongitude = NA)
        # parseGBIF_notes = '',
        # parseGBIF_status = FALSE,
        # parseGBIF_freq_duplicate_or_missing_data = '')
    #
    # occ <- occ %>%
    #   dplyr::arrange(Ctrl_key_family_recordedBy_recordNumber)
    # #
    #
    # aqui chave
    index <- str_sub(occ$Ctrl_key_family_recordedBy_recordNumber, str_count(occ$Ctrl_key_family_recordedBy_recordNumber)-2, str_count(occ$Ctrl_key_family_recordedBy_recordNumber)) %in% '_NA'
    occ$Ctrl_key_family_recordedBy_recordNumber[index==TRUE] <- str_sub(occ$Ctrl_key_family_recordedBy_recordNumber[index==TRUE], 1, str_count(occ$Ctrl_key_family_recordedBy_recordNumber[index==TRUE])-2)

    recordedBy_unique <- occ$Ctrl_key_family_recordedBy_recordNumber %>% unique()

    # recordedBy_unique <- recordedBy_unique_b[64600:64700]
    #
    # r= recordedBy_unique[60]

    # japrocessado <<- rep(FALSE,length(recordedBy_unique))

    tot <- NROW(recordedBy_unique)
    s <- 0

    # r <- 'ACHATOCARPACEAE_ALCORN_'

    for (r in recordedBy_unique)
    {
      # seleção voucher digital
      {
      s <- s+1

      if(! silence == TRUE)
      {
        if (s%%100==0){print(paste0(s, ' de ',tot))}
      }

      # if(japrocessado[s]==TRUE){next}

      # print(paste0(r, ' ',s, ' de ',tot))

      FAMILY__ <-  FAMILY__recordNumber <- FAMILY_recordedBy_ <- FALSE
      sp_name <- ''

      index_occ <- (occ$Ctrl_key_family_recordedBy_recordNumber %in% r) %>% ifelse(is.na(.), FALSE,.)

      occ_key <- occ[index_occ==TRUE,]

      # num_records <- NROW(occ[index_occ==TRUE,])
      num_records <- NROW(occ_key)

      if(num_records == 0)
      {
        print(r)
        print('table')
        break
      }

      # japrocessado[s] <- TRUE

      fam <-str_sub(r,1, str_locate(r, '_')[1]-1) %>% ifelse(is.na(.), "",.)

      if(str_sub(r, str_count(r), str_count(r)) == '_' |
         grepl('__', r) |
         grepl('UNKNOWN-COLLECTOR',r))
      {

        FAMILY__ <- grepl('__', r) & str_locate(r, '__')[2] == str_count(r) %>% ifelse(is.na(.), FALSE,.)

        if(FAMILY__==FALSE)
        {

          FAMILY_recordedBy_ <- (grepl('__', r) &
                                   str_locate(r, '__')[2] != str_count(r)) |
            grepl('UNKNOWN-COLLECTOR',r) %>% ifelse(is.na(.), FALSE,.)

          if (FAMILY_recordedBy_==FALSE)
          {

            FAMILY__recordNumber <- (str_sub(r, str_count(r), str_count(r)) == '_' &
                                       !str_sub(r, str_count(r)-1, str_count(r)-1) == '_')  %>% ifelse(is.na(.), FALSE,.)

          }

        }
      }

      # unmatched
      if(FAMILY__ == TRUE | FAMILY__recordNumber == TRUE | FAMILY_recordedBy_== TRUE )
      {
        # incluir filtro espacial

        # # nomes
        # sp_name <- ifelse(occ[index_occ==TRUE, ]$wcvp_taxon_status == 'Accepted',
        #                   occ[index_occ==TRUE, ]$wcvp_taxon_name %>% as.character(),
        #                   '')

        sp_name <- ifelse(occ_key$wcvp_taxon_status == 'Accepted',
                          occ_key$wcvp_taxon_name %>% as.character(),
                          '')

        sp_id <- ifelse(occ_key$wcvp_taxon_status == 'Accepted',
                          occ_key$wcvp_plant_name_id %>% as.character(),
                          '')

        occ[index_occ==TRUE, ] <- occ_key %>%
          dplyr::mutate(parseGBIF_digital_voucher = TRUE,
                        parseGBIF_non_groupable_duplicates = TRUE,
                        parseGBIF_duplicates = FALSE,
                        parseGBIF_num_duplicates = 1,

                        # new
                        parseGBIF_wcvp_plant_name_id = sp_id,

                        # aqui
                        parseGBIF_sample_taxon_name = sp_name,
                        parseGBIF_unidentified_sample = ifelse(sp_name == '', TRUE,FALSE),

                        parseGBIF_duplicates_grouping_status = ifelse(FAMILY__==TRUE,
                                                                      'not groupable: no recordedBy and no recordNumber',
                                                                      ifelse(FAMILY__recordNumber==TRUE,
                                                                             'not groupable: no recordNumber ',
                                                                             ifelse(FAMILY_recordedBy_==TRUE,
                                                                                    'not groupable: no recordedBy', 'not groupable'))),

                        # aqui
                        parseGBIF_number_taxon_names = ifelse(sp_name == '',
                                                              0,
                                                              1),
                        parseGBIF_sample_taxon_name_status = ifelse(sp_name == '',
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

        # print('1 - Unmatched samples')
        next
      }else
      {
        # digital voucher
        {

        # occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
        #   dplyr::mutate(parseGBIF_duplicates_grouping_status = 'groupable',
        #                 parseGBIF_duplicates = num_records > 1,
        #                 parseGBIF_num_duplicates = num_records,
        #                 parseGBIF_digital_voucher = (occ[index_occ==TRUE, ]$Ctrl_moreInformativeRecord ==
        #                                                max(occ[index_occ==TRUE, ]$Ctrl_moreInformativeRecord)))

          occ[index_occ==TRUE, ] <- occ_key %>% #dplyr::select(-wcvp_taxon_name_and_wcvp_plant_name_id) %>%
            dplyr::mutate(parseGBIF_duplicates_grouping_status = 'groupable',
                          parseGBIF_duplicates = num_records > 1,
                          parseGBIF_num_duplicates = num_records,
                          parseGBIF_digital_voucher = (occ_key$Ctrl_moreInformativeRecord ==
                                                         max(occ_key$Ctrl_moreInformativeRecord)))



        #
        # 64600+

        if (sum(occ[index_occ==TRUE, ]$parseGBIF_digital_voucher)>1)
        # if (sum(occ_key$parseGBIF_digital_voucher)>1)
        {

          index_end <- occ[index_occ==TRUE, ]$parseGBIF_digital_voucher == TRUE
          # index_end <- occ_key$parseGBIF_digital_voucher == TRUE

          n_tmp <- NROW(occ[index_occ==TRUE, ]$parseGBIF_digital_voucher[index_end==TRUE])
          # n_tmp <- NROW(occ_key$parseGBIF_digital_voucher[index_end==TRUE])

          if (n_tmp==1)
          {
            occ[index_occ==TRUE, ]$parseGBIF_digital_voucher[index_end==FALSE] <- FALSE
          } else
          {
            occ[index_occ==TRUE, ]$parseGBIF_digital_voucher[index_end==FALSE] <- FALSE
            occ[index_occ==TRUE, ]$parseGBIF_digital_voucher[index_end==TRUE][2:n_tmp] <- FALSE
          }

          # print(paste0('6 - Selection of the more informative record ', 100/sum(index_end),' %'))

        }

        }

        occ_key <- occ[index_occ==TRUE,]

        occ_key$wcvp_taxon_name_and_wcvp_plant_name_id <- paste0(occ_key$wcvp_taxon_name,
                                                                 ';',
                                                                 occ_key$wcvp_plant_name_id)

        # nomes coordenadas
        {
          sp_name <- ''

          # index_occ <- (occ$Ctrl_key_family_recordedBy_recordNumber %in% r) %>% ifelse(is.na(.), FALSE,.)

          # nomes para agrupaveis
          {

            # num_records <- NROW(occ[index_occ==TRUE,])


            # if(!any(is.na(occ[index_occ==TRUE, ]$wcvp_taxon_name) == FALSE)) # any ???
            if(!any(is.na(occ_key$wcvp_taxon_name) == FALSE)) # any ???
            {

              # nomes
              sp_name <- rep('',num_records)

              sp_id <- rep('', num_records)

              occ[index_occ==TRUE, ] <- occ_key %>% dplyr::select(-wcvp_taxon_name_and_wcvp_plant_name_id) %>%
                dplyr::mutate(

                  # não é preciso entre #
                  #
                  parseGBIF_number_taxon_names = 0,

                  # new
                  parseGBIF_wcvp_plant_name_id = sp_id,

                  parseGBIF_sample_taxon_name = sp_name,
                  #
                  parseGBIF_sample_taxon_name_status = 'unidentified')


            }else
            {

              taxon_name_sample <- table(occ_key$wcvp_taxon_name_and_wcvp_plant_name_id, #occ_key$wcvp_taxon_name,
                                         occ_key$wcvp_taxon_status,
                                         exclude = NA) %>%
                data.frame() %>%
                dplyr::filter(Freq > 0) %>%
                dplyr::arrange(desc(Freq),Var1)


              num_taxon_name <- NROW(taxon_name_sample)

              if(num_taxon_name==0)
              {

                print(occ[index_occ==TRUE, ]$wcvp_taxon_name)

                print('0 - Error')

                break
              }


              if(num_taxon_name==1 & taxon_name_sample$Var2[1] %in% c('Accepted'))#,'Updated'))
              {
                # sp_name <- taxon_name_sample$Var1[1] %>% as.character()

                sp_name_id <- str_split(taxon_name_sample$Var1[1],
                          ';', simplify = TRUE)

                occ[index_occ==TRUE, ] <- occ_key %>% dplyr::select(-wcvp_taxon_name_and_wcvp_plant_name_id) %>%
                  dplyr::mutate(
                    parseGBIF_number_taxon_names = num_taxon_name,

                    # new
                    parseGBIF_wcvp_plant_name_id = sp_name_id[,2],


                    parseGBIF_sample_taxon_name = sp_name_id[,1] , #sp_name,
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

                    sp_name_id <- str_split(taxon_name_sample$Var1[ii],
                                            ';', simplify = TRUE)

                    occ[index_occ==TRUE, ] <- occ_key %>% dplyr::select(-wcvp_taxon_name_and_wcvp_plant_name_id) %>%
                      dplyr::mutate(
                        parseGBIF_number_taxon_names = num_taxon_name,

                        # new
                        parseGBIF_wcvp_plant_name_id = sp_name_id[,2],


                        parseGBIF_sample_taxon_name = sp_name_id[,1],#sp_name,
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

            index_voucher <- occ[index_occ==TRUE,]$parseGBIF_digital_voucher == TRUE
            # index_voucher <- occ_key$parseGBIF_digital_voucher == TRUE

            # se voucher tem coordennadas válidas
            if(occ$Ctrl_coordinates_validated_by_gbif_issue[index_occ==TRUE][index_voucher == TRUE] == TRUE)
            # if(any(occ_key$Ctrl_coordinates_validated_by_gbif_issue[index_voucher == TRUE] == TRUE))
            {

              occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
                dplyr::mutate(parseGBIF_decimalLatitude = occ$Ctrl_decimalLatitude[index_occ==TRUE][index_voucher == TRUE],
                              parseGBIF_decimalLongitude = occ$Ctrl_decimalLongitude[index_occ==TRUE][index_voucher == TRUE],
                              parseGBIF_useful_for_spatial_analysis = TRUE)

              # occ[index_occ==TRUE, ] <- occ_key %>%
              #   dplyr::mutate(parseGBIF_decimalLatitude = occ_key$Ctrl_decimalLatitude[index_voucher == TRUE],
              #                 parseGBIF_decimalLongitude = occ_key$Ctrl_decimalLongitude[index_voucher == TRUE],
              #                 parseGBIF_useful_for_spatial_analysis = TRUE)


            }else # se voucher não tem coordennadas válidas, tentar pegar das coordenadas
            {


              index_no_voucher_useful_for_spatial_analysis <- occ[index_occ==TRUE,]$Ctrl_coordinates_validated_by_gbif_issue == TRUE
              # index_no_voucher_useful_for_spatial_analysis <- occ_key$Ctrl_coordinates_validated_by_gbif_issue == TRUE

              if(sum(index_no_voucher_useful_for_spatial_analysis)==1)
              {

                occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
                  dplyr::mutate(parseGBIF_decimalLatitude = occ$Ctrl_decimalLatitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE],
                                parseGBIF_decimalLongitude = occ$Ctrl_decimalLongitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE],
                                parseGBIF_useful_for_spatial_analysis = TRUE)
                # occ[index_occ==TRUE, ] <- occ_key %>%
                #   dplyr::mutate(parseGBIF_decimalLatitude = occ_key$Ctrl_decimalLatitude[index_no_voucher_useful_for_spatial_analysis == TRUE],
                #                 parseGBIF_decimalLongitude = occ_key$Ctrl_decimalLongitude[index_no_voucher_useful_for_spatial_analysis == TRUE],
                #                 parseGBIF_useful_for_spatial_analysis = TRUE)


              }

              # pegar a com maior score
              if(sum(index_no_voucher_useful_for_spatial_analysis)>1)
              {

                geospatial_quality_tmp <- max(occ$Ctrl_geospatial_quality[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE])
                # geospatial_quality_tmp <- max(occ_key$Ctrl_geospatial_quality[index_no_voucher_useful_for_spatial_analysis == TRUE])

                index_geospatial_quality <- occ$Ctrl_geospatial_quality[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE] == geospatial_quality_tmp
                # index_geospatial_quality <- occ_key$Ctrl_geospatial_quality[index_no_voucher_useful_for_spatial_analysis == TRUE] == geospatial_quality_tmp

                occ[index_occ==TRUE, ] <- occ[index_occ==TRUE, ] %>%
                  dplyr::mutate(parseGBIF_decimalLatitude = occ$Ctrl_decimalLatitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE][index_geospatial_quality == TRUE][1],
                                parseGBIF_decimalLongitude = occ$Ctrl_decimalLongitude[index_occ==TRUE][index_no_voucher_useful_for_spatial_analysis == TRUE][index_geospatial_quality == TRUE][1],
                                parseGBIF_useful_for_spatial_analysis = TRUE)
                # occ[index_occ==TRUE, ] <- occ_key %>%
                #   dplyr::mutate(parseGBIF_decimalLatitude = occ_key$Ctrl_decimalLatitude[index_no_voucher_useful_for_spatial_analysis == TRUE][index_geospatial_quality == TRUE][1],
                #                 parseGBIF_decimalLongitude = occ_key$Ctrl_decimalLongitude[index_no_voucher_useful_for_spatial_analysis == TRUE][index_geospatial_quality == TRUE][1],
                #                 parseGBIF_useful_for_spatial_analysis = TRUE)


              }

            }

          }

        }
      }
    }
  }
}

  # salvar

  occ <- occ %>%
    dplyr::select(Ctrl_geospatial_quality,
                  Ctrl_verbatim_quality,
                  Ctrl_moreInformativeRecord,
                  parseGBIF_digital_voucher,
                  parseGBIF_duplicates,
                  parseGBIF_num_duplicates,
                  parseGBIF_non_groupable_duplicates,
                  parseGBIF_duplicates_grouping_status,
                  Ctrl_coordinates_validated_by_gbif_issue,

                  parseGBIF_unidentified_sample,

                  # new
                  parseGBIF_wcvp_plant_name_id,

                  parseGBIF_sample_taxon_name,
                  parseGBIF_sample_taxon_name_status,
                  parseGBIF_number_taxon_names,
                  parseGBIF_useful_for_spatial_analysis,
                  parseGBIF_decimalLatitude,
                  parseGBIF_decimalLongitude)
                  # parseGBIF_freq_duplicate_or_missing_data)


  # in dup out_to_recover
  {

    occ_in_2 <- cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, occ) %>%
      dplyr::filter(parseGBIF_digital_voucher == TRUE,
                    parseGBIF_unidentified_sample == FALSE, # parseGBIF_sample_taxon_name_status %in% c('identified','divergent identifications'),
                    parseGBIF_useful_for_spatial_analysis == TRUE)

    occ_dup <- cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, occ) %>%
      dplyr::filter(parseGBIF_digital_voucher == FALSE)

    occ_out_to_recover <- cbind(occ_in, occ_wcvp_check_name, occ_collectorsDictionary, occ) %>%
      dplyr::filter(parseGBIF_digital_voucher == TRUE,
                    (parseGBIF_unidentified_sample == TRUE |
                       parseGBIF_useful_for_spatial_analysis == FALSE))

    occ_all <- rbind(occ_in_2 %>%
                       dplyr::mutate(parseGBIF_dataset_result='useable'),
                     occ_out_to_recover %>%
                       dplyr::mutate(parseGBIF_dataset_result='unusable'),
                     occ_dup %>%
                       dplyr::mutate(parseGBIF_dataset_result='duplicate'))


    # wcvp sample
    {
      # x <- occ_digital_voucher$all_data

      xn <- occ_wcvp_check_name %>%
        dplyr::mutate(wcvp_plant_name_id = as.character(wcvp_plant_name_id)) %>%
        dplyr::select(wcvp_plant_name_id,
                      wcvp_taxon_rank,
                      wcvp_taxon_status,
                      wcvp_family,
                      wcvp_taxon_name,
                      wcvp_taxon_authors,
                      wcvp_reviewed) %>%
        dplyr::distinct_all()

      colnames(xn) <- paste0('parseGBIF_',colnames(xn))

      occ_all <- left_join(occ_all,
                     xn,
                     by = 'parseGBIF_wcvp_plant_name_id') %>%
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
                      Ctrl_issue,

                      Ctrl_taxonRank,

                      Ctrl_nameRecordedBy_Standard,
                      Ctrl_recordNumber_Standard,
                      Ctrl_key_family_recordedBy_recordNumber,
                      Ctrl_geospatial_quality,
                      Ctrl_verbatim_quality,
                      Ctrl_moreInformativeRecord,
                      Ctrl_coordinates_validated_by_gbif_issue,

                      wcvp_plant_name_id,
                      wcvp_taxon_rank,
                      wcvp_taxon_status,
                      wcvp_family,
                      wcvp_taxon_name,
                      wcvp_taxon_authors,
                      wcvp_reviewed,
                      wcvp_searchedName,
                      wcvp_searchNotes,

                      parseGBIF_digital_voucher,
                      parseGBIF_duplicates,
                      parseGBIF_num_duplicates,
                      parseGBIF_non_groupable_duplicates,
                      parseGBIF_duplicates_grouping_status,
                      parseGBIF_unidentified_sample,
                      parseGBIF_sample_taxon_name,
                      parseGBIF_sample_taxon_name_status,
                      parseGBIF_number_taxon_names,
                      parseGBIF_useful_for_spatial_analysis,
                      parseGBIF_decimalLatitude,
                      parseGBIF_decimalLongitude,
                      parseGBIF_dataset_result,

                      # parseGBIF_freq_duplicate_or_missing_data,
                      # parseGBIF_duplicates_map,
                      # parseGBIF_merged_fields,
                      # parseGBIF_merged,

                      parseGBIF_wcvp_plant_name_id,
                      parseGBIF_wcvp_taxon_rank,
                      parseGBIF_wcvp_taxon_status,
                      parseGBIF_wcvp_family,
                      parseGBIF_wcvp_taxon_name,
                      parseGBIF_wcvp_taxon_authors,
                      # parseGBIF_wcvp_accepted_plant_name_id,
                      parseGBIF_wcvp_reviewed)
                      # parseGBIF_wcvp_searchedName,
                      # parseGBIF_wcvp_searchNotes)

    }


    }

  # occ <- occ %>%
  #   dplyr::arrange(Ctrl_key_family_recordedBy_recordNumber)

  return(list(
    occ_digital_voucher = occ_all,
    occ_results = occ  ))
}

