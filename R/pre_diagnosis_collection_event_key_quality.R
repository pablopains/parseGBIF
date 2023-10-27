#' @title Pre-diagnosis of collection event key quality
#'
#' @name pre_diagnosis_collection_event_key_quality
#'
#' @description Pre-diagnosis of collection event key quality
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param n_occ_min Minimum records per collector per collection
#' @param n_show Inconsistency number to list
#'
#' @details Pre-diagnosis of collection event key quality
#'
#' @return
#' Text report
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{prepare_gbif_occurrence_data}}, \code{\link[parseGBIF]{extract_gbif_issue}}
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
pre_diagnosis_collection_event_key_quality <- function(occ = NA,
                                   n_occ_min = 10,
                                   n_show=50)
{

  {
    occ$Ctrl_recordNumber_Standard <- ''

    occ$Ctrl_recordNumber_Standard <- stringr::str_replace_all(occ$Ctrl_recordNumber, "[^0-9]", "")


    occ$Ctrl_recordNumber_Standard <- ifelse(is.na(occ$Ctrl_recordNumber_Standard) |
                                               occ$Ctrl_recordNumber_Standard=='',"",occ$Ctrl_recordNumber_Standard  %>% strtoi())
    # tirar o NA do numero
    occ$Ctrl_recordNumber_Standard <- ifelse(is.na(occ$Ctrl_recordNumber_Standard),'',occ$Ctrl_recordNumber_Standard)


    occ$Ctrl_recordNumber <- occ$Ctrl_recordNumber_Standard

    occ$Ctrl_recordNumber <- ifelse(occ$Ctrl_recordNumber=='', 'no recordNumber', occ$Ctrl_recordNumber)

  }

  {
    family <- table(occ$Ctrl_family) %>%
      data.frame() %>%
      dplyr::rename(family = Var1) %>%
      dplyr::arrange(desc(Freq), family)


    recordedBy <- table(occ$Ctrl_recordedBy) %>%
      data.frame() %>%
      dplyr::rename(recordedBy = Var1) %>%
      dplyr::arrange(desc(Freq), recordedBy)


    # View(recordedBy)

    collectionCode <- table(occ$Ctrl_collectionCode) %>%
      data.frame() %>%
      dplyr::rename(collectionCode = Var1) %>%
      dplyr::arrange(desc(Freq), collectionCode)

    # View(collectionCode)

    recordedBy_collectionCode <- table(occ$Ctrl_recordedBy,
                                       occ$Ctrl_collectionCode) %>%
      data.frame() %>%
      dplyr::filter(Freq > n_occ_min) %>%
      dplyr::rename(recordedBy = Var1,
                    collectionCode = Var2) %>%
      dplyr::arrange(desc(Freq), recordedBy, collectionCode)

  }

  # results
  {
    results <- left_join(recordedBy_collectionCode,
                         recordedBy %>%
                           dplyr::rename(n_recordedBy = Freq),
                         by = 'recordedBy')

    results <- left_join(results,
                         collectionCode %>%
                           dplyr::rename(n_collectionCode = Freq),
                         by = 'collectionCode')


    results <- results %>%
      dplyr::mutate(p_recordedBy = round(Freq/n_recordedBy*100,2),
                    p_collectionCode = round(Freq/n_collectionCode*100,2) ) %>%
      dplyr::arrange(desc(p_collectionCode))

    View(results)

    results_II <- results %>%
      dplyr::filter(p_collectionCode>10)

  }

  if(NROW(family)>1)
  {
    print(paste0(NROW(family), ' family in database '))
    print(family)
    print('-------')
  }

  print(paste0(NROW(results_II), ' suspected recordedBy in collectionCode '))

  i=1
  diagnosis <- function(i,
                        results,
                        occ,
                        n_show=n_show)
  {
    qualidade <- FALSE

    print('-------')


    print(paste0(i,'/',NROW(results_II), ' - [',results$recordedBy[i], '] is recordedBy of ', results$p_recordedBy[i], ' % of the collectionCode [' ,results$collectionCode[i] ,'] samples (',results$Freq[i],') '))

    x <- occ %>% dplyr::filter(Ctrl_recordedBy == as.character(results$recordedBy[i]),
                               Ctrl_collectionCode == results$collectionCode[i])

    duplicates <- table(x$Ctrl_recordNumber) %>%
      data.frame() %>%
      dplyr::rename(recordNumber = Var1) %>%
      dplyr::arrange(desc(Freq), recordNumber)


    id=1
    for(id in 1:NROW(duplicates))
    {
      x2 <- occ %>% dplyr::filter(Ctrl_recordedBy == as.character(results$recordedBy[i]),
                                  Ctrl_collectionCode == results$collectionCode[i],
                                  Ctrl_recordNumber == duplicates$recordNumber[id])


      x2$Ctrl_recordNumber
      nomes_amostra <- table(x2$Ctrl_scientificName) %>%
        data.frame() %>%
        dplyr::rename(scientificName = Var1) %>%
        dplyr::arrange(desc(Freq), scientificName)

      if(NROW(nomes_amostra)<3){next}
      #
      # if(round(NROW(nomes_amostra)/NROW(x2)*100,2)<50){next}


      if(duplicates$recordNumber[id] == 'no recordNumber' )# &       round(NROW(nomes_amostra)/NROW(x2)*100,2) > 25)
      {
        print(paste0('[',results$recordedBy[i], '] ',round(NROW(nomes_amostra)/NROW(x2)*100,2), ' % of the samples no recordNumber'))
        qualidade <- TRUE
        next
      }


      # print(paste0(duplicates$recordNumber[id], ' : ',  NROW(nomes_amostra)))
      print(paste0(results$recordedBy[i], ' / ', duplicates$recordNumber[id], ' / ',  NROW(nomes_amostra), ' diferents scientificName in ',  NROW(x2),' duplicates - ', round(NROW(nomes_amostra)/NROW(x2)*100,2) ,' %' ))
      if(id==n_show){break}

    }

    # Sys.sleep(1)

  }

  # diagnosis(i, results_II, occ)

  for(i in 1:NROW(results_II)){diagnosis(i = i,
                                         results = results_II,
                                         occ = occ,
                                         n_show = n_show)}


}
