#' @title Checking coordinates, centroids, artificial points and others
#' @name parse_coordinates
#'
#' @description Checking coordinates, centroids, artificial points, also makes use of basic functions of CoordinateCleaner and other packages
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param iso2_field_name indicates the name of the field with ISO2 code of the countries
#' @param file_centroids 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataRaw/parseGBIF_GADM_centroids.CSV'
#' @param centroid_round point_11_1_km, point_1_1_km, point_110m or point_11m
#' @details Adds the following fields, result of the coordinate checking process:
#' parseGBIF_GADM_centroids,
#' parseGBIF_GADM_centroids_level,
#' parseGBIF_coordinate_status,
#' .coordinates_outOfRange, .val,.zer,.sea,.equ,.cen,.cap,.urb,.con,.inst,.dup
#'
#' @return
#'  list with two data frames, occ, with the original data set plus two columns, parseGBIF_countryCode_ISO3 and
#'  parseGBIF_countryName_en and countrycodelist, with the list of countries found with all the columns of countrycode::codelist
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{prepare_gbif_occurrence_data}}, \code{\link[parseGBIF]{download_gbif_data_from_doi}}
#'
#' @examples
#' \donttest{
#' help(standardize_country_from_iso2)
#'
#' occ <- prepare_gbif_occurrence_data(gbif_occurrece_file =  'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt',
#'                                     columns = 'standard')
#' x <- standardize_country_from_iso2(occ = occ,
#'                                      iso2_field_name = 'Ctrl_countryCode',
#'                                      return_fields = c('iso3c','country.name.en'))
#' colnames(x$occ)
#' head(x$countrycodelist)
#' }
#' @import bdc
#' @import dplyr
#' @import plyr
#' @import terra
#' @import CoordinateCleaner
#' @import sf
#' @import readr
#' @export
parse_coordinates <- function(occ = NA,
                             file_occ = NA,
                             iso2_field_name = 'Ctrl_countryCode',
                             file_centroids = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataRaw/parseGBIF_GADM_centroids.CSV',
                             centroid_round = 'point_110m')
{

  if(is.na(path_centroids))
  {
    path_word <- tempdir()
  }else
  {
    path_word <- path_centroids
  }

  centroids <- get_centroids(file_centroids = file_centroids) %>%
    dplyr::mutate(point_11_1_km = paste0(format(round(lon, 1), nsmall = 1), ' _ ', format(round(lat, 1), nsmall = 1))) %>%
    dplyr::mutate(point_1_1_km = paste0(format(round(lon, 2), nsmall = 2), ' _ ', format(round(lat, 2), nsmall = 2))) %>%
    dplyr::mutate(point_110m = paste0(format(round(lon, 3), nsmall = 3), ' _ ', format(round(lat, 3), nsmall = 3))) %>%
    dplyr::mutate(point_11m = paste0(format(round(lon, 4), nsmall = 4), ' _ ', format(round(lat, 4), nsmall = 4)))

  centroids <- centroids %>% filter(level %in% c(0,1))

  if(NROW(occ)==1 & !is.na(file_occ))
  {
    occ <- readr::read_csv(file_occ,
                           locale = readr::locale(encoding = "UTF-8"),
                           show_col_types = FALSE)
  }

  # standardize country codes ISO2 to ISO3
  occ <- standardize_country_from_iso2(occ = occ,
                                       silence = T)$occ

  # Warning messages:
  #   1: GEOS support is provided by the sf and terra packages among others
  # 2: In explodePolygons(x, ...) : No rgeos support in sp from October 2023;
  # see https://r-spatial.org/r/2023/05/15/evolution4.html
  # 3: In explodePolygons(x, ...) : No rgeos support in sp from October 2023;
  # see https://r-spatial.org/r/2023/05/15/evolution4.html
  # 4: In explodePolygons(x, ...) : No rgeos support in sp from October 2023;
  # see https://r-spatial.org/r/2023/05/15/evolution4.html
  # 5: In spTransform(xSP, CRSobj, ...) :
  #   NULL source CRS comment, falling back to PROJ string
  # 6: In reproj(ref) :
  #   reprojecting reference to '+proj=longlat +datum=WGS84 +no_defs'


  occ_cc <- occ %>%
    dplyr::select(Ctrl_gbifID,
                  parseGBIF_dataset_result,
                  parseGBIF_sample_taxon_name,
                  parseGBIF_decimalLongitude,
                  parseGBIF_decimalLatitude,
                  parseGBIF_useful_for_spatial_analysis,
                  parseGBIF_countryCode_ISO3) %>%
    dplyr::rename(species = parseGBIF_sample_taxon_name,
                  decimalLongitude  = parseGBIF_decimalLongitude,
                  decimalLatitude = parseGBIF_decimalLatitude) %>%
    dplyr::mutate(decimalLongitude  = as.numeric(decimalLongitude),
                  decimalLatitude = as.numeric(decimalLatitude)) %>%

    # dplyr::mutate(
    #               # point = paste0(format(round(decimallongitude, n_dec_round), nsmall = n_dec_round), ' _ ', format(round(decimallatitude, n_dec_round), nsmall = n_dec_round)),
    #               point_11_1_km = paste0(format(round(decimalLongitude, 1), nsmall = 1), ' _ ', format(round(decimalLatitude, 1), nsmall = 1)),
    #               point_1_1_km = paste0(format(round(decimalLongitude, 2), nsmall = 2), ' _ ', format(round(decimalLatitude, 2), nsmall = 2)),
    #               point_110m = paste0(format(round(decimalLongitude, 3), nsmall = 3), ' _ ', format(round(decimalLatitude, 3), nsmall = 3)),
    #               point_11m = paste0(format(round(decimalLongitude, 4), nsmall = 4), ' _ ', format(round(decimalLatitude, 4), nsmall = 4))) %>%
    dplyr::mutate(.val = FALSE,
                  .zer = FALSE,
                  .sea = FALSE,
                  .equ = FALSE,
                  .cen = FALSE,
                  .cap = FALSE,
                  .urb = FALSE,
                  # .con = TRUE,
                  .inst = FALSE,
                  .dup = FALSE,
                  parseGBIF_coordinate_status = '',
                  .before = 1)

  # CoordinateCleaner and bdc
  {
    occ_cc <- bdc::bdc_coordinates_outOfRange(
      data = occ_cc,
      lat = "decimalLatitude",
      lon = "decimalLongitude")

    # colnames(occ_cc)
    # NROW(occ_cc)

    index <- occ_cc$.coordinates_outOfRange == TRUE &
      occ_cc$parseGBIF_useful_for_spatial_analysis
    # index <- occ_cc$parseGBIF_useful_for_spatial_analysis

    occ_cc$.val[index==TRUE] <- CoordinateCleaner::cc_val(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')
    # NROW(occ_cc)

    index <- index %in% TRUE &  occ_cc$.val %in% TRUE

    occ_cc$.zer[index==TRUE] <- CoordinateCleaner::cc_zero(x=occ_cc[index==TRUE,],
                                                           value = 'flagged')
    # NROW(occ_cc)

    index <- index %in% TRUE & occ_cc$.zer %in% TRUE

    occ_cc$.equ[index==TRUE] <- CoordinateCleaner::cc_equ(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')
    # NROW(occ_cc)

    index <- index %in% TRUE & occ_cc$.equ %in% TRUE

    occ_cc$.sea[index==TRUE] <- CoordinateCleaner::cc_sea(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    # NROW(occ_cc)

    occ_cc$.cen[index==TRUE] <- CoordinateCleaner::cc_cen(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    # NROW(occ_cc)

    occ_cc$.cap[index==TRUE] <- CoordinateCleaner::cc_cap(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    # NROW(occ_cc)

    occ_cc$.urb[index==TRUE] <- CoordinateCleaner::cc_urb(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    # NROW(occ_cc)
    # https://r-spatial.org/r/2023/05/15/evolution4.html
    occ_cc$.inst[index==TRUE] <- CoordinateCleaner::cc_inst(x=occ_cc[index==TRUE,],
                                                            value = 'flagged')
    # NROW(occ_cc)

    # # This testing for duplicates may not be necessary
    occ_cc$.dup[index==TRUE] <- CoordinateCleaner::cc_dupl(x=occ_cc[index==TRUE,],
                                                           value = 'flagged')

    # # This country test takes a long time to run
    # index <- index == TRUE & occ_cc$parseGBIF_countryCode_ISO3!=''
    # occ_cc$.con[index==TRUE] <- CoordinateCleaner::cc_coun(x = occ_cc[index==TRUE,],
    #                                                        lon = "decimalLongitude",
    #                                                        lat = "decimalLatitude",
    #                                                        iso3 = "parseGBIF_countryCode_ISO3",
    #                                                        value = "flagged",
    #                                                        ref = rnaturalearth::ne_countries(scale = "medium"),# +proj=longlat +datum=WGS84 +no_defs
    #                                                        ref_col = "iso_a3")
    }

  # occ e sp por ponto, se proximo de centroid
  {

    # # point_11_1_km
    # {
    #   point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_11_1_km, Ctrl_gbifID
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_11_1_km, Ctrl_gbifID
    #                     ORDER BY point_11_1_km, Ctrl_gbifID")
    #
    #   point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_11_1_km
    #                     FROM point_hot_oc
    #                     GROUP BY point_11_1_km
    #                     ORDER BY count(Ctrl_gbifID) DESC") %>%
    #     dplyr::rename(n_unique_collection_event_11_1_km=`count(Ctrl_gbifID)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_oc2,
    #                       by = 'point_11_1_km')
    #
    #
    #   point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_11_1_km, species
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_11_1_km, species
    #                     ORDER BY point_11_1_km, species")
    #
    #   point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_11_1_km
    #                     FROM point_hot_sp
    #                     GROUP BY point_11_1_km
    #                     ORDER BY count(species) DESC") %>%
    #     dplyr::rename(n_taxon_name_11_1_km=`count(species)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_sp2,
    #                       by = 'point_11_1_km')
    # }
    #
    # # point_1_1_km
    # {
    #   point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_1_1_km, Ctrl_gbifID
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_1_1_km, Ctrl_gbifID
    #                     ORDER BY point_1_1_km, Ctrl_gbifID")
    #
    #   point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_1_1_km
    #                     FROM point_hot_oc
    #                     GROUP BY point_1_1_km
    #                     ORDER BY count(Ctrl_gbifID) DESC") %>%
    #     dplyr::rename(n_unique_collection_event_1_1_km=`count(Ctrl_gbifID)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_oc2,
    #                       by = 'point_1_1_km')
    #
    #
    #   point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_1_1_km, species
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_1_1_km, species
    #                     ORDER BY point_1_1_km, species")
    #
    #   point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_1_1_km
    #                     FROM point_hot_sp
    #                     GROUP BY point_1_1_km
    #                     ORDER BY count(species) DESC") %>%
    #     dplyr::rename(n_taxon_name_1_1_km=`count(species)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_sp2,
    #                       by = 'point_1_1_km')
    # }
    #
    # # point_110m
    # {
    #   point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_110m, Ctrl_gbifID
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_110m, Ctrl_gbifID
    #                     ORDER BY point_110m, Ctrl_gbifID")
    #
    #   point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_110m
    #                     FROM point_hot_oc
    #                     GROUP BY point_110m
    #                     ORDER BY count(Ctrl_gbifID) DESC") %>%
    #     dplyr::rename(n_unique_collection_event_110m=`count(Ctrl_gbifID)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_oc2,
    #                       by = 'point_110m')
    #
    #
    #   point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_110m, species
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_110m, species
    #                     ORDER BY point_110m, species")
    #
    #   point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_110m
    #                     FROM point_hot_sp
    #                     GROUP BY point_110m
    #                     ORDER BY count(species) DESC") %>%
    #     dplyr::rename(n_taxon_name_110m=`count(species)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_sp2,
    #                       by = 'point_110m')
    # }
    #
    # # point_11m
    # {
    #   point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_11m, Ctrl_gbifID
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_11m, Ctrl_gbifID
    #                     ORDER BY point_11m, Ctrl_gbifID")
    #
    #   point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_11m
    #                     FROM point_hot_oc
    #                     GROUP BY point_11m
    #                     ORDER BY count(Ctrl_gbifID) DESC") %>%
    #     dplyr::rename(n_unique_collection_event_11m=`count(Ctrl_gbifID)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_oc2,
    #                       by = 'point_11m')
    #
    #
    #   point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_11m, species
    #                     FROM occ_cc
    #                     WHERE parseGBIF_dataset_result = 'useable'
    #                     GROUP BY point_11m, species
    #                     ORDER BY point_11m, species")
    #
    #   point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_11m
    #                     FROM point_hot_sp
    #                     GROUP BY point_11m
    #                     ORDER BY count(species) DESC") %>%
    #     dplyr::rename(n_taxon_name_11m=`count(species)`)
    #
    #   occ_cc <- left_join(occ_cc,
    #                       point_hot_sp2,
    #                       by = 'point_11m')
    # }

    colx <- c(centroid_round,'level')
    occ_cc <- left_join(occ_cc,
                        centroids %>%
                          dplyr::select(colx) %>%
                          dplyr::rename(parseGBIF_GADM_centroids_level =level),
                        by = centroid_round)

    occ_cc$parseGBIF_GADM_centroids <- is.na(occ_cc$parseGBIF_GADM_centroids_level)

    # centroids$level %>% unique()
    # occ_cc$parseGBIF_GADM_centroids_level %>% unique()
    # occ_cc$point %>% unique()
    # View(occ_cc)
    # summary(occ_cc)

  }

  # .parseGBIF_coordinate_status
  {
    ####
    # Nadia, please check the need to include the country to create this summary (e.g. if country fails)
    ####

    geo_issue <- !(occ_cc$.val==FALSE | occ_cc$.equ==FALSE | occ_cc$.zer==FALSE | occ_cc$.coordinates_outOfRange==FALSE)
    geo_issue_urb <- !(occ_cc$.cen==FALSE | occ_cc$.cap==FALSE | occ_cc$.urb==FALSE | occ_cc$.inst==FALSE | occ_cc$.sea==FALSE)

    occ_cc$parseGBIF_coordinate_status <- ifelse(geo_issue==FALSE,'danger',ifelse(geo_issue_urb==FALSE,'warning','success'))

    occ <- cbind(occ_cc %>% dplyr::select(point_11_1_km,
                                          n_taxon_name_11_1_km,
                                          n_unique_collection_event_11_1_km,
                                          point_1_1_km,
                                          n_taxon_name_1_1_km,
                                          n_unique_collection_event_1_1_km,
                                          point_110m,
                                          n_taxon_name_110m,
                                          n_unique_collection_event_110m,
                                          point_11m,
                                          n_taxon_name_11m,
                                          n_unique_collection_event_11m,
                                          parseGBIF_GADM_centroids,
                                          parseGBIF_GADM_centroids_level,
                                          parseGBIF_coordinate_status,
                                          .coordinates_outOfRange,
                                          .val,.zer,.sea,.equ,.cen,.cap,.urb,.inst,.dup),
                 occ)

    occ$parseGBIF_useful_for_spatial_analysis <- ifelse(geo_issue==FALSE,geo_issue, occ$parseGBIF_useful_for_spatial_analysis)

    print(NROW(occ_cc))
    print(paste0('.coordinates_outOfRange', '-', occ_cc %>% dplyr::filter(.coordinates_outOfRange == FALSE) %>% NROW()))
    print(paste0('.val', '-', occ_cc %>% dplyr::filter(.val == FALSE) %>% NROW()))
    print(paste0('.zer', '-', occ_cc %>% dplyr::filter(.zer == FALSE) %>% NROW()))
    print(paste0('.cen', '-', occ_cc %>% dplyr::filter(.cen == FALSE) %>% NROW()))
    print(paste0('.equ', '-', occ_cc %>% dplyr::filter(.equ == FALSE) %>% NROW()))
    print(paste0('.cap', '-', occ_cc %>% dplyr::filter(.cap == FALSE) %>% NROW()))
    print(paste0('.urb', '-', occ_cc %>% dplyr::filter(.urb == FALSE) %>% NROW()))
    print(paste0('.inst', '-', occ_cc %>% dplyr::filter(.inst == FALSE) %>% NROW()))
    print(paste0('.dup', '-', occ_cc %>% dplyr::filter(.dup == FALSE) %>% NROW()))
    print(paste0('.sea', '-', occ_cc %>% dplyr::filter(.sea == FALSE) %>% NROW()))


    # parseGBIF_useful_for_spatial_analysis

  }

  return(occ)
}
