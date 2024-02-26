#' @title Checking coordinates, centroids, artificial points and others
#' @name parse_coordinates
#'
#' @description Checking coordinates, centroids, artificial points, also makes use of basic functions of CoordinateCleaner and other packages
#'
#' @param occ GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')
#' @param iso2_field_name indicates the name of the field with ISO2 code of the countries
#'
#' @details Returns the last name
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
#' @import leaflet
#' @import maptools
#' @import countrycode
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
                             # centroids = NA,
                             path_centroids = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataRaw',
                             scale = 110)
{

  if(is.na(path_centroids))
  {
    path_word <- tempdir()
  }else
  {
    path_word <- path_centroids
  }

  centroids <- get_centroids(path_centroids=path_centroids) %>%
    # dplyr::mutate(point = paste0(format(round(lon, n_dec_round), nsmall = n_dec_round), ' _ ', format(round(lat, n_dec_round), nsmall = n_dec_round))) %>%
    dplyr::mutate(point_11_1_km = paste0(format(round(lon, 1), nsmall = 1), ' _ ', format(round(lat, 1), nsmall = 1))) %>%
    dplyr::mutate(point_1_1_km = paste0(format(round(lon, 2), nsmall = 2), ' _ ', format(round(lat, 2), nsmall = 2))) %>%
    dplyr::mutate(point_110m = paste0(format(round(lon, 3), nsmall = 3), ' _ ', format(round(lat, 3), nsmall = 3))) %>%
    dplyr::mutate(point_11m = paste0(format(round(lon, 4), nsmall = 4), ' _ ', format(round(lat, 4), nsmall = 4)))


  if(NROW(occ)==1 & !is.na(file_occ))
  {
    occ <- readr::read_csv(file_occ,
                           locale = readr::locale(encoding = "UTF-8"),
                           show_col_types = FALSE)
  }

  # standardize country codes ISO2 to ISO3
  occ <- standardize_country_from_iso2(occ = occ,
                                       iso2_field_name = iso2_field_name,#'Ctrl_countryCode',
                                       silence = FALSE)$occ

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
                  decimallongitude  = parseGBIF_decimalLongitude,
                  decimallatitude = parseGBIF_decimalLatitude) %>%
    dplyr::mutate(
                  # point = paste0(format(round(decimallongitude, n_dec_round), nsmall = n_dec_round), ' _ ', format(round(decimallatitude, n_dec_round), nsmall = n_dec_round)),
                  point_11_1_km = paste0(format(round(decimallongitude, 1), nsmall = 1), ' _ ', format(round(decimallatitude, 1), nsmall = 1)),
                  point_1_1_km = paste0(format(round(decimallongitude, 2), nsmall = 2), ' _ ', format(round(decimallatitude, 2), nsmall = 2)),
                  point_110m = paste0(format(round(decimallongitude, 3), nsmall = 3), ' _ ', format(round(decimallatitude, 3), nsmall = 3)),
                  point_11m = paste0(format(round(decimallongitude, 4), nsmall = 4), ' _ ', format(round(decimallatitude, 4), nsmall = 4))) %>%
    dplyr::mutate(.val = FALSE,
                  .zer = FALSE,
                  .sea = FALSE,
                  .equ = FALSE,
                  .cen = FALSE,
                  .cap = FALSE,
                  .urb = FALSE,
                  .con = FALSE,
                  .inst = FALSE,
                  .dup = FALSE,
                  parseGBIF_coordinate_status = '',
                  .before = 1)

  # CoordinateCleaner and bdc
  {
    occ_cc <- bdc::bdc_coordinates_outOfRange(
      data = occ_cc,
      lat = "decimallatitude",
      lon = "decimallongitude")

    colnames(occ_cc)
    NROW(occ_cc)

    # index <- index == TRUE & occ_cc$.coordinates_outOfRange == TRUE
    index <- occ_cc$parseGBIF_useful_for_spatial_analysis

    occ_cc$.val[index==TRUE] <- CoordinateCleaner::cc_val(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')
    NROW(occ_cc)

    index <- index %in% TRUE &  occ_cc$.val %in% TRUE

    occ_cc$.zer[index==TRUE] <- CoordinateCleaner::cc_zero(x=occ_cc[index==TRUE,],
                                                           value = 'flagged')
    NROW(occ_cc)

    index <- index %in% TRUE & occ_cc$.zer %in% TRUE

    occ_cc$.equ[index==TRUE] <- CoordinateCleaner::cc_equ(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')
    NROW(occ_cc)

    index <- index %in% TRUE & occ_cc$.equ %in% TRUE

    occ_cc$.sea[index==TRUE] <- CoordinateCleaner::cc_sea(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    NROW(occ_cc)

    occ_cc$.cen[index==TRUE] <- CoordinateCleaner::cc_cen(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    NROW(occ_cc)

    occ_cc$.cap[index==TRUE] <- CoordinateCleaner::cc_cap(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    NROW(occ_cc)

    occ_cc$.urb[index==TRUE] <- CoordinateCleaner::cc_urb(x=occ_cc[index==TRUE,],
                                                          value = 'flagged')

    NROW(occ_cc)
    # https://r-spatial.org/r/2023/05/15/evolution4.html
    occ_cc$.inst[index==TRUE] <- CoordinateCleaner::cc_inst(x=occ_cc[index==TRUE,],
                                                            value = 'flagged')
    NROW(occ_cc)

    # # This testing for duplicates may not be necessary
    occ_cc$.dup[index==TRUE] <- CoordinateCleaner::cc_dupl(x=occ_cc[index==TRUE,],
                                                           value = 'flagged')

    # This country test takes a long time to run
    index <- index == TRUE & occ_cc$parseGBIF_countryCode_ISO3!=''
    occ_cc$.con[index==TRUE] <- CoordinateCleaner::cc_coun(x = occ_cc[index==TRUE,],
                                                           lon = "decimallongitude",
                                                           lat = "decimallatitude",
                                                           iso3 = "parseGBIF_countryCode_ISO3",
                                                           value = "flagged",
                                                           ref = rnaturalearth::ne_countries(scale = "medium"),# +proj=longlat +datum=WGS84 +no_defs
                                                           ref_col = "iso_a3")
    }

  # occ e sp por ponto, se proximo de centroid

  {

    # point
    {
      # point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point, Ctrl_gbifID
      #                   FROM occ_cc
      #                   WHERE parseGBIF_dataset_result = 'useable'
      #                   GROUP BY point, Ctrl_gbifID
      #                   ORDER BY point, Ctrl_gbifID")
      #
      # point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point
      #                   FROM point_hot_oc
      #                   GROUP BY point
      #                   ORDER BY count(Ctrl_gbifID) DESC") %>%
      #   dplyr::rename(n_unique_collection_event=`count(Ctrl_gbifID)`)
      #
      # occ_cc <- left_join(occ_cc,
      #                     point_hot_oc2,
      #                     by = 'point')
      #
      #
      # point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point, species
      #                   FROM occ_cc
      #                   WHERE parseGBIF_dataset_result = 'useable'
      #                   GROUP BY point, species
      #                   ORDER BY point, species")
      #
      # point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point
      #                   FROM point_hot_sp
      #                   GROUP BY point
      #                   ORDER BY count(species) DESC") %>%
      #   dplyr::rename(n_taxon_name=`count(species)`)
      #
      # occ_cc <- left_join(occ_cc,
      #                     point_hot_sp2,
      #                     by = 'point')
    }

    # point_11_1_km
    {
      point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_11_1_km, Ctrl_gbifID
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_11_1_km, Ctrl_gbifID
                        ORDER BY point_11_1_km, Ctrl_gbifID")

      point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_11_1_km
                        FROM point_hot_oc
                        GROUP BY point_11_1_km
                        ORDER BY count(Ctrl_gbifID) DESC") %>%
        dplyr::rename(n_unique_collection_event_11_1_km=`count(Ctrl_gbifID)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_oc2,
                          by = 'point_11_1_km')


      point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_11_1_km, species
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_11_1_km, species
                        ORDER BY point_11_1_km, species")

      point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_11_1_km
                        FROM point_hot_sp
                        GROUP BY point_11_1_km
                        ORDER BY count(species) DESC") %>%
        dplyr::rename(n_taxon_name_11_1_km=`count(species)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_sp2,
                          by = 'point_11_1_km')
    }

    # point_1_1_km
    {
      point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_1_1_km, Ctrl_gbifID
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_1_1_km, Ctrl_gbifID
                        ORDER BY point_1_1_km, Ctrl_gbifID")

      point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_1_1_km
                        FROM point_hot_oc
                        GROUP BY point_1_1_km
                        ORDER BY count(Ctrl_gbifID) DESC") %>%
        dplyr::rename(n_unique_collection_event_1_1_km=`count(Ctrl_gbifID)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_oc2,
                          by = 'point_1_1_km')


      point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_1_1_km, species
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_1_1_km, species
                        ORDER BY point_1_1_km, species")

      point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_1_1_km
                        FROM point_hot_sp
                        GROUP BY point_1_1_km
                        ORDER BY count(species) DESC") %>%
        dplyr::rename(n_taxon_name_1_1_km=`count(species)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_sp2,
                          by = 'point_1_1_km')
    }

    # point_110m
    {
      point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_110m, Ctrl_gbifID
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_110m, Ctrl_gbifID
                        ORDER BY point_110m, Ctrl_gbifID")

      point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_110m
                        FROM point_hot_oc
                        GROUP BY point_110m
                        ORDER BY count(Ctrl_gbifID) DESC") %>%
        dplyr::rename(n_unique_collection_event_110m=`count(Ctrl_gbifID)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_oc2,
                          by = 'point_110m')


      point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_110m, species
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_110m, species
                        ORDER BY point_110m, species")

      point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_110m
                        FROM point_hot_sp
                        GROUP BY point_110m
                        ORDER BY count(species) DESC") %>%
        dplyr::rename(n_taxon_name_110m=`count(species)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_sp2,
                          by = 'point_110m')
    }

    # point_11m
    {
      point_hot_oc <- sqldf::sqldf("SELECT DISTINCT point_11m, Ctrl_gbifID
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_11m, Ctrl_gbifID
                        ORDER BY point_11m, Ctrl_gbifID")

      point_hot_oc2 <- sqldf::sqldf("SELECT DISTINCT count(Ctrl_gbifID), point_11m
                        FROM point_hot_oc
                        GROUP BY point_11m
                        ORDER BY count(Ctrl_gbifID) DESC") %>%
        dplyr::rename(n_unique_collection_event_11m=`count(Ctrl_gbifID)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_oc2,
                          by = 'point_11m')


      point_hot_sp <- sqldf::sqldf("SELECT DISTINCT point_11m, species
                        FROM occ_cc
                        WHERE parseGBIF_dataset_result = 'useable'
                        GROUP BY point_11m, species
                        ORDER BY point_11m, species")

      point_hot_sp2 <- sqldf::sqldf("SELECT DISTINCT count(species), point_11m
                        FROM point_hot_sp
                        GROUP BY point_11m
                        ORDER BY count(species) DESC") %>%
        dplyr::rename(n_taxon_name_11m=`count(species)`)

      occ_cc <- left_join(occ_cc,
                          point_hot_sp2,
                          by = 'point_11m')
    }

    occ_cc <- left_join(occ_cc,
                        centroids %>%
                          dplyr::select(point_11m, level) %>%
                          dplyr::rename(parseGBIF_GADM_centroids_level =level),
                        by = 'point_11m')

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
    geo_issue_urb <- !(occ_cc$.cen==FALSE | occ_cc$.cap==FALSE | occ_cc$.urb==FALSE | occ_cc$.inst==FALSE | occ_cc$.con==FALSE | occ_cc$.sea==FALSE)

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
                                          parseGBIF_coordinate_status,
                                          .coordinates_outOfRange,
                                          .val,.zer,.sea,.equ,.cen,.cap,.urb,.con,.inst,.dup),
                 occ)
    occ$parseGBIF_useful_for_spatial_analysis <- geo_issue

    print(NROW(occ_cc))
    print(paste0('.coordinates_outOfRange', '-', occ_cc %>% dplyr::filter(.coordinates_outOfRange == FALSE) %>% NROW()))
    print(paste0('.val', '-', occ_cc %>% dplyr::filter(.val == FALSE) %>% NROW()))
    print(paste0('.zer', '-', occ_cc %>% dplyr::filter(.zer == FALSE) %>% NROW()))
    print(paste0('.cen', '-', occ_cc %>% dplyr::filter(.cen == FALSE) %>% NROW()))
    print(paste0('.equ', '-', occ_cc %>% dplyr::filter(.equ == FALSE) %>% NROW()))
    print(paste0('.cap', '-', occ_cc %>% dplyr::filter(.cap == FALSE) %>% NROW()))
    print(paste0('.urb', '-', occ_cc %>% dplyr::filter(.urb == FALSE) %>% NROW()))
    print(paste0('.con', '-', occ_cc %>% dplyr::filter(.con == FALSE) %>% NROW()))
    print(paste0('.inst', '-', occ_cc %>% dplyr::filter(.inst == FALSE) %>% NROW()))
    print(paste0('.dup', '-', occ_cc %>% dplyr::filter(.dup == FALSE) %>% NROW()))
    print(paste0('.sea', '-', occ_cc %>% dplyr::filter(.sea == FALSE) %>% NROW()))


    # parseGBIF_useful_for_spatial_analysis

  }

  return(occ)
}
#'
#' {
#'   #
#'   # v <- terra::vect(matrix(c(0, 0), nrow=1), crs="OGC:CRS84")
#'   #
#'   # library(raster)
#'   # slot(as(v, "Spatial"), "proj4string")
#'   # plot(v)
#'   #
#'   #
#'   }
#'
#' {
#' #'
#' #' {
#' #'
#' #' #   occ.pto.x <- sp::SpatialPointsDataFrame(cbind.data.frame(occ$parseGBIF_decimalLongitude[occ_cc_==FALSE],
#' #' #                                                            occ$parseGBIF_decimalLatitude[occ_cc_==FALSE]),
#' #' #                                         data.frame(occ[occ_cc_==FALSE,]))
#' #' #
#' #' #   occ.pto <- sp::SpatialPointsDataFrame(cbind.data.frame(occ$parseGBIF_decimalLongitude[occ_cc_==TRUE],
#' #' #                                                            occ$parseGBIF_decimalLatitude[occ_cc_==TRUE]),
#' #' #                                           data.frame(occ[occ_cc_==TRUE,]))
#' #' #
#' #' #
#' #' #
#' #' #   plot(wd[wd$iso_a3  %in% occ$parseGBIF_countryCode_ISO3,
#' #' #           c('iso_a3')])
#' #' #
#' #' #   points(occ.pto, col = "blue")
#' #' #   points(occ.pto.x, col = "red")
#' #' #
#' #' #
#' #' #   # points(occ.pto.x, col = "green")
#' #' #
#' #' #   plot(wd[wd$iso_a3  %in% occ_cc$parseGBIF_countryCode_ISO3[x==FALSE],
#' #' #           c('iso_a3')])
#' #' #
#' #' #   points(occ.pto.x, col = "red")
#' #' #
#' #' #
#' #' # NROW(occ.pto.x)
#' #' #
#' #' #   plot(world[world$GID_0 %in% c('BOL'),])
#' #' #
#' #' #
#' #' #
#' #' #   x <- occ_cc$.con
#' #' #
#' #' #   occ.pto <- sf::st_sfc(occ_cc$decimallongitude[occ_cc$.summary_CoordinateCleaner==TRUE],
#' #' #                         occ_cc$decimallatitude[occ_cc$.summary_CoordinateCleaner==TRUE])
#' #' #
#' #' #                         sf::st_crs( raster::crs("+proj=longlat +datum=WGS84 +no_defs")))
#' #' #
#' #' #
#' #' #   occ.pto.out <- sf::st_sfc(occ_cc$decimallongitude[occ_cc$.summary_CoordinateCleaner==FALSE],
#' #' #                         occ_cc$decimallatitude[occ_cc$.summary_CoordinateCleaner==FALSE],
#' #' #                         crs = sf::st_crs("+proj=longlat +datum=WGS84 +no_defs"))
#' #' #
#' #' #   occ.pto.out <- sp::SpatialPointsDataFrame(cbind.data.frame(occ_cc$decimallongitude[occ_cc$.summary_CoordinateCleaner==FALSE],
#' #' #                                                          occ_cc$decimallatitude[occ_cc$.summary_CoordinateCleaner==FALSE]),
#' #' #                                         data.frame(occ[occ_cc$.summary_CoordinateCleaner==FALSE,]))
#' #' #
#' #' #
#' #' #   occ.pto <- sp::SpatialPointsDataFrame(cbind.data.frame(occ_cc$decimallongitude[occ_cc$.summary_CoordinateCleaner==TRUE],
#' #' #                                                      occ_cc$decimallatitude[occ_cc$.summary_CoordinateCleaner==TRUE]),
#' #' #                                     data.frame(occ[occ_cc$.summary_CoordinateCleaner==TRUE,]))
#' #' #
#' #' #
#' #' #   proj4string(occ.pto) <- sf::st_transform(occ.pto,
#' #' #                                            sf::st_crs("+proj=longlat +datum=WGS84 +no_defs"))
#' #' #
#' #' #
#' #' #   x <- occ_cc$.summary_CoordinateCleaner == TRUE & occ_cc$.summary_CoordinateCleaner == TRUE
#' #' #
#' #' #   occ_cc$.coordinates_outOfRange
#' #' #
#' #' #   plo
#' #' #
#' #' #   points(occ.pto.out, col = "red")
#' #' #   points(occ.pto, col = "green")
#' #' #
#' #' #   ,
#' #' #   ,
#' #' #   ,
#' #' #   parseGBIF_useful_for_spatial_analysis,
#' #' #   parseGBIF_countryCode_ISO3
#' #' #
#' #' #   limpaCoordenadas = CoordinateCleaner::clean_coordinates(
#' #' #     x = occ,
#' #' #     species = "parseGBIF_sample_taxon_name",
#' #' #     lon = "parseGBIF_decimalLongitude",
#' #' #     lat = "parseGBIF_decimalLatitude",
#' #' #     tests = c("duplicates", #duplicatas
#' #' #               "equal", #coordenadas iguais
#' #' #               "seas", #pontos no mar
#' #' #               "zeros" #zeros e pontos em que lat = lon
#' #' #     )
#' #' #   )
#' #' #
#' #' #   summary(limpaCoordenadas)
#' #' #   table(filtroSpp_limpo$parseGBIF_sample_taxon_name)
#' #' #
#' #' #   filtroSpp_limpo = limpaCoordenadas %>% dplyr::filter(limpaCoordenadas$.summary==FALSE)
#' #' #   filtroSpp_limpo
#' #' #
#' #' #   table(filtroSpp_limpo$parseGBIF_sample_taxon_name)
#' #' #
#' #' #
#' #' #   # geodata_
#' #' #   geodata_ = filtroSpp_limpo %>% arrange(desc(parseGBIF_sample_taxon_name))
#' #' #
#' #' #
#' #' #   #define os limites para o mapa
#' #' #   xlim = range(geodata_$parseGBIF_decimalLongitude, na.rm=TRUE) + c(-6,6)
#' #' #   ylim = range(geodata_$parseGBIF_decimalLatitude, na.rm=TRUE) + c(-6,6)
#' #' #
#' #' #
#' #' #   library(maps)
#' #' #   #plota as camadas
#' #' #   # map("world", "Brazil", fill=T, col="grey90", xlim=xlim, ylim=ylim)
#' #' #   # map(,,add=T,xlim=xlim, ylim=ylim)
#' #' #   map(,,xlim=xlim, ylim=ylim)
#' #' #
#' #' #
#' #' #   #filtro contendo as coordenadas geográficas de cada sp
#' #' #   pto = filtroSpp_limpo[, c('parseGBIF_decimalLongitude', 'parseGBIF_decimalLatitude')] #3:2 para inverter a ordem para lon e lat
#' #' #   points(pto, pch=19, cex=1, col='red')
#' #' #   #plota alguns elementos de mapa
#' #' #   abline(h=0, lwd=0.8, lty=2, col="gray56") #adiciona uma linha para representar a do equador (opcional)
#' #' #   #grid(lty=2, lwd=0.8, col="gray56") #adiciona um grid (opcional) - caso opte pelo grid seria esteticamente melhor não executar o comando acima
#' #' #   prettymapr::addnortharrow("bottomleft", scale=0.7) #adiciona o símbolo do Norte
#' #' #   #GISTools::north.arrow(xb=-85, yb=-29,len=0.8, lab="N")  #outra opção de símbolo do Norte
#' #' #   maps::map.scale(x=-42,y=-28, ratio=FALSE, cex=0.8) #adiciona escala; em x e y você também pode alterar a posição
#' #' #   map.axes() #delimita a figura
#' #' #
#' #' #
#' #' #   {
#' #' #
#' #' #     listaSpp <- occ$parseGBIF_sample_taxon_name %>% unique() %>% as.charecter()
#' #' #     #plota os mapas de cada sp dentro de um loop
#' #' #     for(i in 1:length(listaSpp)){
#' #' #       map("world", "Brazil", fill=T, col="grey90", xlim=xlim, ylim=ylim)
#' #' #       title(nomeSpp_legenda[i], line=1) #optei por não plotar a legenda e sim o nome das spp no título de cada plot
#' #' #       map(,,add=T,xlim=xlim, ylim=ylim)
#' #' #       points(listaSpp[[i]], pch=19, cex=1, col=paletaCustomizada[i])
#' #' #       abline(h=0, lwd=0.8, lty=2, col="gray56")
#' #' #       prettymapr::addnortharrow("bottomleft", scale=0.5) #altere os argumentos que envolvem posição e tamanho conforme sua necessidade
#' #' #       maps::map.scale(x=-43,y=-25, ratio=FALSE, cex=0.6)
#' #' #       map.axes()
#' #' #     }
#' #' #
#' #' #   }
#' #' }
#' #'
#' #'
#' #'   occ <- occ %>%
#' #'     dplyr::filter((.summary_CoordinateCleaner==TRUE)) %>%
#' #'     dplyr::mutate(decimalLongitude = Ctrl_decimalLongitude,
#' #'                   decimalLatitude = Ctrl_decimalLatitude) %>%
#' #'     dplyr::mutate(point = paste0(decimalLongitude, ' _ ', decimalLatitude))
#' #'
#' #'
#' #'
#' #'   #' @details Pontos proxios de centroides
#' #'   {
#' #'     n_dec_round <- input$decimal_round_lat_long
#' #'
#' #'     occ_full <<- occ_full %>%
#' #'       dplyr::mutate(point_cent = paste0(round(decimalLatitude, n_dec_round), ' _ ', round(decimalLongitude,n_dec_round)))
#' #'
#' #'     hot_centroids <- left_join(occ_full,
#' #'                                # %>%
#' #'                                #   dplyr::mutate(point_cent = paste0(round(decimalLatitude, n_dec_round), ' _ ', round(decimalLongitude,n_dec_round))),
#' #'                                centroids %>%
#' #'                                  dplyr::mutate(point_cent = paste0(round(lon,n_dec_round), ' _ ', round(lat,n_dec_round))),
#' #'                                by = 'point_cent')
#' #'
#' #'
#' #'     point_to_check_hot_centroids <- sqldf::sqldf("SELECT DISTINCT point_cent, COUNT(Ctrl_scientificNameReference), decimalLongitude, decimalLatitude
#' #'                         FROM hot_centroids
#' #'                         GROUP BY point_cent
#' #'                         ORDER BY COUNT(Ctrl_scientificNameReference) DESC")
#' #'
#' #'     point_to_check_hot_centroids <- point_to_check_hot_centroids %>%
#' #'       dplyr::rename(n_occ=`COUNT(Ctrl_scientificNameReference)`)
#' #'
#' #'
#' #'     point_to_check_hot_centroids <- point_to_check_hot_centroids %>%
#' #'       dplyr::filter(n_occ>input$numero_registos_por_centroide)
#' #'
#' #'
#' #'     point_to_check_hot_centroids <- point_to_check_hot_centroids %>%
#' #'       dplyr::mutate(decimalLongitude = as.numeric(decimalLongitude),
#' #'                     decimalLatitude = as.numeric(decimalLatitude))
#' #'
#' #'     }
#' #'
#' #'
#' #'   #' @section coordenadas com mais de um registro
#' #'   {
#' #'     point_to_check <- sqldf::sqldf("SELECT DISTINCT point, COUNT(Ctrl_scientificNameReference), decimalLongitude, decimalLatitude
#' #'                         FROM occ_full
#' #'                         GROUP BY point
#' #'                         ORDER BY COUNT(Ctrl_scientificNameReference) DESC")
#' #'
#' #'     point_to_check <- point_to_check %>%
#' #'       dplyr::rename(n_occ=`COUNT(Ctrl_scientificNameReference)`)
#' #'
#' #'     point_to_check <- point_to_check %>%
#' #'       dplyr::mutate(decimalLongitude = as.numeric(decimalLongitude),
#' #'                     decimalLatitude = as.numeric(decimalLatitude))
#' #'
#' #'     point_to_check_moll <- point_to_check %>%
#' #'       sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
#' #'       sf::st_transform(crs = "+proj=moll")
#' #'
#' #'
#' #'     # # filter
#' #'     # index <- point_to_check$n_occ > 10
#' #'     #
#' #'     # point_to_check_r <- point_to_check[index==TRUE,]
#' #'     #
#' #'     # plot(point_to_check_r$x,
#' #'     #      col=point_to_check$n_occ)
#' #'     }
#' #'
#' #'
#' #'   #' @section duplicatatas digitais
#' #'   {
#' #'     occ_full$Ctrl_catalogNumber
#' #'
#' #'     occ_full$tombo <- paste0(occ_full$Ctrl_collectionCode,'_', occ_full$Ctrl_catalogNumber)
#' #'     # catalogNumber_list <- sqldf::sqldf("SELECT DISTINCT tombo, Ctrl_collectionCode,  Ctrl_catalogNumber, COUNT(Ctrl_catalogNumber) as n_occ
#' #'     catalogNumber_list <- sqldf::sqldf("SELECT DISTINCT tombo, COUNT(tombo) as n_occ
#' #'                                          FROM occ_full
#' #'                         GROUP BY tombo
#' #'                         ORDER BY n_occ DESC") %>%
#' #'       dplyr::filter(n_occ>1)
#' #'     # View(catalogNumber_list)
#' #'
#' #'   }
#' #'
#' #'   #' @section Preparar mapas
#' #'   {
#' #'     # open
#' #'     {
#' #'
#' #'       centroids <- get_centroids(path_centroids='c:\\R_temp')
#' #'
#' #'       # n_dec_round <- 1
#' #'       # hot_centroids <- left_join(occ_full %>%
#' #'       #                               dplyr::mutate(point_cent = paste0(round(decimalLatitude, n_dec_round), ' _ ', round(decimalLongitude,n_dec_round))),
#' #'       #                            centroids %>%
#' #'       #                               dplyr::mutate(point_cent = paste0(round(lon,n_dec_round), ' _ ', round(lat,n_dec_round))),
#' #'       #                            by = 'point_cent'
#' #'       #                            )
#' #'       #
#' #'       # NROW(hot_centroids)
#' #'
#' #'
#' #'     }
#' #'
#' #'     # mapas base leaflet
#' #'     {
#' #'       iconSet <- awesomeIconList(
#' #'         `Vértice EOO Dentro UC` = makeAwesomeIcon(
#' #'           icon = 'whatever',
#' #'           library = 'ion',
#' #'           iconColor = 'black',
#' #'           markerColor = 'darkblue',
#' #'           iconRotate = 30
#' #'         )
#' #'
#' #'       )
#' #'
#' #'       #
#' #'       # pal <- colorNumeric(legenda$cor, domain=NULL)
#' #'
#' #'       m_map_on <- leaflet( width = '1200px', height = '1200px') %>%
#' #'         addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
#' #'         addProviderTiles("Esri.WorldImagery", group = 'WorldImagery') %>%
#' #'         addProviderTiles("Stamen.Terrain", group = 'Terrain') %>%
#' #'         addLayersControl(
#' #'           # overlayGroups = c("Vértice EOO", "Validados", "Invalidados"),
#' #'           baseGroups = c("Mapnik", "WorldImagery", "Terrain"))
#' #'       # %>%
#' #'       #    addLegendAwesomeIcon(iconSet = iconSet,
#' #'       #                         orientation = 'horizontal',
#' #'       #                         title = NULL,
#' #'       #                         labelStyle = 'font-size: 10px;',
#' #'       #                         # position = 'bottomright',
#' #'       #                         group = 'Legenda')
#' #'
#' #'
#' #'     }
#' #'
#' #'     # grid
#' #'     {
#' #'       # world
#' #'       wd <- rnaturalearth::ne_countries(returnclass = "sf")
#' #'
#' #'       wd_proj_moll <- sf::st_transform(wd, crs = "+proj=moll")
#' #'
#' #'       # https://proj.org/operations/projections/cea.html
#' #'       wd_proj_cea <- sf::st_transform(wd, crs = "+proj=cea")
#' #'
#' #'     }
#' #'
#' #'   }
#' #'
#' #'   cellsize <<- 50000
#' #'   # grid
#' #'   {
#' #'
#' #'     # cellsize <- get_resoluction()
#' #'
#' #'
#' #'     # grid
#' #'     {
#' #'
#' #'       grid_moll <- wd_proj_moll %>%
#' #'         sf::st_make_grid(cellsize = cellsize) %>%
#' #'         sf::st_as_sf() %>%
#' #'         tibble::rowid_to_column() %>%
#' #'         dplyr::mutate(area = sf::st_area(.))
#' #'
#' #'
#' #'       grid_cea <- wd_proj_cea %>%
#' #'         sf::st_make_grid(cellsize = cellsize) %>%
#' #'         sf::st_as_sf() %>%
#' #'         tibble::rowid_to_column() %>%
#' #'         dplyr::mutate(area = sf::st_area(.))
#' #'
#' #'
#' #'
#' #'     }
#' #'
#' #'     # ocorrências
#' #'     {
#' #'       data_tmp <- occ_full %>%
#' #'         dplyr::filter(.summary_CoordinateCleaner==TRUE) %>%
#' #'         dplyr::mutate(species = Ctrl_scientificNameReference,
#' #'                       decimalLongitude = Ctrl_decimalLongitude,
#' #'                       decimalLatitude = Ctrl_decimalLatitude,
#' #'                       point = paste0(Ctrl_decimalLongitude, ' _ ',Ctrl_decimalLatitude)) %>%
#' #'         dplyr::select(species, decimalLongitude, decimalLatitude, point) %>%
#' #'         as.data.frame()
#' #'
#' #'       # EPSG:4326 - World Geodetic System 1984, used in GPS
#' #'       # https://epsg.io/4326
#' #'
#' #'       # Mollweide World Mollweide - ESRI:54009
#' #'       # https://proj.org/operations/projections/moll.html
#' #'
#' #'       occ_proj_moll <- data_tmp %>%
#' #'         sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
#' #'         sf::st_transform(crs = "+proj=moll")
#' #'
#' #'
#' #'       # Lambert Cylindrical Equal Area (Spherical) - EPSG:9834
#' #'       # https://proj.org/operations/projections/cea.html
#' #'       occ_proj_cea <- data_tmp %>%
#' #'         sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
#' #'         sf::st_transform(crs = "+proj=cea")
#' #'
#' #'
#' #'     }
#' #'
#' #'     #  cruzamento de dados
#' #'     {
#' #'       # moll
#' #'       grid_spp_moll <- sf::st_join(x = grid_moll,
#' #'                                    y = occ_proj_moll,
#' #'                                    left = TRUE)
#' #'
#' #'       grid_spp_tb_moll <- grid_spp_moll %>%
#' #'         tibble::as_tibble() %>%
#' #'         dplyr::group_by(rowid) %>%
#' #'         dplyr::summarise(ocorrencias = length(species[!is.na(species)]),
#' #'                          riqueza = n_distinct(species, na.rm = TRUE))
#' #'
#' #'       grid_spp_moll <- left_join(grid_spp_moll, grid_spp_tb_moll, by = 'rowid')
#' #'
#' #'
#' #'       # cea
#' #'       grid_spp_cea <- sf::st_join(x = grid_cea,
#' #'                                   y = occ_proj_cea,
#' #'                                   left = TRUE)
#' #'
#' #'       grid_spp_tb_cea <- grid_spp_cea %>%
#' #'         tibble::as_tibble() %>%
#' #'         dplyr::group_by(rowid) %>%
#' #'         dplyr::summarise(ocorrencias = length(species[!is.na(species)]),
#' #'                          riqueza = n_distinct(species, na.rm = TRUE))
#' #'
#' #'       grid_spp_cea <- left_join(grid_spp_cea, grid_spp_tb_cea, by = 'rowid')
#' #'
#' #'
#' #'     }
#' #'
#' #'   }
#' #'
#' #'
#' #'
#' }
