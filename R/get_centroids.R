#' @title Load or generate table with centroids
#' @name get_centroids
#'
#' @description load or generate table with centroids for levels 0, 1 and 2,
#' countries, states and municipalities, in the world
#'
#' @param path_centroids path to the centroids file, default 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataRaw'
#' if not provided (NA), the table will be generated and loaded only into memory.
#'
#' @details Returns the table with centroids
#'
#' @return Table with centroids for levels 0, 1 and 2
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
#' centroids <- get_centroids(path_centroids=NA)
#'
#' colnames(centroids)
#' head(centroids)
#' }
#'
#' @import raster
#' @import geosphere
#' @import dplyr
#' @import geodata
#' @import countrycode
#' @import readr
#' @export
get_centroids <- function(path_centroids='https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataRaw')
{
  # library(raster)
  # library(geosphere)
  # library(dplyr)
  # library(readr)
  # library(geodata)
  # library(countrycode)


  # Load libraries
  # library(GADMTools)
  # library('raster')
  # library('geosphere')
  # library('mapview') # incredible interactive map visualization in R

  centroids <- {}

  if (!is.na(path_centroids))
  {
    file_centroids <- paste0(path_centroids,'/','parseGBIF_GADM_centroids.CSV')
    centroids <- readr::read_csv(file_centroids,
                                 locale = locale(encoding = "UTF-8"),
                                 show_col_types = FALSE)
  }
  # else
  # {
  #     data(centroids)
  # }

  if (is.null(centroids))
  {

    wd <- rnaturalearth::ne_countries(returnclass = "sv")

    countryCode_ISO3_wd <- wd$iso_a3 %>% unique()
    countrycode_table <- countrycode::codelist
    countryCode_ISO3 <- countrycode_table$iso3c %>% na.omit() %>% unique()

    ind <- countryCode_ISO3 %in% countryCode_ISO3_wd


    countryCode_ISO3 <- countryCode_ISO3[ind==FALSE]

    # Get SpatialPolygonsDataFrame object example

    centroids <- data.frame(countryCode_ISO3=NA,
                            # name0=NA,
                            # name1=NA,
                            # name2=NA,
                            level=NA,
                            lon=NA,
                            lat=NA)[-1,]

    i=1
    for (i in 1:NROW(countryCode_ISO3))
    {
      polygons <- geodata::gadm(country = countryCode_ISO3[i], path = path_centroids ,level = 0)
      polygons <- as(polygons, "Spatial")

      # centroids_tmp <- as.data.frame(geosphere::centroid(polygons))
      # centroids_tmp <- sf::st_centroid(sf::st_as_sf(polygons), of_largest_polygon=TRUE)
      centroids_tmp <- sf::st_centroid(sf::st_as_sf(polygons), of_largest_polygon=TRUE)

      centroids_tmp  %>%  as_tibble()

      # library(tidyverse)

      centroids_tmp <- data.frame(lon = unlist(purrr::map(centroids_tmp$geometry,1)),
                                    lat = unlist(purrr::map(centroids_tmp$geometry,2)))

      colnames(centroids_tmp) <- c("lon", "lat")

      # plot(polygons)
      # terra::points(nz_centroid, pch=1, col='red')
      # terra::points(seine_centroid, pch=1, col='blue')
      # terra::points(centroids_tmp, pch=1, col='red')

      centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
                                               # # name0=polygons$NAME_0,
                                               # name1=rep(NA, NROW(centroids_tmp)),
                                               # name2=rep(NA, NROW(centroids_tmp)),
                                               level=rep(0, NROW(centroids_tmp)),
                                               lon=centroids_tmp$lon,
                                               lat=centroids_tmp$lat))

      print(countryCode_ISO3[i])
    }

    i=1
    for (i in 1:NROW(countryCode_ISO3))
    {
      polygons <- NULL
      try({polygons <- geodata::gadm(country = countryCode_ISO3[i], path = path_centroids ,level = 1);
      polygons <- as(polygons, "Spatial")})
      if(is.null(polygons)){next}


      # Get polygons centroids
      centroids_tmp <- as.data.frame(centroid(polygons))
      colnames(centroids_tmp) <- c("lon", "lat")

      centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
                                               # name0=polygons$NAME_0,
                                               # name1=polygons$NAME_1,
                                               # name2=rep(NA, NROW(centroids_tmp)),
                                               level=rep(1, NROW(centroids_tmp)),
                                               lon=centroids_tmp$lon,
                                               lat=centroids_tmp$lat))

      print(countryCode_ISO3[i])
    }

    i=1
    for (i in 1:NROW(countryCode_ISO3))
    {
      polygons <- NULL
      try({polygons <- geodata::gadm(country = countryCode_ISO3[i], path = path_centroids ,level = 1);
      polygons <- as(polygons, "Spatial")})
      if(is.null(polygons)){next}


      # Get polygons centroids
      centroids_tmp <- as.data.frame(centroid(polygons))
      colnames(centroids_tmp) <- c("lon", "lat")

      centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
                                               # name0=polygons$NAME_0,
                                               # name1=polygons$NAME_1,
                                               # name2=polygons$NAME_2,
                                               level=rep(2, NROW(centroids_tmp)),
                                               lon=centroids_tmp$lon,
                                               lat=centroids_tmp$lat))

      print(countryCode_ISO3[i])
    }


    if (!is.na(path_centroids))
    {
      if ((substr(path_centroids,1,5) !='https')&(substr(path_centroids,2,2) ==':'))
      {
        if(!file.exists(file_centroids))
        {
          write.csv(centroids, file_centroids, fileEncoding = "UTF-8", na = "", row.names = FALSE)
          print(file_centroids)
        }
      }
    }


  }

  return(centroids)
}

