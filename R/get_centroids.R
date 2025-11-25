#' @title Load or Generate Geographic Centroids Table
#' @name get_centroids
#'
#' @description
#' Loads or generates a table with geographic centroids for administrative levels
#' 0 (countries), 1 (states/provinces), and 2 (municipalities) worldwide.
#' Uses GADM data to calculate centroids for political boundaries.
#'
#' @param file_centroids
#' Character. Path to centroids CSV file. Default uses the parseGBIF GitHub repository.
#' If `NA`, generates centroids table in memory only.
#'
#' @details
#' ## Data Sources:
#' - Uses GADM (Global Administrative Areas) database for boundary data
#' - Calculates centroids using largest polygon method
#' - Supports countries worldwide with ISO3 country codes
#'
#' ## Processing:
#' 1. Loads existing centroids file if available
#' 2. If not available, generates centroids from GADM data
#' 3. Calculates centroids for countries, states, and municipalities
#' 4. Saves to file if path provided and file doesn't exist
#'
#' @return
#' A data frame with centroids containing:
#' - `countryCode_ISO3`: ISO3 country code
#' - `level`: Administrative level (0=country, 1=state, 2=municipality)
#' - `lon`: Longitude of centroid
#' - `lat`: Latitude of centroid
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`prepare_gbif_occurrence_data()`] for preparing GBIF occurrence data,
#' [`standardize_country_from_iso2()`] for country code standardization
#'
#' @examples
#' \donttest{
#' # Load centroids table
#' centroids <- get_centroids()
#'
#' # View structure
#' colnames(centroids)
#' head(centroids)
#'
#' # Filter by administrative level
#' country_centroids <- centroids[centroids$level == 0, ]
#' state_centroids <- centroids[centroids$level == 1, ]
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_csv locale
#' @importFrom rnaturalearth ne_countries
#' @importFrom countrycode codelist
#' @importFrom geodata gadm
#' @importFrom sf st_centroid st_as_sf
#' @importFrom purrr map
#' @importFrom stats na.omit
#' @importFrom geosphere centroid
#' @importFrom utils write.csv
#' @export
get_centroids <- function(file_centroids = 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataRaw/parseGBIF_GADM_centroids.CSV') {

  centroids <- data.frame()

  # CORREÇÃO: Usar URL correta do GitHub (raw.githubusercontent.com)
  if (!is.na(file_centroids)) {
    tryCatch({
      centroids <- readr::read_csv(
        file_centroids,
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE
      )
    }, error = function(e) {
      warning("Could not load centroids from file: ", e$message)
    })
  }

  # If centroids not loaded from file, generate them
  if (nrow(centroids) == 0) {

    # CORREÇÃO: Definir path_centroids que estava faltando
    path_centroids <- tempdir()

    # Get world countries data
    wd <- rnaturalearth::ne_countries(returnclass = "sf")
    countryCode_ISO3_wd <- unique(wd$iso_a3)

    countrycode_table <- countrycode::codelist
    countryCode_ISO3 <- stats::na.omit(countrycode_table$iso3c)

    # Filter countries available in world data
    ind <- countryCode_ISO3 %in% countryCode_ISO3_wd
    countryCode_ISO3 <- countryCode_ISO3[!ind]

    # Initialize results dataframe
    centroids <- data.frame(
      countryCode_ISO3 = character(),
      level = integer(),
      lon = numeric(),
      lat = numeric(),
      stringsAsFactors = FALSE
    )

    # Process level 0 (countries)
    for (i in seq_along(countryCode_ISO3)) {
      country_code <- countryCode_ISO3[i]

      tryCatch({
        polygons <- geodata::gadm(
          country = country_code,
          path = path_centroids,
          level = 0
        )

        polygons_sf <- sf::st_as_sf(polygons)
        centroids_tmp <- sf::st_centroid(polygons_sf, of_largest_polygon = TRUE)

        # Extract coordinates
        coords <- as.data.frame(sf::st_coordinates(centroids_tmp))
        colnames(coords) <- c("lon", "lat")

        centroids <- rbind(
          centroids,
          data.frame(
            countryCode_ISO3 = polygons$GID_0,
            level = 0,
            lon = coords$lon,
            lat = coords$lat,
            stringsAsFactors = FALSE
          )
        )

        if (interactive()) message("Processed country: ", country_code)

      }, error = function(e) {
        if (interactive()) message("Failed to process country: ", country_code, " - ", e$message)
      })
    }

    # Process level 1 (states/provinces) - CORREÇÃO: usar geosphere::centroid
    for (i in seq_along(countryCode_ISO3)) {
      country_code <- countryCode_ISO3[i]

      tryCatch({
        polygons <- geodata::gadm(
          country = country_code,
          path = path_centroids,
          level = 1
        )

        polygons_sp <- as(polygons, "Spatial")
        centroids_tmp <- as.data.frame(geosphere::centroid(polygons_sp))
        colnames(centroids_tmp) <- c("lon", "lat")

        centroids <- rbind(
          centroids,
          data.frame(
            countryCode_ISO3 = polygons$GID_0,
            level = 1,
            lon = centroids_tmp$lon,
            lat = centroids_tmp$lat,
            stringsAsFactors = FALSE
          )
        )

        if (interactive()) message("Processed states for: ", country_code)

      }, error = function(e) {
        if (interactive()) message("Failed to process states for: ", country_code)
      })
    }

    # Process level 2 (municipalities) - CORREÇÃO: nível 2, não 1
    for (i in seq_along(countryCode_ISO3)) {
      country_code <- countryCode_ISO3[i]

      tryCatch({
        polygons <- geodata::gadm(
          country = country_code,
          path = path_centroids,
          level = 2
        )

        polygons_sp <- as(polygons, "Spatial")
        centroids_tmp <- as.data.frame(geosphere::centroid(polygons_sp))
        colnames(centroids_tmp) <- c("lon", "lat")

        centroids <- rbind(
          centroids,
          data.frame(
            countryCode_ISO3 = polygons$GID_0,
            level = 2,
            lon = centroids_tmp$lon,
            lat = centroids_tmp$lat,
            stringsAsFactors = FALSE
          )
        )

        if (interactive()) message("Processed municipalities for: ", country_code)

      }, error = function(e) {
        if (interactive()) message("Failed to process municipalities for: ", country_code)
      })
    }

    # Save if local file path provided
    if (!is.na(file_centroids) && !grepl("^https?://", file_centroids)) {
      if (!file.exists(file_centroids)) {
        utils::write.csv(
          centroids,
          file_centroids,
          fileEncoding = "UTF-8",
          na = "",
          row.names = FALSE
        )
        if (interactive()) message("Saved centroids to: ", file_centroids)
      }
    }
  }

  return(centroids)
}
# get_centroids <- function(file_centroids='https://github.com/pablopains/parseGBIF/blob/main/dataRaw/parseGBIF_GADM_centroids.CSV')
# {
#   # library(raster)
#   # library(geosphere)
#   # library(dplyr)
#   # library(readr)
#   # library(geodata)
#   # library(countrycode)
#
#
#   # Load libraries
#   # library(GADMTools)
#   # library('raster')
#   # library('geosphere')
#   # library('mapview') # incredible interactive map visualization in R
#
#   centroids <- {}
#
#   if (!is.na(file_centroids))
#   {
#     centroids <- readr::read_csv(file_centroids,
#                                  locale = locale(encoding = "UTF-8"),
#                                  show_col_types = FALSE)
#   }
#   # else
#   # {
#   #     data(centroids)
#   # }
#
#   if (is.null(centroids))
#   {
#
#     wd <- rnaturalearth::ne_countries(returnclass = "sv")
#
#     countryCode_ISO3_wd <- wd$iso_a3 %>% unique()
#     countrycode_table <- countrycode::codelist
#     countryCode_ISO3 <- countrycode_table$iso3c %>% na.omit() %>% unique()
#
#     ind <- countryCode_ISO3 %in% countryCode_ISO3_wd
#
#
#     countryCode_ISO3 <- countryCode_ISO3[ind==FALSE]
#
#     # Get SpatialPolygonsDataFrame object example
#
#     centroids <- data.frame(countryCode_ISO3=NA,
#                             # name0=NA,
#                             # name1=NA,
#                             # name2=NA,
#                             level=NA,
#                             lon=NA,
#                             lat=NA)[-1,]
#
#     i=1
#     for (i in 1:NROW(countryCode_ISO3))
#     {
#       polygons <- geodata::gadm(country = countryCode_ISO3[i], path = path_centroids ,level = 0)
#       polygons <- as(polygons, "Spatial")
#
#       # centroids_tmp <- as.data.frame(geosphere::centroid(polygons))
#       # centroids_tmp <- sf::st_centroid(sf::st_as_sf(polygons), of_largest_polygon=TRUE)
#       centroids_tmp <- sf::st_centroid(sf::st_as_sf(polygons), of_largest_polygon=TRUE)
#
#       centroids_tmp  %>%  as_tibble()
#
#       # library(tidyverse)
#
#       centroids_tmp <- data.frame(lon = unlist(purrr::map(centroids_tmp$geometry,1)),
#                                     lat = unlist(purrr::map(centroids_tmp$geometry,2)))
#
#       colnames(centroids_tmp) <- c("lon", "lat")
#
#       # plot(polygons)
#       # terra::points(nz_centroid, pch=1, col='red')
#       # terra::points(seine_centroid, pch=1, col='blue')
#       # terra::points(centroids_tmp, pch=1, col='red')
#
#       centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
#                                                # # name0=polygons$NAME_0,
#                                                # name1=rep(NA, NROW(centroids_tmp)),
#                                                # name2=rep(NA, NROW(centroids_tmp)),
#                                                level=rep(0, NROW(centroids_tmp)),
#                                                lon=centroids_tmp$lon,
#                                                lat=centroids_tmp$lat))
#
#       print(countryCode_ISO3[i])
#     }
#
#     i=1
#     for (i in 1:NROW(countryCode_ISO3))
#     {
#       polygons <- NULL
#       try({polygons <- geodata::gadm(country = countryCode_ISO3[i], path = path_centroids ,level = 1);
#       polygons <- as(polygons, "Spatial")})
#       if(is.null(polygons)){next}
#
#
#       # Get polygons centroids
#       centroids_tmp <- as.data.frame(centroid(polygons))
#       colnames(centroids_tmp) <- c("lon", "lat")
#
#       centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
#                                                # name0=polygons$NAME_0,
#                                                # name1=polygons$NAME_1,
#                                                # name2=rep(NA, NROW(centroids_tmp)),
#                                                level=rep(1, NROW(centroids_tmp)),
#                                                lon=centroids_tmp$lon,
#                                                lat=centroids_tmp$lat))
#
#       print(countryCode_ISO3[i])
#     }
#
#     i=1
#     for (i in 1:NROW(countryCode_ISO3))
#     {
#       polygons <- NULL
#       try({polygons <- geodata::gadm(country = countryCode_ISO3[i], path = path_centroids ,level = 1);
#       polygons <- as(polygons, "Spatial")})
#       if(is.null(polygons)){next}
#
#
#       # Get polygons centroids
#       centroids_tmp <- as.data.frame(centroid(polygons))
#       colnames(centroids_tmp) <- c("lon", "lat")
#
#       centroids <- rbind(centroids, data.frame(countryCode_ISO3=polygons$GID_0,
#                                                # name0=polygons$NAME_0,
#                                                # name1=polygons$NAME_1,
#                                                # name2=polygons$NAME_2,
#                                                level=rep(2, NROW(centroids_tmp)),
#                                                lon=centroids_tmp$lon,
#                                                lat=centroids_tmp$lat))
#
#       print(countryCode_ISO3[i])
#     }
#
#
#     if (!is.na(path_centroids))
#     {
#       if ((substr(path_centroids,1,5) !='https')&(substr(path_centroids,2,2) ==':'))
#       {
#         if(!file.exists(file_centroids))
#         {
#           write.csv(centroids, file_centroids, fileEncoding = "UTF-8", na = "", row.names = FALSE)
#           print(file_centroids)
#         }
#       }
#     }
#
#
#   }
#
#   return(centroids)
# }

