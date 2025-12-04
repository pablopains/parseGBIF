#' @title Parse TDWG distribution for occurrence points
#' @name parse_tdwg_distribution
#'
#' @description Identifies which occurrence points fall within the native
#' range of a species based on TDWG (World Geographical Scheme for
#' Recording Plant Distributions) regions.
#'
#' @param points A data frame containing occurrence records with columns
#' `parseGBIF_decimalLongitude` and `parseGBIF_decimalLatitude`.
#' @param native_range A data frame containing the native range information
#' with column `LEVEL3_COD` for TDWG level 3 codes.
#' @param range_polygons An sf object containing TDWG polygon data with
#' column `LEVEL3_COD` for TDWG level 3 codes.
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item{Converts occurrence points to an sf spatial object}
#'   \item{Filters TDWG polygons to include only the native range}
#'   \item{Performs spatial join to identify points within native range}
#'   \item{Returns the original data with an additional column indicating
#'         native range membership}
#' }
#'
#' The function handles CRS alignment and geometry validation automatically.
#'
#' @return
#' The input data frame `points` with an additional column:
#' \item{native_range}{TDWG level 3 code for points within native range,
#' NA for points outside native range}
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' \code{\link[parseGBIF]{preprocess_data}},
#' \code{\link[parseGBIF]{prepare_collectionCode}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' data(example_occurrences)
#' data(tdwg_native_range)
#' data(tdwg_polygons)
#'
#' # Parse TDWG distribution
#' result <- parse_tdwg_distribution(
#'   points = example_occurrences,
#'   native_range = tdwg_native_range,
#'   range_polygons = tdwg_polygons
#' )
#'
#' # View results
#' head(result[, c("parseGBIF_decimalLongitude", "parseGBIF_decimalLatitude",
#'                "native_range")])
#'
#' # Count points by native range
#' table(result$native_range, useNA = "always")
#' }
#'
#' @importFrom sf st_as_sf st_crs st_transform st_make_valid st_join st_is_valid
#' @importFrom dplyr filter select rename
#' @importFrom tibble as_tibble
#' @export
parse_tdwg_distribution <- function(points, native_range, range_polygons) {

  # Validate input parameters
  if (missing(points) || !is.data.frame(points)) {
    stop("Parameter 'points' must be a data frame")
  }

  if (missing(native_range) || !is.data.frame(native_range)) {
    stop("Parameter 'native_range' must be a data frame")
  }

  if (missing(range_polygons) || !inherits(range_polygons, "sf")) {
    stop("Parameter 'range_polygons' must be an sf object")
  }

  # Check required columns in points
  required_point_cols <- c("parseGBIF_decimalLongitude", "parseGBIF_decimalLatitude")
  if (!all(required_point_cols %in% colnames(points))) {
    stop("Points data must contain columns: ",
         paste(required_point_cols, collapse = ", "))
  }

  # Check required columns in native_range
  if (!"LEVEL3_COD" %in% colnames(native_range)) {
    stop("Native range data must contain 'LEVEL3_COD' column")
  }

  # Check required columns in range_polygons
  if (!"LEVEL3_COD" %in% colnames(range_polygons)) {
    stop("Range polygons must contain 'LEVEL3_COD' column")
  }

  # Check for valid coordinates
  invalid_coords <- points[!is.finite(points$parseGBIF_decimalLongitude) |
                             !is.finite(points$parseGBIF_decimalLatitude), ]
  if (nrow(invalid_coords) > 0) {
    warning("Found ", nrow(invalid_coords),
            " records with invalid coordinates. These will be excluded.")
    points <- points[is.finite(points$parseGBIF_decimalLongitude) &
                       is.finite(points$parseGBIF_decimalLatitude), ]
  }

  if (nrow(points) == 0) {
    stop("No valid coordinates found in points data")
  }

  message("Processing ", nrow(points), " occurrence points...")

  # Convert points to sf object with WGS84 CRS
  point_sf <- sf::st_as_sf(
    points,
    coords = c("parseGBIF_decimalLongitude", "parseGBIF_decimalLatitude"),
    crs = 4326,  # WGS84
    remove = FALSE
  )

  # Filter native range TDWG polygons
  message("Filtering native range polygons...")
  native_tdwg <- range_polygons %>%
    dplyr::filter(LEVEL3_COD %in% native_range$LEVEL3_COD) %>%
    dplyr::select(LEVEL3_COD)

  if (nrow(native_tdwg) == 0) {
    warning("No native range polygons found matching the provided LEVEL3_COD values")
    points$native_range <- NA_character_
    return(points)
  }

  # Ensure CRS compatibility
  message("Ensuring CRS compatibility...")
  native_tdwg <- sf::st_transform(native_tdwg, crs = sf::st_crs(point_sf))

  # Validate and repair geometries if necessary
  invalid_geoms <- sum(!sf::st_is_valid(native_tdwg))
  if (invalid_geoms > 0) {
    message("Repairing ", invalid_geoms, " invalid geometries...")
    native_tdwg <- sf::st_make_valid(native_tdwg)
  }

  # Perform spatial join
  message("Performing spatial join...")
  native_points <- sf::st_join(point_sf, native_tdwg)

  # Rename the output column
  native_points <- native_points %>%
    dplyr::rename(native_range = LEVEL3_COD)

  # Convert back to tibble and remove geometry
  result <- native_points %>%
    tibble::as_tibble() %>%
    dplyr::select(-geometry)

  # Provide summary statistics
  native_count <- sum(!is.na(result$native_range))
  total_count <- nrow(result)

  message("Spatial analysis completed: ",
          native_count, " of ", total_count,
          " points (", round(native_count/total_count * 100, 1),
          "%) within native range")

  return(result)
}


# parse_tdwg_distribution <- function(points, native_range, range_polygons){
#   # point_sf <- sf::st_as_sf(points,
#   #                          coords=c("parseGBIF_decimalLongitude", "parseGBIF_decimalLatitude"),
#   #                          crs=sf::st_crs(range_polygons),
#   #                          remove=FALSE)
#   # # get shapes of native range
#   # native_tdwg <- dplyr::filter(range_polygons, LEVEL3_COD %in% native_range$LEVEL3_COD)
#   # native_tdwg <- dplyr::select(native_tdwg, LEVEL3_COD)
#   # # clip points to native range with a spatial join
#   # native_points <- sf::st_join(point_sf, native_tdwg)
#   # native_points <- dplyr::rename(native_points, native_range=LEVEL3_COD)
#   # # convert back to normal data frame from sf
#   # native_points <- tibble::as_tibble(native_points)
#   # native_points <- dplyr::select(native_points, -geometry)
#   # native_points
#
#   # Garantir que range_polygons tenha um CRS atualizado
#   # range_polygons <- sf::st_transform(range_polygons, crs = 4326)
#
#   # Criar o objeto sf para os pontos, garantindo que o CRS seja WGS84 (EPSG:4326)
#   point_sf <- sf::st_as_sf(points,
#                        coords = c("parseGBIF_decimalLongitude", "parseGBIF_decimalLatitude"),
#                        crs = 4326,  # Define explicitamente o CRS
#                        remove = FALSE)
#
#   # Filtrar a regi?o nativa no shapefile TDWG
#   native_tdwg <- dplyr::filter(range_polygons, LEVEL3_COD %in% native_range$LEVEL3_COD)
#   native_tdwg <- dplyr::select(native_tdwg, LEVEL3_COD)
#
#   # Garantir que os CRS dos objetos sejam compat?veis antes do join
#   native_tdwg <- sf::st_transform(native_tdwg, crs = sf::st_crs(point_sf))
#
#   native_tdwg <- sf::st_make_valid(native_tdwg)
#
#   # # Verificar se h? geometrias inv?lidas em native_tdwg
#   # if(sum(!sf::st_is_valid(native_tdwg))>0)  # Conta quantas geometrias est?o inv?lidas
#   # {
#   #   # Corrigir geometrias inv?lidas
#   #   native_tdwg <- sf::st_make_valid(native_tdwg)  # Corrige automaticamente
#   #
#   #   if(sum(!sf::st_is_valid(native_tdwg))>0)  # Conta quantas geometrias est?o inv?lidas
#   #   {
#   #     # Alternativa: Remover geometrias inv?lidas, caso n?o sejam corrig?veis
#   #     native_tdwg <- native_tdwg[sf::st_is_valid(native_tdwg), ]
#   #   }
#   # }
#
#
#   # Realizar a interse??o espacial entre os pontos e as regi?es TDWG
#   native_points <- sf::st_join(point_sf, native_tdwg)
#
#   # Renomear a coluna de sa?da
#   native_points <- dplyr::rename(native_points, native_range = LEVEL3_COD)
#
#   # Converter para tibble e remover a geometria
#   native_points <- tibble::as_tibble(native_points)
#   native_points <- dplyr::select(native_points, -geometry)
#
#   # # Verificar os CRS para garantir compatibilidade
#   # print(st_crs(native_tdwg))
#   # print(st_crs(point_sf))
#
#   # Retornar os pontos corrigidos
#   native_points
# }
