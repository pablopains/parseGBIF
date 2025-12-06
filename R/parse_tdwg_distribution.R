#' @title Parse TDWG Distribution for Occurrence Points
#' @name parse_tdwg_distribution
#'
#' @description
#' Identifies which occurrence points fall within the native range of a species
#' based on TDWG (World Geographical Scheme for Recording Plant Distributions) regions.
#' Performs spatial analysis to match occurrence coordinates with native range polygons.
#'
#' @param points
#' Data frame. Occurrence records with coordinate columns:
#' `parseGBIF_decimalLongitude` and `parseGBIF_decimalLatitude`.
#'
#' @param native_range
#' Data frame. Native range information with TDWG level 3 codes in column `LEVEL3_COD`.
#'
#' @param range_polygons
#' sf object. TDWG polygon data with TDWG level 3 codes in column `LEVEL3_COD`.
#'
#' @details
#' ## Processing Steps:
#' 1. Validates input parameters and coordinate data
#' 2. Converts occurrence points to sf spatial object with WGS84 CRS
#' 3. Filters TDWG polygons to include only the native range regions
#' 4. Ensures CRS compatibility between points and polygons
#' 5. Performs spatial join to identify points within native range
#' 6. Returns original data with native range membership indicator
#'
#' ## Error Handling:
#' - Validates coordinate data and excludes invalid coordinates
#' - Checks for required columns in all input data
#' - Handles geometry validation and repair automatically
#' - Provides informative warnings and progress messages
#'
#' @return
#' The input data frame `points` with an additional column:
#' - `native_range`: TDWG level 3 code for points within native range,
#'   `NA` for points outside native range
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`get_tdwg_distribution()`] for retrieving TDWG distribution data from POWO,
#' [`parse_coordinates()`] for coordinate validation and quality assessment
#'
#' @examples
#' \donttest{
#' # Example with simulated data
#' # Create sample occurrence points
#' points <- data.frame(
#'   parseGBIF_decimalLongitude = c(-47.9, -46.6, -45.0),
#'   parseGBIF_decimalLatitude = c(-15.8, -23.5, -22.9),
#'   species = c("Species_A", "Species_B", "Species_C")
#' )
#'
#' # Create sample native range data
#' native_range <- data.frame(LEVEL3_COD = c("BZL", "BZS"))
#'
#' # Note: range_polygons would typically be loaded from TDWG shapefile
#' # For this example, we assume range_polygons is available
#'
#' # Parse TDWG distribution (commented out as range_polygons is required)
#' # result <- parse_tdwg_distribution(
#' #   points = points,
#' #   native_range = native_range,
#' #   range_polygons = range_polygons
#' # )
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

  # message("Processing ", nrow(points), " occurrence points...")

  # Convert points to sf object with WGS84 CRS
  point_sf <- sf::st_as_sf(
    points,
    coords = c("parseGBIF_decimalLongitude", "parseGBIF_decimalLatitude"),
    crs = 4326,  # WGS84
    remove = FALSE
  )

  # Filter native range TDWG polygons
  # message("Filtering native range polygons...")
  native_tdwg <- range_polygons %>%
    dplyr::filter(LEVEL3_COD %in% native_range$LEVEL3_COD) %>%
    dplyr::select(LEVEL3_COD)

  if (nrow(native_tdwg) == 0) {
    warning("No native range polygons found matching the provided LEVEL3_COD values")
    points$native_range <- NA_character_
    return(points)
  }

  # Ensure CRS compatibility
  # message("Ensuring CRS compatibility...")
  native_tdwg <- sf::st_transform(native_tdwg, crs = sf::st_crs(point_sf))

  # Validate and repair geometries if necessary
  invalid_geoms <- sum(!sf::st_is_valid(native_tdwg))
  if (invalid_geoms > 0) {
    # message("Repairing ", invalid_geoms, " invalid geometries...")
    native_tdwg <- sf::st_make_valid(native_tdwg)
  }

  # Perform spatial join
  # message("Performing spatial join...")
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

