#' @title Check Occurrence Distribution against TDWG Native Range
#'
#' @name check_tdwg_distribution
#'
#' @description Compares a set of occurrence records with the native geographical distribution,
#'              as defined by the WCVP database and TDWG Level 3 regions.
#'              This function iterates through unique taxa in the occurrence data,
#'              retrieves their native range, and checks which TDWG region each
#'              occurrence point falls into.
#'
#' @param occ A data frame containing occurrence data, likely including
#'            geographic coordinates (`lat`, `lon`), a column with the WCVP taxon
#'            name (`wcvp_taxon_name`), and a quality control column like
#'            `parseGBIF_dataset_result` (expected to have 'useable' values).
#' @param TDWG_LEVEL3 An `sf` object containing the TDWG Level 3 geographical
#'                    boundaries (polygons). If `NULL`, it is loaded from
#'                    `data/tdwg_level3.rds` using `here::here()`.
#' @param wcvp A list containing the WCVP data, typically the result of
#'             calling a function like `wcvp_get_data()`. It should contain at
#'             least the distribution data required by `get_tdwg_distribution()`.
#'
#' @details The function processes records for each unique taxon, using
#'          `get_tdwg_distribution()` to find the known native range and
#'          `parse_tdwg_distribution()` to perform the spatial comparison
#'          between the occurrence points and the native regions.
#'
#' @return A data frame, which is the result of binding the output of
#'         `parse_tdwg_distribution()` for all unique taxa processed. This
#'         data frame typically contains the original occurrence data with
#'         added columns indicating whether the point falls within the
#'         reported native range.
#'
#' @author Pablo Hendrigo Alves de Melo
#'
#' @seealso \code{\link[sf]{st_transform}}, \code{\link[here]{here}}
#'
#' @importFrom readr read_rds
#' @importFrom sf st_transform
#' @importFrom dplyr filter
#' @export
check_tdwg_distribution <- function(occ=NA,
                                    TDWG_LEVEL3=NULL,
                                    wcvp=NULL)
{
  if(is.null(TDWG_LEVEL3)){
    TDWG_LEVEL3 <- readr::read_rds(here::here("data/tdwg_level3.rds"))
    TDWG_LEVEL3 <- sf::st_transform(TDWG_LEVEL3, crs = 4326)
  }

  index <- occ$parseGBIF_dataset_result =='useable'


  nomes <- occ$wcvp_taxon_name[index==TRUE] %>% unique() %>% na.omit()
  i=13


  # native_range <- data.frame(native_range = rep(NA, NROW(occ$occ_voucher)),
  #                            wcvp_taxon_name = occ$occ_voucher$wcvp_taxon_name)

  tdwg_points_res <- {}

  # occ$occ_voucher <- dplyr::select(occ$occ_voucher, -native_range)
  i=2
  for(i in 1:NROW(nomes))
  {
    message(nomes[i])
    tdwg_dist <- get_tdwg_distribution(searchedName = nomes[i],
                                       wcvp_names = wcvp)

    index <- occ$parseGBIF_dataset_result =='useable' &
      occ$wcvp_taxon_name == nomes[i]


    index <- ifelse(is.na(index), FALSE,index)

    points <- occ[index==TRUE,]

    tdwg_points <- parse_tdwg_distribution(points = points,
                                           native_range = tdwg_dist,
                                           range_polygons = TDWG_LEVEL3)

    # View(tdwg_points[,50:88])

    # plot(TDWG_LEVEL3[['geometry']])
    # plot(point_sf, add=TRUE)
    # View(tdwg_dist)

    tdwg_points_res <- rbind(tdwg_points_res,
                             tdwg_points)

  }

  return(tdwg_points_res)
}
