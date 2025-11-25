#' @title Download Index Herbariorum Data from API
#' @name get_index_herbariorum
#'
#' @description
#' Downloads herbarium institution data from the Index Herbariorum API maintained
#' by the New York Botanical Garden. Returns a structured list or data frame
#' containing information about herbaria worldwide.
#'
#' @param url
#' Character. The API endpoint URL for Index Herbariorum data.
#' Default is "https://sweetgum.nybg.org/science/api/v1/institutions".
#'
#' @param flatten
#' Logical. If `TRUE` (default), flattens nested JSON structures into
#' a data frame. If `FALSE`, returns the raw nested list structure.
#'
#' @param timeout_sec
#' Numeric. Timeout in seconds for API request. Default is 30 seconds.
#'
#' @param verbose
#' Logical. If `TRUE`, displays verbose HTTP request information.
#' Default is `FALSE`.
#'
#' @details
#' ## Data Source:
#' The Index Herbariorum API provides comprehensive information about
#' herbaria institutions worldwide, including codes, locations, contact
#' information, and collection statistics.
#'
#' ## HTTP Client:
#' The function preferentially uses `httr2` if available for modern
#' HTTP handling with retry logic. Falls back to `httr` if `httr2` is
#' not installed.
#'
#' ## Error Handling:
#' - Implements retry logic for transient errors (403, 500+)
#' - Provides informative error messages for HTTP failures
#' - Handles timeout scenarios gracefully
#'
#' @return
#' A list or data frame (depending on `flatten` parameter) containing
#' Index Herbariorum institution data. Typical structure includes:
#' - Institution codes and names
#' - Geographic information (country, city)
#' - Contact details and URLs
#' - Collection statistics
#' - Administrative metadata
#'
#' @author
#' Pablo Hendrigo Alves de Melo,
#' Nadia Bystriakova &
#' Alexandre Monro
#'
#' @seealso
#' [`prepare_gbif_occurrence_data()`] for preparing GBIF occurrence data,
#' [`select_gbif_fields()`] for selecting relevant GBIF fields
#'
#' @examples
#' \donttest{
#' # Download Index Herbariorum data
#' tryCatch({
#'   herbarium_data <- get_index_herbariorum()
#'
#'   # View the structure of the data
#'   str(herbarium_data, max.level = 2)
#'
#'   # Access specific elements
#'   if (!is.null(herbarium_data$data)) {
#'     head(herbarium_data$data)
#'   }
#' }, error = function(e) {
#'   message("API request failed: ", e$message)
#' })
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils runif
#' @export
get_index_herbariorum <- function(
    url = "https://sweetgum.nybg.org/science/api/v1/institutions",
    flatten = TRUE,
    timeout_sec = 30,
    verbose = FALSE
) {
  # Preferir httr2; se não estiver instalado, cair para httr
  use_httr2 <- requireNamespace("httr2", quietly = TRUE)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Pacote 'jsonlite' é necessário.", call. = FALSE)

  if (use_httr2) {
    req <- httr2::request(url) |>
      httr2::req_user_agent(
        # UA "de browser" costuma resolver 403 de WAF
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) R-httr2"
      ) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(timeout_sec) |>
      # Tentativas com backoff (403 às vezes é intermitente no WAF)
      httr2::req_retry(
        max_tries = 3,
        backoff = ~ runif(1, 1, 3),
        is_transient = function(resp) {
          sc <- httr2::resp_status(resp)
          sc >= 500 || sc == 403
        }
      )

    if (verbose) req <- httr2::req_verbose(req)

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      stop(sprintf("Falha HTTP %s ao acessar %s", status, url), call. = FALSE)
    }
    txt <- httr2::resp_body_string(resp)
    out <- jsonlite::fromJSON(txt, flatten = isTRUE(flatten))
    return(out)

  } else {
    # Fallback com httr
    if (!requireNamespace("httr", quietly = TRUE))
      stop("Instale 'httr2' ou 'httr'.", call. = FALSE)

    resp <- httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) R-httr",
        "Accept"     = "application/json"
      ),
      httr::timeout(timeout_sec)
    )
    if (httr::http_error(resp)) {
      stop(sprintf("Falha HTTP %s ao acessar %s",
                   httr::status_code(resp), url), call. = FALSE)
    }
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    out <- jsonlite::fromJSON(txt, flatten = isTRUE(flatten))
    return(out)
  }
}


# # httr fallback implementation
# get_index_herbariorum_httr <- function(url, flatten, timeout_sec, verbose) {
#   resp <- httr::GET(
#     url,
#     httr::add_headers(
#       "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 R-httr",
#       "Accept" = "application/json"
#     ),
#     httr::timeout(timeout_sec)
#   )
#
#   if (httr::http_error(resp)) {
#     stop(sprintf("HTTP error %s accessing %s",
#                  httr::status_code(resp), url), call. = FALSE)
#   }
#
#   txt <- httr::content(resp, as = "text", encoding = "UTF-8")
#   jsonlite::fromJSON(txt, flatten = flatten)
# }
