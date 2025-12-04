# Instale se precisar:
# install.packages(c("httr2","jsonlite","httr"))

ih_get_institutions <- function(
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


x <- ih_get_institutions()
