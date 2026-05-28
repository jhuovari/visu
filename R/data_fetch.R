#' Hae dataa StatFin-rajapinnasta
#'
#' @param url PXWEB API endpoint -osoite.
#' @param query Lista kyselyparametreista pxwebtools-kutsuun.
#' @return Data frame haetusta datasta.
#' @export
get_statfi_data <- function(url, query = NULL) {
  if (!requireNamespace("pxwebtools", quietly = TRUE)) {
    stop("Paketti 'pxwebtools' puuttuu. Asenna se ensin.", call. = FALSE)
  }

  if (is.null(query)) {
    return(pxwebtools::pxweb_get(url))
  }

  pxwebtools::pxweb_get(url, query = query)
}
