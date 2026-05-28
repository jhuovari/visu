#' Visualisoi StatFi-data ggplotilla
#'
#' @param data Data frame visualisointia varten.
#' @param x X-akselin sarakkeen nimi merkkijonona.
#' @param y Y-akselin sarakkeen nimi merkkijonona.
#' @param fill Valinnainen täyttövärin sarakkeen nimi merkkijonona.
#' @return ggplot-objekti.
#' @export
plot_statfi_data <- function(data, x, y, fill = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Paketti 'ggplot2' puuttuu. Asenna se ensin.", call. = FALSE)
  }
  if (!requireNamespace("ggcustom", quietly = TRUE)) {
    stop("Paketti 'ggcustom' puuttuu. Asenna se ensin.", call. = FALSE)
  }

  aes_map <- ggplot2::aes(x = .data[[x]], y = .data[[y]])
  if (!is.null(fill)) {
    aes_map <- ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])
  }

  ggplot2::ggplot(data = data, mapping = aes_map) +
    ggplot2::geom_col() +
    ggcustom::theme_ggcustom()
}
