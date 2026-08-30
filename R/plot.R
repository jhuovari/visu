#' Piirrä StatFin-aikasarja
#'
#' Tekee `pxwebtools::pxw_get_data()`-muotoisesta datasta ggplot-kuvion
#' ggcustom-teemalla. Oletukset on valittu `pxw_get_data()`:n paluuarvoa
#' varten: aika on ensimmäinen sarake ja arvot sarakkeessa `values`.
#'
#' @param data Data frame, tyypillisesti `pxwebtools::pxw_get_data()`:n tulos.
#' @param x X-akselin sarake merkkijonona. Oletuksena datan ensimmäinen sarake.
#' @param y Y-akselin sarake merkkijonona. Oletuksena `values`, tai jos sitä ei
#'   ole, datan viimeinen numeerinen sarake.
#' @param colour Valinnainen luokittelusarake merkkijonona. Piirtotyypeissä
#'   `col` ja `area` se ohjaa täyttöväriä, tyypissä `line` viivan väriä.
#' @param type Piirtotyyppi: `"line"` (oletus), `"col"` tai `"area"`.
#' @param title,subtitle,caption Valinnaiset otsikkotekstit.
#' @param y_lab,x_lab Valinnaiset akselien otsikot. Oletuksena akselit ovat
#'   nimeämättömiä, koska yksikkö kuuluu tyypillisesti otsikkoon.
#' @return ggplot-objekti.
#' @examples
#' df <- data.frame(time = as.Date(c("2024-01-01", "2025-01-01")), values = c(1, 2))
#' visu_plot(df)
#' @export
visu_plot <- function(data,
                      x = NULL,
                      y = NULL,
                      colour = NULL,
                      type = c("line", "col", "area"),
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      x_lab = NULL,
                      y_lab = NULL) {
  type <- match.arg(type)
  if (!is.data.frame(data)) {
    stop("`data` pit\u00e4\u00e4 olla data frame, ei ", class(data)[1], ".", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` on tyhj\u00e4. Tarkista kysely.", call. = FALSE)
  }

  x <- x %||% names(data)[1]
  y <- y %||% visu_value_col(data)

  visu_require_col(data, x, "x")
  visu_require_col(data, y, "y")
  if (!is.null(colour)) visu_require_col(data, colour, "colour")

  mapping <- if (is.null(colour)) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  } else if (type == "line") {
    ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[colour]])
  } else {
    ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[colour]])
  }

  geom <- switch(type,
    line = ggplot2::geom_line(linewidth = 0.8),
    col  = ggplot2::geom_col(position = "dodge"),
    area = ggplot2::geom_area(position = "stack")
  )

  p <- ggplot2::ggplot(data, mapping) +
    geom +
    ggplot2::labs(
      title = title, subtitle = subtitle, caption = caption,
      x = x_lab, y = y_lab, colour = NULL, fill = NULL
    ) +
    ggcustom::theme_vm()

  if (!is.null(colour)) {
    p <- p + if (type == "line") ggcustom::scale_colour_vm() else ggcustom::scale_fill_vm()
  }

  p
}

# Arvaa arvosarake: `values` jos on, muuten viimeinen numeerinen sarake.
visu_value_col <- function(data) {
  if ("values" %in% names(data)) return("values")
  num <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(num) == 0L) {
    stop("Datassa ei ole numeerista saraketta. Anna `y` nimenomaisesti. ",
         "Sarakkeet: ", paste(names(data), collapse = ", "), call. = FALSE)
  }
  utils::tail(num, 1L)
}

# Virheilmoitus, joka kertoo mitkä sarakkeet ovat tarjolla.
visu_require_col <- function(data, col, arg) {
  if (!is.character(col) || length(col) != 1L) {
    stop("`", arg, "` pit\u00e4\u00e4 olla yksi sarakkeen nimi merkkijonona.", call. = FALSE)
  }
  if (!col %in% names(data)) {
    stop("Saraketta '", col, "' (argumentti `", arg, "`) ei ole datassa. ",
         "Tarjolla: ", paste(names(data), collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
