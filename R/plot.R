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
#' @param linewidth Valinnainen sarake, jonka luokat ovat saman sarjan
#'   versioita: alkuperäinen, kausitasoitettu ja trendi. Ne piirretään samalla
#'   värillä mutta eri paksuisella viivalla, koska ne ovat sama ilmiö eri
#'   tavalla siloitettuna — eri värit antaisivat ymmärtää, että kyse on eri
#'   sarjoista. Järjestä luokat karkeimmasta siloitetuimpaan, eli
#'   alkuperäinen ensin ja trendi viimeisenä; viiva paksunee järjestyksessä.
#'   Vain piirtotyypille `"line"`.
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
                      linewidth = NULL,
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
  if (!is.null(linewidth)) {
    visu_require_col(data, linewidth, "linewidth")
    if (type != "line") {
      stop("`linewidth` toimii vain piirtotyypill\u00e4 \"line\", ei tyypill\u00e4 \"",
           type, "\".", call. = FALSE)
    }
  }

  geom <- switch(type,
    # Kun paksuus on kartoitettu, sen antaa skaala eika kiintea arvo.
    line = if (is.null(linewidth)) ggplot2::geom_line(linewidth = 0.8) else ggplot2::geom_line(),
    col  = ggplot2::geom_col(position = "dodge"),
    area = ggplot2::geom_area(position = "stack")
  )

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  if (!is.null(colour)) {
    p <- p + if (type == "line") {
      ggplot2::aes(colour = .data[[colour]])
    } else {
      ggplot2::aes(fill = .data[[colour]])
    }
  }
  if (!is.null(linewidth)) {
    p <- p + ggplot2::aes(linewidth = .data[[linewidth]])
  }

  p <- p +
    geom +
    ggplot2::labs(
      title = title, subtitle = subtitle, caption = caption,
      x = x_lab, y = y_lab, colour = NULL, fill = NULL, linewidth = NULL
    ) +
    ggcustom::theme_vm()

  if (!is.null(colour)) {
    p <- p + if (type == "line") ggcustom::scale_colour_vm() else ggcustom::scale_fill_vm()
  }
  if (!is.null(linewidth)) {
    p <- p + ggplot2::scale_linewidth_manual(
      values = visu_linewidths(visu_level_count(data, linewidth))
    )
  }

  p
}

# Saman sarjan versioiden viivanpaksuudet karkeimmasta siloitetuimpaan.
# Siloitetuin saa normaalin paksuuden ja karkeammat ohenevat, jotta trendi
# erottuu ilman etta alkuperainen sarja katoaa.
visu_linewidths <- function(n, base = 0.8) {
  if (n <= 1L) return(base)
  if (n == 2L) return(c(0.45, base))
  if (n == 3L) return(c(0.3, 0.55, base))
  seq(base * 0.35, base, length.out = n)
}

# Datassa esiintyvien luokkien maara: tekijalla tasot, muuten yksiloidyt arvot.
visu_level_count <- function(data, col) {
  arvot <- data[[col]]
  if (is.factor(arvot)) length(levels(droplevels(arvot))) else length(unique(arvot))
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
