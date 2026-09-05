#' Muuta ggplot-kuvio interaktiiviseksi
#'
#' Kääntää `visu_plot()`:n tuottaman kuvion plotly-widgetiksi: arvot näkyvät
#' osoittimella, kuviota voi zoomata ja sarjoja piilottaa selitteestä.
#'
#' Huomaa, että plotly ei tue ggplotin `subtitle`- ja `caption`-elementtejä.
#' Anna ne tarvittaessa argumenteilla `subtitle` ja `caption`, jotka
#' sijoitetaan plotlyn omaan asetteluun.
#'
#' @param p ggplot-objekti, tyypillisesti `visu_plot()`:n tulos.
#' @param tooltip Vihjelaatikossa näytettävät aestetiikat.
#' @param subtitle,caption Valinnaiset tekstit, jotka ggplotly muuten pudottaisi.
#' @param locale Plotlyn työkalupalkin ja lukumuotoilun kieli. Plotlyn mukana
#'   tulevat muun muassa `"fi"` ja `"sv"`; englanti on sen oletus.
#' @param ... Lisäargumentit funktiolle `plotly::ggplotly()`.
#' @return plotly-objekti (htmlwidget).
#' @export
visu_interactive <- function(p,
                             tooltip = c("x", "y", "colour", "fill"),
                             subtitle = NULL,
                             caption = NULL,
                             locale = "fi",
                             ...) {
  if (!inherits(p, "ggplot")) {
    stop("`p` pit\u00e4\u00e4 olla ggplot-objekti, ei ", class(p)[1], ".", call. = FALSE)
  }

  w <- plotly::ggplotly(p, tooltip = tooltip, ...)

  annotations <- list()
  if (!is.null(subtitle)) {
    annotations <- c(annotations, list(visu_annotation(subtitle, y = 1.06, size = 12)))
  }
  if (!is.null(caption)) {
    annotations <- c(annotations, list(visu_annotation(caption, y = -0.28, size = 10)))
  }

  w <- plotly::layout(
    w,
    legend = list(orientation = "h", x = 0, y = -0.15, title = list(text = "")),
    margin = list(t = 60, b = 80),
    annotations = annotations
  )

  plotly::config(
    w,
    displaylogo = FALSE,
    locale = locale,
    modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d")
  )
}

# Vasempaan reunaan ankkuroitu kuvion ulkopuolinen tekstiselite.
visu_annotation <- function(text, y, size) {
  list(
    text = text, x = 0, y = y,
    xref = "paper", yref = "paper",
    xanchor = "left", yanchor = "top",
    showarrow = FALSE,
    font = list(size = size)
  )
}
