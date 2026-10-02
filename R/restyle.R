#' Sovita kuvio esitykseen
#'
#' Pienet muokkaukset valmiiseen kuvioon: aikarajaus ja otsikoiden poisto.
#' Tarkoitettu [visu_chart()]:n kautta, mutta toimii mille tahansa
#' [visu_plot()]:n tekemälle kuviolle.
#'
#' Aikarajaus ei ole pelkkä `coord_cartesian(xlim = ...)`: se pyyhkisi
#' [visu_plot()]:n laskeman y-rajan, jolloin akseli jäisi koko historian
#' mukaiseksi ja näkymä litistyisi. Y-raja lasketaan siksi uudelleen
#' näkyvästä datasta, pinotut pylväät ja mahdollinen päällysviiva mukaan
#' lukien.
#'
#' @param p ggplot-objekti.
#' @param start,end Aikarajauksen alku ja loppu. `NULL` jättää rajan
#'   ennalleen.
#' @param titles `FALSE` poistaa otsikon ja alaotsikon. Lähde jää, koska se
#'   kuuluu kuvioon myös esityksessä.
#' @param legend Selitteen paikka. `NULL` jättää teeman mukaisen oikean
#'   reunan, `"auto"` valitsee paikan sarjojen määrän mukaan, ja ggplotin omat
#'   arvot (`"bottom"`, `"right"`, `"none"`) kelpaavat sellaisenaan. Dian
#'   kapeassa palstassa oikean reunan selite vie neljänneksen leveydestä
#'   piirtoalalta, mutta alareunassa monen sarjan selite vie saman verran
#'   korkeutta — `"auto"` siirtää selitteen alas vain kun sarjoja on vähän.
#' @param scale Tekstien kokokerroin. Esityksessä kuvio on pienempänä ja
#'   katsotaan kauempaa kuin näytöllä, joten tekstit tarvitsevat kokoa lisää.
#' @return ggplot-objekti.
#' @export
visu_restyle <- function(p, start = NULL, end = NULL, titles = TRUE,
                         legend = NULL, scale = 1) {
  if (!inherits(p, "ggplot")) {
    stop("`p` pitää olla ggplot-objekti, ei ", class(p)[1], ".", call. = FALSE)
  }
  if (isFALSE(titles)) {
    p <- p + ggplot2::labs(title = NULL, subtitle = NULL)
  }
  if (identical(legend, "auto")) legend <- visu_legend_side(p)
  if (!is.null(legend)) {
    p <- p + ggplot2::theme(legend.position = legend)
    # Alareunassa selitteet asettuvat oletuksena vierekkain, jolloin kahden
    # selitteen kuviossa (pinotut erat ja paallysviiva) rivi ei mahdu kuvion
    # leveyteen ja oikea reuna leikkautuu. Pystyyn ladottuna kumpikin saa
    # koko leveyden ja katkeaa riveille itse.
    if (identical(legend, "bottom")) {
      p <- p + ggplot2::theme(legend.box = "vertical",
                              legend.justification = "left",
                              legend.box.just = "left")
    }
  }
  if (!identical(scale, 1)) {
    if (!is.numeric(scale) || length(scale) != 1L || scale <= 0) {
      stop("`scale` pit\u00e4\u00e4 olla yksi positiivinen luku.", call. = FALSE)
    }
    # Teeman oma peruskoko kerrotaan, jotta kaikki tekstit kasvavat samassa
    # suhteessa eika yksittaisia elementteja tarvitse luetella.
    p <- p + ggplot2::theme(text = ggplot2::element_text(size = 12 * scale))
  }
  if (is.null(start) && is.null(end)) return(p)

  tiedot <- visu_plot_spec(p)
  if (is.null(tiedot)) {
    stop("Kuvion aikarajausta ei voi muuttaa: x-akseli ei ole aikaa.",
         call. = FALSE)
  }

  alku <- if (is.null(start)) min(tiedot$x, na.rm = TRUE) else as.Date(start)
  loppu <- if (is.null(end)) max(tiedot$x, na.rm = TRUE) else as.Date(end)
  if (alku >= loppu) {
    stop("Aikarajauksen alku '", alku, "' ei ole loppua '", loppu,
         "' aiemmin.", call. = FALSE)
  }

  nakyva <- tiedot$data[!is.na(tiedot$x) & tiedot$x >= alku & tiedot$x <= loppu, ,
                        drop = FALSE]
  if (nrow(nakyva) == 0L) {
    stop("Aikavälillä ", alku, "–", loppu, " ei ole havaintoja.", call. = FALSE)
  }
  viiva <- tiedot$line
  if (!is.null(viiva)) {
    aika <- as.Date(viiva[[tiedot$x_col]])
    viiva <- viiva[!is.na(aika) & aika >= alku & aika <= loppu, , drop = FALSE]
    if (nrow(viiva) == 0L) viiva <- NULL
  }

  # Koordinaatiston korvaaminen on tassa tarkoitus, joten ggplotin siita
  # antama huomautus ei kerro kutsujalle mitaan.
  suppressMessages(p + ggplot2::coord_cartesian(
    xlim = c(alku, loppu),
    ylim = visu_view_ylim(nakyva, tiedot$x_col, tiedot$y_col, tiedot$type,
                          tiedot$stack, viiva)
  ))
}

# Alareuna vai oikea reuna. Alareunassa selite vie korkeutta rivi kerrallaan,
# joten se kannattaa vain kun sarjoja on vahan; muuten piirtoala jaa matalaksi.
# Raja on neljassa, koska siihen asti selite mahtuu dian palstassa yhdelle
# tai kahdelle riville.
visu_legend_side <- function(p, max_entries = 4L) {
  if (visu_legend_entries(p) > max_entries) "right" else "bottom"
}

visu_legend_entries <- function(p) {
  sarakkeet <- c(visu_mapping_col(p$mapping$colour),
                 visu_mapping_col(p$mapping$fill),
                 visu_mapping_col(p$mapping$linewidth))
  maarat <- vapply(sarakkeet, function(s) {
    if (is.null(s) || !s %in% names(p$data)) return(0L)
    visu_level_count(p$data, s)
  }, integer(1))
  # Paallysviiva on oma kerroksensa omalla variskaalallaan, eli yksi
  # selitemerkinta lisaa.
  kerrokset <- Filter(function(l) !inherits(l$geom, "GeomHline"), p$layers)
  viiva <- length(kerrokset) > 1L &&
    is.data.frame(kerrokset[[length(kerrokset)]]$data)
  sum(maarat) + as.integer(viiva)
}

# Mita kuviosta pitaa tietaa y-rajan laskemiseksi uudelleen: aika- ja
# arvosarake, piirtotyyppi, pinotaanko, ja mahdollinen paallysviivan data.
# Luetaan kuviosta itsestaan, jotta rajaus toimii ilman etta kutsuja tietaa
# miten kuvio on tehty.
visu_plot_spec <- function(p) {
  x_col <- visu_mapping_col(p$mapping$x)
  y_col <- visu_mapping_col(p$mapping$y)
  if (is.null(x_col) || is.null(y_col) || is.null(p$data)) return(NULL)
  if (!x_col %in% names(p$data) || !y_col %in% names(p$data)) return(NULL)
  x <- p$data[[x_col]]
  if (!inherits(x, c("Date", "POSIXct"))) return(NULL)

  # Paageomi on ensimmainen muu kuin nollaviiva; paallysviiva on oma
  # kerroksensa omalla datallaan.
  kerrokset <- Filter(function(l) !inherits(l$geom, "GeomHline"), p$layers)
  if (length(kerrokset) == 0L) return(NULL)
  paa <- kerrokset[[1]]
  type <- if (inherits(paa$geom, "GeomCol") || inherits(paa$geom, "GeomBar")) {
    "col"
  } else if (inherits(paa$geom, "GeomArea")) {
    "area"
  } else {
    "line"
  }
  stack <- inherits(paa$position, "PositionStack")

  viiva <- NULL
  if (length(kerrokset) > 1L) {
    oma <- kerrokset[[length(kerrokset)]]$data
    if (is.data.frame(oma) && all(c(x_col, y_col) %in% names(oma))) viiva <- oma
  }

  list(data = p$data, x = as.Date(x), x_col = x_col, y_col = y_col,
       type = type, stack = stack, line = viiva)
}

# Sarakkeen nimi aes-lausekkeesta. visu_plot kayttaa .data[[nimi]] -muotoa,
# jossa nimi on merkkijono.
visu_mapping_col <- function(quosure) {
  if (is.null(quosure)) return(NULL)
  lauseke <- rlang::quo_get_expr(quosure)
  if (is.call(lauseke) && identical(as.character(lauseke[[1]]), "[[")) {
    arvo <- lauseke[[3]]
    if (is.character(arvo)) return(arvo)
    ymparisto <- rlang::quo_get_env(quosure)
    arvo <- tryCatch(eval(arvo, ymparisto), error = function(e) NULL)
    if (is.character(arvo) && length(arvo) == 1L) return(arvo)
  }
  if (is.symbol(lauseke)) return(as.character(lauseke))
  NULL
}
