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
#' @param width Kuvion leveys tuumina. Vaikuttaa vain siihen, mahtuuko selite
#'   alareunaan, kun `legend = "auto"`.
#' @param linewidth Viivojen paksuus. `NULL` jättää [visu_plot()]:n
#'   paksuuden (0.8). Esityksessä käytetään ggcustomin VM-teeman paksuutta
#'   1.5. Jos paksuus on kartoitettu sarjaan, kaikki sarjat paksunevat samassa
#'   suhteessa, jolloin niiden järjestys säilyy. Nollaviiva ei muutu.
#' @param height Kuvion korkeus tuumina. Kun se annetaan, otsikko, alaotsikko
#'   ja lähde sovitetaan kuvion leveyteen: liian pitkä otsikko jaetaan
#'   riveille, ja lähde, joka ei mahdu piirtoalueen levyiseksi, tasataan koko
#'   kuvion oikeaan reunaan ja tarvittaessa jaetaan riveille. Tekstit mitataan
#'   samalla PNG-laitteella, jolla [ggplot2::ggsave()] kuvan piirtää. `NULL`
#'   jättää tekstit ennalleen.
#' @return ggplot-objekti.
#' @export
visu_restyle <- function(p, start = NULL, end = NULL, titles = TRUE,
                         legend = NULL, scale = 1, width = 6.28,
                         linewidth = NULL, height = NULL) {
  if (!inherits(p, "ggplot")) {
    stop("`p` pitää olla ggplot-objekti, ei ", class(p)[1], ".", call. = FALSE)
  }
  if (isFALSE(titles)) {
    p <- p + ggplot2::labs(title = NULL, subtitle = NULL)
  }
  if (identical(legend, "auto")) legend <- visu_legend_side(p, width, scale)
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
  if (!is.null(linewidth)) p <- visu_restyle_linewidth(p, linewidth)
  if (!is.null(start) || !is.null(end)) p <- visu_restyle_range(p, start, end)
  # Viimeisena, koska selitteen paikka, tekstikoko ja akselin rajat vaikuttavat
  # kaikki siihen, paljonko tilaa teksteille jaa.
  if (!is.null(height)) p <- visu_fit_text(p, width, height)
  p
}

# Aikarajaus. Y-raja lasketaan uudelleen nakyvasta datasta.
visu_restyle_range <- function(p, start, end) {
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

# Otsikot ja lahde kuvion leveyteen. Kuvio rakennetaan gtableksi samalla
# laitteella ja mitoilla kuin ggsave sen piirtaa, jolloin seka tekstien etta
# akselin ja selitteen leveydet ovat todellisia eika arvioita merkkimaarasta.
#
# Otsikko alkaa piirtoalueen vasemmasta reunasta (VM-teeman
# plot.title.position = "panel") ja voi jatkua selitteen ylle, joten sille
# jaa tilaa kuvion oikeaan reunaan asti. Lahde taas tasataan piirtoalueen
# oikeaan reunaan ja kasvaa vasemmalle: jos selite on oikealla, lahde
# leikkautuu vasemmasta reunasta. Silloin se tasataan koko kuvion oikeaan
# reunaan, mika on pienin poikkeama teemasta.
visu_fit_text <- function(p, width, height) {
  tiedosto <- tempfile(fileext = ".png")
  grDevices::png(tiedosto, width = width, height = height, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tiedosto)
  }, add = TRUE)

  tila <- visu_text_room(p, width)
  for (osa in c("title", "subtitle")) {
    p <- visu_wrap_label(p, osa, tila[[osa]], tila$tarve[[osa]])
  }
  if (tila$tarve$caption > tila$caption) {
    p <- p + ggplot2::theme(plot.caption.position = "plot")
    tila <- visu_text_room(p, width)
    p <- visu_wrap_label(p, "caption", tila$caption, tila$tarve$caption)
  }
  p
}

# Kunkin tekstin tarvitsema ja kaytettavissa oleva leveys tuumina.
visu_text_room <- function(p, width) {
  g <- ggplot2::ggplotGrob(p)
  tuumina <- function(u) grid::convertWidth(u, "in", valueOnly = TRUE)
  leveydet <- vapply(seq_along(g$widths), function(i) tuumina(g$widths[i]), numeric(1))
  # Piirtoalueen sarakkeet ovat null-yksikoita, jotka muuntuvat nollaksi;
  # niiden yhteisleveys on se mita kiinteilta sarakkeilta jaa.
  paneelit <- g$layout[grepl("^panel", g$layout$name), , drop = FALSE]
  vasen <- min(paneelit$l)
  oikea <- max(paneelit$r)
  vapaa <- width - sum(leveydet)
  reunus <- ggplot2::calc_element("plot.margin", ggplot2:::plot_theme(p))
  reunus <- vapply(seq_along(reunus), function(i) tuumina(reunus[i]), numeric(1))

  paneelin_alku <- sum(leveydet[seq_len(vasen - 1L)])
  paneelin_loppu <- paneelin_alku + sum(leveydet[vasen:oikea]) + vapaa
  tarve <- lapply(c(title = "title", subtitle = "subtitle", caption = "caption"),
                  function(nimi) {
    i <- which(g$layout$name == nimi)
    if (length(i) == 0L) return(0)
    tuumina(grid::grobWidth(g$grobs[[i[1]]]))
  })
  lahde_paneelissa <- !identical(
    ggplot2::calc_element("plot.caption.position", ggplot2:::plot_theme(p)), "plot")
  list(
    title = width - paneelin_alku - reunus[2],
    subtitle = width - paneelin_alku - reunus[2],
    caption = if (lahde_paneelissa) paneelin_loppu - reunus[4] else width - reunus[2] - reunus[4],
    tarve = tarve
  )
}

# Jaa teksti riveille niin, etta pisin rivi mahtuu tilaan. Rivin pituus
# merkkeina arvioidaan mitatusta leveydesta, ja pieni vara kattaa sen, etta
# merkit ovat eri levyisia.
visu_wrap_label <- function(p, osa, tila, tarve) {
  teksti <- p$labels[[osa]]
  if (is.null(teksti) || !is.character(teksti) || tarve <= tila) return(p)
  merkkeja <- max(nchar(strsplit(teksti, "\n")[[1]]))
  rivi <- max(10L, floor(merkkeja * tila / tarve * 0.95))
  rivit <- vapply(strsplit(teksti, "\n")[[1]], function(r) {
    paste(strwrap(r, width = rivi), collapse = "\n")
  }, character(1))
  p + do.call(ggplot2::labs, stats::setNames(list(paste(rivit, collapse = "\n")), osa))
}

# Viivat paksummiksi. Kerrokset ja skaalat ovat ggproto-olioita eli
# viittauksia, joten ne kopioidaan ennen muokkausta: muuten muutos nakyisi myos
# alkuperaisessa kuviossa, joka voi olla sivun muistissa.
visu_restyle_linewidth <- function(p, linewidth) {
  if (!is.numeric(linewidth) || length(linewidth) != 1L || linewidth <= 0) {
    stop("`linewidth` pit\u00e4\u00e4 olla yksi positiivinen luku.", call. = FALSE)
  }
  kerroin <- linewidth / visu_linewidths(1)
  p$layers <- lapply(p$layers, function(l) {
    if (!inherits(l$geom, "GeomLine") || is.null(l$aes_params$linewidth)) return(l)
    uusi <- ggplot2::ggproto(NULL, l)
    uusi$aes_params$linewidth <- l$aes_params$linewidth * kerroin
    uusi
  })
  skaala <- p$scales$get_scales("linewidth")
  if (!is.null(skaala)) {
    uusi <- skaala$clone()
    vanha <- skaala$palette
    uusi$palette <- function(n) vanha(n) * kerroin
    skaalat <- p$scales$clone()
    # Kloonaus kopioi myos skaalat, joten oikea tunnistetaan estetiikasta.
    skaalat$scales <- lapply(skaalat$scales, function(s) {
      if ("linewidth" %in% s$aesthetics) uusi else s
    })
    p$scales <- skaalat
  }
  p
}

# Alareuna vai oikea reuna. Alareunassa selite latoutuu yhdelle riville, joka
# leikkautuu jos se ei mahdu kuvion leveyteen: pelkka sarjojen lukumaara ei
# siis riita, koska nelja pitkaa nimea vie enemman tilaa kuin kuusi lyhytta.
# Siksi verrataan arvioitua rivin leveytta kuvion leveyteen.
visu_legend_side <- function(p, width = 6.28, scale = 1) {
  nimet <- visu_legend_labels(p)
  if (length(nimet) == 0L) return("bottom")
  if (visu_legend_width(nimet, scale) > width) "right" else "bottom"
}

# Selitelivin leveys tuumina. Merkin keskileveys on noin puolet fonttikoosta,
# ja jokainen avain vie lisaksi noin kolmanneksen tuumasta.
visu_legend_width <- function(nimet, scale = 1, base_size = 12) {
  merkkia <- sum(nchar(nimet))
  merkkia * 0.5 * base_size * scale / 72 + length(nimet) * 0.35
}

# Selitteeseen tulevat nimet. Paageomin nimet tulevat kuvion datasta, ja
# omalla datallaan piirretyt kerrokset - kuten pylvaiden paallysviiva - omasta
# kartoituksestaan.
visu_legend_labels <- function(p) {
  sarakkeet <- c(visu_mapping_col(p$mapping$colour),
                 visu_mapping_col(p$mapping$fill),
                 visu_mapping_col(p$mapping$linewidth))
  nimet <- unlist(lapply(sarakkeet, function(s) {
    if (is.null(s) || !s %in% names(p$data)) return(NULL)
    arvot <- p$data[[s]]
    if (is.factor(arvot)) levels(droplevels(arvot)) else as.character(unique(arvot))
  }))

  kerrokset <- Filter(function(l) !inherits(l$geom, "GeomHline"), p$layers)
  omat <- Filter(function(l) is.data.frame(l$data), utils::tail(kerrokset, -1L))
  lisat <- unlist(lapply(omat, function(l) {
    q <- l$mapping$colour %||% l$mapping$fill
    if (is.null(q)) return(NULL)
    as.character(tryCatch(rlang::eval_tidy(q, l$data), error = function(e) NULL))
  }))

  unique(c(nimet, lisat))
}

visu_legend_entries <- function(p) length(visu_legend_labels(p))

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
