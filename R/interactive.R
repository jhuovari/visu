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
#'   tulevat muun muassa `"fi"` ja `"sv"`; englanti on sen oletus. Ohjaa myös
#'   desimaalierottimen: suomessa ja ruotsissa pilkku.
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

  # ggplotly kiinnittaa akselimerkinnat alkunakymaan ja piirtaa nollaviivan
  # sen levyisena janana. Kumpikin loppuu kesken heti kun kuviota zoomataan
  # ulospain, joten ne puretaan plotlyn omiksi.
  w <- visu_hline_shape(w, p)
  aika <- !is.null(visu_time_scale(p))
  w <- visu_time_axis(w, p)
  w <- visu_dynamic_ticks(w, aika)

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
    annotations = annotations,
    separators = visu_separators(locale)
  )

  # Y-akseli seuraa aika-akselin zoomia, jotta nakyva sarja tayttaa kuvion.
  # Vain aikasarjoissa: poikkileikkauskuviossa x-akselia ei zoomata.
  if (aika) w <- htmlwidgets::onRender(w, visu_autoscale_js())

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

# Nollaviiva jaljesta muodoksi. ggplotly piirtaa geom_hlinen kahden pisteen
# janana alkunakyman levyiselta, jolloin viiva loppuu kesken zoomatessa;
# paperikoordinaatteihin ankkuroitu muoto jatkuu aina reunasta reunaan.
visu_hline_shape <- function(w, p) {
  if (length(p$layers) == 0L || !inherits(p$layers[[1]]$geom, "GeomHline")) return(w)
  if (length(w$x$data) == 0L) return(w)

  w$x$data <- w$x$data[-1L]
  # Suoraan asetteluun eika plotly::layout():lla, joka pudottaa ggplotly-
  # objektin muodot rakennusvaiheessa.
  w$x$layout$shapes <- c(w$x$layout$shapes, list(list(
    type = "line", layer = "below",
    xref = "paper", x0 = 0, x1 = 1,
    yref = "y", y0 = 0, y1 = 0,
    line = list(color = "grey35", width = 1)
  )))
  w
}

# Aika-akseli plotlyn omaksi date-akseliksi. ggplotly antaa paivat lukuina
# lineaarisella akselilla, jolloin plotly ei osaa muodostaa merkintoja
# alkunakyman ulkopuolelle. Date-akselilla se muotoilee ne itse zoomin mukaan.
visu_time_axis <- function(w, p) {
  kerroin <- visu_time_scale(p)
  if (is.null(kerroin)) return(w)

  w$x$data <- lapply(w$x$data, function(tr) {
    if (!is.null(tr$x) && is.numeric(tr$x)) tr$x <- tr$x * kerroin
    tr
  })
  if (!is.null(w$x$layout$xaxis$range)) {
    w$x$layout$xaxis$range <- as.numeric(w$x$layout$xaxis$range) * kerroin
  }
  w$x$layout$xaxis$type <- "date"
  w
}

# Kuinka monella millisekunnilla ggplotlyn x-luvut kerrotaan. NULL kun x ei
# ole aikaa, jolloin akseli jatetaan rauhaan.
visu_time_scale <- function(p) {
  arvot <- tryCatch(rlang::eval_tidy(p$mapping$x, p$data), error = function(e) NULL)
  if (inherits(arvot, "Date")) return(86400000)
  if (inherits(arvot, "POSIXct")) return(1000)
  NULL
}

# Kiinteat akselimerkinnat pois. ggplotly laskee ne alkunakymalle, joten
# zoomattaessa akselit jaisivat tyhjiksi tai vanhentuneiksi.
#
# Luokka-akselilla merkintataulukko on kuitenkin ainoa paikka, jossa luokkien
# nimet ovat — ggplotly antaa jaljille pelkat jarjestysnumerot. Siksi taulukko
# puretaan vain kun merkinnat ovat lukuja tai kun akselista tehtiin
# aika-akseli. Numeeriselle akselille annetaan ryhmitelty muoto, koska
# plotlyn oletus lyhentaisi tuhannet muotoon "35k".
visu_dynamic_ticks <- function(w, aika = FALSE) {
  if (aika || visu_numeric_ticks(w$x$layout$xaxis$ticktext)) {
    w <- visu_clear_ticks(w, "xaxis", muoto = !aika)
  }
  if (visu_numeric_ticks(w$x$layout$yaxis$ticktext)) {
    w <- visu_clear_ticks(w, "yaxis", muoto = TRUE)
  }
  w
}

visu_clear_ticks <- function(w, akseli, muoto) {
  if (is.null(w$x$layout[[akseli]])) return(w)
  w$x$layout[[akseli]]$tickmode <- "auto"
  w$x$layout[[akseli]]$tickvals <- NULL
  w$x$layout[[akseli]]$ticktext <- NULL
  if (muoto) w$x$layout[[akseli]]$tickformat <- ","
  w
}

# Ovatko merkinnat lukuja? Desimaalipilkku ja tuhaterottimena kaytetty
# valilyonti kuuluvat lukuun, samoin typografinen miinusmerkki.
visu_numeric_ticks <- function(ticktext) {
  if (is.null(ticktext) || length(ticktext) == 0L) return(FALSE)
  teksti <- as.character(ticktext)
  teksti <- gsub("\u2212", "-", teksti)
  teksti <- gsub("[ \u00a0]", "", teksti)
  teksti <- sub(",", ".", teksti, fixed = TRUE)
  all(!is.na(suppressWarnings(as.numeric(teksti))))
}

# Desimaali- ja tuhaterotin plotlyn omille merkinnoille ja vihjelaatikolle.
visu_separators <- function(locale) {
  if (locale %in% c("fi", "sv")) ", " else ".,"
}

# Y-akseli sovitetaan nakyvaan aikavaliin aina kun x-akselia zoomataan.
# Plotly ei tee sita itse, joten kuuntelemme relayout-tapahtumaa.
visu_autoscale_js <- function() {
  "function(el) {
  var gd = el;
  function luku(v) {
    if (v === null || v === undefined) return null;
    if (typeof v === 'number') return v;
    var t = new Date(v).getTime();
    return isNaN(t) ? null : t;
  }
  function sovita(alku, loppu) {
    var lo = Infinity, hi = -Infinity, pylvaita = false;
    for (var t = 0; t < gd.data.length; t++) {
      var tr = gd.data[t];
      if (tr.visible === false || tr.visible === 'legendonly') continue;
      if (tr.type === 'bar') pylvaita = true;
      if (!tr.x || !tr.y) continue;
      for (var i = 0; i < tr.x.length; i++) {
        var xv = luku(tr.x[i]);
        var yv = tr.y[i];
        if (xv === null || yv === null || !isFinite(yv)) continue;
        if (alku !== null && xv < alku) continue;
        if (loppu !== null && xv > loppu) continue;
        // Pylvaan y on korkeus ja base sen alkupaa, joten alaspain menevan
        // pylvaan arvo on base eika y.
        var pohja = Array.isArray(tr.base) ? tr.base[i] : (tr.base || 0);
        if (!isFinite(pohja)) pohja = 0;
        var ala = tr.type === 'bar' ? pohja : yv;
        var yla = tr.type === 'bar' ? pohja + yv : yv;
        if (ala > yla) { var apu = ala; ala = yla; yla = apu; }
        if (ala < lo) lo = ala;
        if (yla > hi) hi = yla;
      }
    }
    if (!isFinite(lo) || !isFinite(hi)) return null;
    // Pylvaat lahtevat nollasta; base kattaa tavallisesti tamankin, mutta
    // varmistetaan silti.
    if (pylvaita) { if (lo > 0) lo = 0; if (hi < 0) hi = 0; }
    var vali = hi - lo;
    if (vali === 0) vali = Math.abs(hi) || 1;
    return [lo - vali * 0.05, hi + vali * 0.05];
  }
  var kesken = false;
  gd.on('plotly_relayout', function(e) {
    if (kesken) return;
    var uusi = null;
    if (e['xaxis.autorange'] === true) {
      uusi = sovita(null, null);
    } else if (e['xaxis.range[0]'] !== undefined) {
      uusi = sovita(luku(e['xaxis.range[0]']), luku(e['xaxis.range[1]']));
    } else if (e['xaxis.range'] !== undefined) {
      uusi = sovita(luku(e['xaxis.range'][0]), luku(e['xaxis.range'][1]));
    }
    if (uusi === null) return;
    kesken = true;
    Plotly.relayout(gd, {'yaxis.range': uusi}).then(function() { kesken = false; });
  });
}"
}
