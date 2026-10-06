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
#' @param height Widgetin korkeus pikseleinä. Oletus on hieman korkeampi kuin
#'   plotlyn oma, jotta monirivinen selite ja lähde mahtuvat kuvion alle
#'   ilman että piirtoala kutistuu.
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
                             height = 450,
                             locale = "fi",
                             ...) {
  if (!inherits(p, "ggplot")) {
    stop("`p` pit\u00e4\u00e4 olla ggplot-objekti, ei ", class(p)[1], ".", call. = FALSE)
  }
  # Hiljaisessa tilassa sivu ajetaan vain kuvion hakemiseksi; widgettia ei
  # nayteta kenellekaan, joten sita ei kannata rakentaa.
  if (isTRUE(the$quiet)) return(invisible(p))

  w <- plotly::ggplotly(p, tooltip = tooltip, ...)

  w <- visu_legend_names(w)

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
    # Lahde ankkuroidaan piirtoalan alareunaan ja siirretaan pikseleina
    # alaspain. Lopullisen siirtyman asettaa visu_caption_js() vasta kun
    # selitteen korkeus on mitattavissa; tama on alkuarvo yhdelle riville.
    annotations <- c(annotations, list(visu_annotation(
      caption, y = 0, size = 10, yshift = -40, name = "visu-caption")))
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
  if (!is.null(caption)) w <- htmlwidgets::onRender(w, visu_caption_js())

  # Quarton fig-responsive korvaa htmlwidgetsin kokolaskennan ja tulkitsee
  # korkeuden kuvasuhteeksi: se skaalaa korkeuden leveyden suhteessa 650
  # pikselin oletusleveyteen ja asettaa leveydeksi 100 %. Numeerinen leveys
  # laukaisee skaalauksen, joten 450 kutistui 380 pikseliin (450 / 768 * 650).
  # Kun leveys annetaan valmiiksi prosentteina, skaalaus jaa valiin ja korkeus
  # on se mita pyydettiin -- lopputulos on leveyden osalta sama.
  w$height <- height
  w$width <- "100%"

  plotly::config(
    w,
    displaylogo = FALSE,
    locale = locale,
    modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d")
  )
}

# ggplotly nimeaa jaljen kaikkien selitteellisten aestetiikkojen yhdistelmana
# heti kun niita on useampi kuin yksi. Kun pinottujen pylvaiden paalle tulee
# kokonaissarja omalla variskaalallaan, pylvaan nimesta "Palkat" tulee
# "(Palkat,1)", jossa ykkonen on tyhjan toisen aestetiikan tasonumero. Se
# nakyisi sellaisenaan selitteessa ja vihjelaatikossa.
visu_legend_names <- function(w) {
  w$x$data <- lapply(w$x$data, function(tr) {
    nimi <- visu_plain_name(tr$name)
    if (!is.null(nimi)) {
      if (!is.null(tr$legendgroup)) tr$legendgroup <- nimi
      tr$name <- nimi
    }
    tr
  })
  w
}

# Yhdistelmanimen oikea osa, tai NULL jos nimi ei ole yhdistelma tai jos
# kumpikaan osa ei ole tasonumero. Jalkimmaisessa tapauksessa selitteessa on
# aidosti kaksi aestetiikkaa eika nimea saa karsia.
visu_plain_name <- function(nimi) {
  if (is.null(nimi) || length(nimi) != 1L || is.na(nimi)) return(NULL)
  osat <- regmatches(nimi, regexec("^\\((.+),([^,]+)\\)$", nimi))[[1]]
  if (length(osat) != 3L) return(NULL)
  numero <- grepl("^[0-9]+$", osat[-1L])
  if (sum(numero) != 1L) return(NULL)
  osat[-1L][!numero]
}

# Vasempaan reunaan ankkuroitu kuvion ulkopuolinen tekstiselite.
visu_annotation <- function(text, y, size, yshift = NULL, name = NULL) {
  visu_compact(list(
    text = text, x = 0, y = y,
    xref = "paper", yref = "paper",
    xanchor = "left", yanchor = "top",
    yshift = yshift, name = name,
    showarrow = FALSE,
    font = list(size = size)
  ))
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
    # Pylvaan leveys ja siirtyma ovat x-akselin datayksikoissa, joten ne on
    # skaalattava x:n mukana. Ilman tata neljannesvuosipylvaan leveydeksi jaisi
    # 81 millisekuntia 81 paivan sijaan, eli pylvaat olisivat nakymattomia.
    for (kentta in c("width", "offset")) {
      if (!is.null(tr[[kentta]]) && is.numeric(tr[[kentta]])) {
        tr[[kentta]] <- tr[[kentta]] * kerroin
      }
    }
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

# Lahde selitteen alle. Selite latoutuu alareunaan niin monelle riville kuin
# nimet vaativat, ja riveja tulee lisaa kun ikkuna kapenee. Paperiyksikoissa
# annettu paikka ei kesta sita: yksikko on osuus piirtoalan korkeudesta, ja
# kun selite kasvaa, piirtoala kutistuu ja sama osuus on pienempi matka
# pikseleina - lahde siis nousee selitteen paalle juuri silloin kun tilaa on
# vahiten. Mitattuna kaksirivinen selite toi lahteen 36 pikselia selitteen
# sisaan. Siksi paikka asetetaan pikseleina vasta kun selite on piirretty.
visu_caption_js <- function() {
  "function(el) {
  var gd = el;
  var VALI = 10, RIVI = 16, REUNA = 8, KIERROKSIA = 6;
  var kesken = false, kierros = 0;
  function sovita() {
    if (kesken || !gd.layout || !gd._fullLayout) return;
    var ann = gd.layout.annotations || [];
    var i = -1;
    for (var k = 0; k < ann.length; k++) if (ann[k].name === 'visu-caption') i = k;
    if (i < 0) return;

    // Selitteen alareuna mitataan suhteessa piirtoalan alareunaan. Selite ei
    // ala piirtoalan alareunasta vaan sen alapuolelta, ja etaisyys riippuu
    // piirtoalan korkeudesta, joten se on luettava piirretysta kuviosta.
    var leg = gd.querySelector('.legend');
    var alaosa = 0;
    if (leg) {
      var pohja = gd._fullLayout.height - gd._fullLayout.margin.b;
      alaosa = Math.max(leg.getBoundingClientRect().bottom -
                        gd.getBoundingClientRect().top - pohja, 0);
    }
    var siirto = -(alaosa + VALI);
    var marginaali = alaosa + VALI + RIVI + REUNA;

    var muutos = {};
    if (Math.abs((ann[i].yshift || 0) - siirto) > 1) {
      muutos['annotations[' + i + '].yshift'] = siirto;
    }
    if (Math.abs((gd._fullLayout.margin.b || 0) - marginaali) > 1) {
      muutos['margin.b'] = marginaali;
    }
    // Marginaalin kasvu kutistaa piirtoalaa ja siirtaa selitetta, joten tulos
    // haetaan muutamalla kierroksella. Seuraava kierros ajetaan suoraan
    // relayoutin jalkeen eika plotly_afterplot-tapahtumasta: tapahtuma osuu
    // relayoutin sisaan, jolloin kesken-vartija nielaisee juuri sen kutsun
    // joka jatkaisi iteraatiota, ja tulos jaa puolitiehen. Askel pienenee
    // joka kierroksella, joten raja tayttyy nopeasti; laskuri on varmistus.
    if (Object.keys(muutos).length === 0 || ++kierros > KIERROKSIA) return;
    kesken = true;
    Plotly.relayout(gd, muutos).then(function() {
      kesken = false;
      sovita();
    });
  }
  function aja() {
    kierros = 0;
    sovita();
  }
  gd.on('plotly_afterplot', aja);
  window.addEventListener('resize', function() { setTimeout(aja, 150); });
  aja();
}"
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
