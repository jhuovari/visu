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

  # Pienruutukuviossa ruutujen otsikot vievat juuri sen kaistan piirtoalan
  # ylapuolelta, johon alaotsikko muuten asettuu: mitattuna ruutujen otsikot
  # ovat -18..0 pikselia ja alaotsikko -16..+1 pikselia piirtoalan
  # ylareunasta. Alaotsikko nostetaan siksi ruutujen otsikoiden yli, ja
  # ylamarginaali kasvaa saman verran, jottei se puolestaan osu otsikkoon.
  ruutuja <- length(visu_panels(w))
  nosto <- if (ruutuja > 1L) 22 else 0
  if (nosto > 0) {
    # Plotly keskittaa otsikon ylamarginaaliin, joten kasvanut marginaali
    # toisi senkin alemmas ja alaotsikko osuisi siihen. Otsikko ankkuroidaan
    # siksi kuvan ylareunaan, jolloin se pysyy paikallaan marginaalin kasvaessa.
    w$x$layout$title$yref <- "container"
    w$x$layout$title$yanchor <- "top"
    w$x$layout$title$y <- 1
    w$x$layout$title$pad <- list(t = 17)
  }

  annotations <- list()
  if (!is.null(subtitle)) {
    # Yhden ruudun kuviossa alaotsikko on osuutena piirtoalan korkeudesta
    # kuten ennenkin. Pienruuduissa paikka annetaan pikseleina piirtoalan
    # ylareunasta, jotta se asettuu ruutujen otsikoiden ylapuolelle kuvion
    # korkeudesta riippumatta: 38 pikselia yloaspain jattaa ruutujen
    # 18-pikseliselle otsikkoriville kolme pikselia ilmaa.
    annotations <- c(annotations, list(
      if (ruutuja > 1L) {
        visu_annotation(subtitle, y = 1, size = 12, yshift = 38)
      } else {
        visu_annotation(subtitle, y = 1.06, size = 12)
      }
    ))
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
    margin = list(t = 60 + nosto, b = 80),
    annotations = annotations,
    separators = visu_separators(locale)
  )

  # Y-akseli seuraa aika-akselin zoomia, jotta nakyva sarja tayttaa kuvion.
  # Vain aikasarjoissa: poikkileikkauskuviossa x-akselia ei zoomata. Ei
  # myoskaan pienruuduissa: niiden yhteinen asteikko on koko pointti, ja
  # ruutukohtainen skaalaus veisi ruuduilta vertailukelpoisuuden.
  if (aika && ruutuja == 1L) {
    w <- htmlwidgets::onRender(w, visu_autoscale_js())
  }
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

  # Pienruutukuviossa ggplotly tekee jokaisesta kerroksesta yhden jaljen
  # ruutua kohti, joten nollaviivalta poistetaan ne kaikki eika vain
  # ensimmaista.
  w$x$data <- w$x$data[-seq_len(min(visu_panel_count(p), length(w$x$data)))]

  # Suoraan asetteluun eika plotly::layout():lla, joka pudottaa ggplotly-
  # objektin muodot rakennusvaiheessa. Viiva piirretaan ruudun omaan
  # koordinaatistoon, jotta se osuu oikealle nollalle myos silloin kun
  # ruutuja on useita.
  w$x$layout$shapes <- c(w$x$layout$shapes, lapply(visu_panels(w), function(ruutu) {
    list(
      type = "line", layer = "below",
      xref = paste0(ruutu[["x"]], " domain"), x0 = 0, x1 = 1,
      yref = ruutu[["y"]], y0 = 0, y1 = 0,
      line = list(color = "grey35", width = 1)
    )
  }))
  w
}

# Kuvion pienruudut. ggplotly nimeaa ruudun akseliparilla, jossa sarakkeet
# jakavat x-akselin ja rivit y-akselin, joten ruudut loytyvat jaljille
# merkittyjen parien joukosta.
visu_panels <- function(w) {
  parit <- lapply(w$x$data, function(tr) {
    c(x = tr$xaxis %||% "x", y = tr$yaxis %||% "y")
  })
  if (length(parit) == 0L) return(list(c(x = "x", y = "y")))
  parit[!duplicated(vapply(parit, paste, character(1), collapse = " "))]
}

visu_panel_count <- function(p) {
  ruudut <- tryCatch(nrow(ggplot2::ggplot_build(p)$layout$layout),
                     error = function(e) NULL)
  if (is.null(ruudut) || is.na(ruudut) || ruudut < 1L) 1L else as.integer(ruudut)
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
  # Pienruutukuviossa sarakkeilla on omat akselinsa (xaxis, xaxis2, ...), ja
  # skaalaamatta jaanyt akseli nayttaisi ruutunsa tyhjana: data olisi
  # miljoonia kertoja akselin vasemmalla puolella.
  for (akseli in visu_axis_names(w, "xaxis")) {
    if (!is.null(w$x$layout[[akseli]]$range)) {
      w$x$layout[[akseli]]$range <- as.numeric(w$x$layout[[akseli]]$range) * kerroin
    }
    w$x$layout[[akseli]]$type <- "date"
  }
  w
}

# Asettelun akselit nimen alkuosan mukaan: xaxis, xaxis2, ... Jarjestys on
# sama kuin asettelussa, ja yhden ruudun kuviossa osumia on tasan yksi.
visu_axis_names <- function(w, alku) {
  grep(paste0("^", alku, "[0-9]*$"), names(w$x$layout), value = TRUE)
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
  for (akseli in visu_axis_names(w, "xaxis")) {
    if (aika || visu_numeric_ticks(w$x$layout[[akseli]]$ticktext)) {
      w <- visu_clear_ticks(w, akseli, muoto = !aika)
    }
  }
  for (akseli in visu_axis_names(w, "yaxis")) {
    if (visu_numeric_ticks(w$x$layout[[akseli]]$ticktext)) {
      w <- visu_clear_ticks(w, akseli, muoto = TRUE)
    }
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

# Lahde piirtoalan alapuolisen sisallon alle. Selite latoutuu alareunaan niin
# monelle riville kuin nimet vaativat, ja riveja tulee lisaa kun ikkuna
# kapenee. Paperiyksikoissa annettu paikka ei kesta sita: yksikko on osuus
# piirtoalan korkeudesta, ja kun selite kasvaa, piirtoala kutistuu ja sama
# osuus on pienempi matka pikseleina - lahde siis nousee selitteen paalle
# juuri silloin kun tilaa on vahiten. Mitattuna kaksirivinen selite toi
# lahteen 36 pikselia selitteen sisaan. Siksi paikka asetetaan pikseleina
# vasta kun kuvio on piirretty.
#
# Selite ei ole ainoa este: kun sita ei ole lainkaan, alin sisalto on
# x-akselin lukurivi, ja pelkka piirtoalan alareunaan sidottu lahde osui sen
# paalle. Siksi mitataan kaikki piirtoalan alapuolinen.
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

    // Piirtoalan alapuolelle jaa muutakin kuin selite: x-akselin luvut ja
    // mahdollinen akselin otsikko. Niiden alareunat mitataan suhteessa
    // piirtoalan alareunaan ja lahde asetetaan alimman alle. Mittaus on
    // luettava piirretysta kuviosta, koska etaisyydet riippuvat piirtoalan
    // korkeudesta eivatka ole tiedossa ennen piirtoa.
    var pohja = gd._fullLayout.height - gd._fullLayout.margin.b;
    var ylareuna = gd.getBoundingClientRect().top;
    var alaosa = 0;
    var alapuoliset = gd.querySelectorAll('.legend, .xtick, .g-xtitle');
    for (var n = 0; n < alapuoliset.length; n++) {
      var r = alapuoliset[n].getBoundingClientRect();
      if (r.height === 0) continue;
      alaosa = Math.max(alaosa, r.bottom - ylareuna - pohja);
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
