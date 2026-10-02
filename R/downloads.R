#' Tallenna kuvio muunkielisinä PNG-kuvina ja tulosta latauslinkit
#'
#' Sivusto on suomenkielinen, mutta kuvioista tarvitaan usein myös ruotsin- ja
#' englanninkielinen versio esitykseen tai julkaisuun. Sen sijaan että koko
#' sivu monistettaisiin kolmeksi, kuvio piirretään muilla kielillä PNG:ksi ja
#' tarjotaan latauslinkkinä kuvion alla.
#'
#' Argumentti `builder` on koodilohkossa määritelty funktio, joka rakentaa
#' kuvion annetulla kielellä. Sama funktio piirtää siis sekä sivulla näkyvän
#' suomenkielisen kuvion että ladattavat käännökset, joten käännökset elävät
#' kuvion koodin vieressä eivätkä erillisessä tiedostossa.
#'
#' Tarkoitettu koodilohkoon, jonka asetuksina ovat `#| output: asis` ja
#' `#| echo: false`.
#'
#' @param builder Funktio, joka saa kielikoodin (`"sv"`, `"en"`) ja palauttaa
#'   ggplot-kuvion.
#' @param id Tiedostonimen runko, esimerkiksi `"inflaatio-vuosimuutos"`.
#' @param langs Kielet, joista kuva tallennetaan. Oletuksena kaikki kolme.
#'   Suomenkielinen kuva on sivulla interaktiivisena, mutta esitykseen ja
#'   julkaisuun tarvitaan sekin kuvana.
#' @param dir Hakemisto kuville, suhteessa sivuun. Oletuksena `"kuvat"`.
#' @param width,height,dpi Kuvan mitat tuumina ja tarkkuus. Oletukset
#'   vastaavat sivuston kuvioiden mittasuhteita.
#' @return Tulostettu markdown näkymättömänä; kutsutaan tulosteen vuoksi.
#' @export
visu_downloads <- function(builder, id,
                           langs = c("fi", "sv", "en"),
                           dir = "kuvat",
                           width = 8, height = 4.5, dpi = 150) {
  if (!is.function(builder)) {
    stop("`builder` pit\u00e4\u00e4 olla funktio, joka saa kielikoodin ja palauttaa ",
         "ggplot-kuvion.", call. = FALSE)
  }
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    stop("`id` pit\u00e4\u00e4 olla yksi tiedostonimen runko merkkijonona.", call. = FALSE)
  }
  # Kuvio kirjataan aina, myos hiljaisessa tilassa: rekisteri on se, mista
  # kuvion saa kasiinsa sivuston ulkopuolella.
  visu_chart_register(id, builder)
  if (isTRUE(the$quiet)) return(invisible(""))

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  tiedostot <- vapply(langs, function(lang) {
    kuvio <- builder(lang)
    if (!inherits(kuvio, "ggplot")) {
      stop("`builder(\"", lang, "\")` ei palauttanut ggplot-kuviota.", call. = FALSE)
    }
    tiedosto <- file.path(dir, paste0(id, "-", lang, ".png"))
    ggplot2::ggsave(tiedosto, kuvio, width = width, height = height,
                    dpi = dpi, bg = "white")
    tiedosto
  }, character(1))

  linkit <- paste0("[", visu_lang_name(langs), "](", tiedostot, ")")

  # Otsikot ja lahteet luetaan valmiista kuvioista, jotta luettelo kertoo
  # kuvion oikean nimen eika vain tiedostonimen rungon.
  # Polut luetteloon sivuston hakemiston suhteen, jotta ne toimivat mista
  # tahansa ajettuna; visu_downloads itse ajetaan kuvion oman sivun vieressa.
  otsikot <- visu_chart_labels(builder)
  kuvat <- stats::setNames(as.list(file.path("kuviot", tiedostot)), langs)
  visu_chart_register(id, builder, titles = otsikot, pngs = kuvat)
  visu_catalog_touch(id, visu_current_page(id), otsikot, kuvat)

  # Tyhja rivi lopussa, jotta seuraava asis-lohko ei jatku samalta rivilta.
  ulos <- paste0("Lataa kuva: ", paste(linkit, collapse = " \u00b7 "), "\n\n")
  cat(ulos)
  invisible(ulos)
}

# Kielen nimi omalla kielellaan, jotta linkki on tunnistettava.
visu_lang_name <- function(lang) {
  nimet <- c(fi = "Suomeksi", sv = "P\u00e5 svenska", en = "In English")
  ifelse(lang %in% names(nimet), unname(nimet[lang]), lang)
}

# Mille sivulle renderoitava kuvio kuuluu. Knitr tietaa kaannettavan
# tiedoston; muuten tunniste etsitaan viereisista .qmd-tiedostoista, koska
# visu_downloads ajetaan aina kuvion oman sivun vieressa.
visu_current_page <- function(id) {
  tiedosto <- if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::current_input()
  } else {
    NULL
  }
  if (!is.null(tiedosto)) return(tools::file_path_sans_ext(basename(tiedosto)))

  for (qmd in list.files(".", pattern = "\\.qmd$")) {
    rivit <- readLines(qmd, warn = FALSE)
    if (any(grepl(paste0("visu_downloads\\([^,]+,\\s*\"", id, "\""), rivit))) {
      return(tools::file_path_sans_ext(basename(qmd)))
    }
  }
  NA_character_
}
