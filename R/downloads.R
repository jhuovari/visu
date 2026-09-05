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
#' @param langs Kielet, joista kuva tallennetaan. Oletuksena ruotsi ja
#'   englanti.
#' @param dir Hakemisto kuville, suhteessa sivuun. Oletuksena `"kuvat"`.
#' @param width,height,dpi Kuvan mitat tuumina ja tarkkuus. Oletukset
#'   vastaavat sivuston kuvioiden mittasuhteita.
#' @return Tulostettu markdown näkymättömänä; kutsutaan tulosteen vuoksi.
#' @export
visu_downloads <- function(builder, id,
                           langs = c("sv", "en"),
                           dir = "kuvat",
                           width = 8, height = 4.5, dpi = 150) {
  if (!is.function(builder)) {
    stop("`builder` pit\u00e4\u00e4 olla funktio, joka saa kielikoodin ja palauttaa ",
         "ggplot-kuvion.", call. = FALSE)
  }
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    stop("`id` pit\u00e4\u00e4 olla yksi tiedostonimen runko merkkijonona.", call. = FALSE)
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  linkit <- vapply(langs, function(lang) {
    kuvio <- builder(lang)
    if (!inherits(kuvio, "ggplot")) {
      stop("`builder(\"", lang, "\")` ei palauttanut ggplot-kuviota.", call. = FALSE)
    }
    tiedosto <- file.path(dir, paste0(id, "-", lang, ".png"))
    ggplot2::ggsave(tiedosto, kuvio, width = width, height = height,
                    dpi = dpi, bg = "white")
    paste0("[", visu_lang_name(lang), "](", tiedosto, ")")
  }, character(1))

  # Tyhja rivi lopussa, jotta seuraava asis-lohko ei jatku samalta rivilta.
  ulos <- paste0("Lataa kuva: ", paste(linkit, collapse = " \u00b7 "), "\n\n")
  cat(ulos)
  invisible(ulos)
}

# Kielen nimi omalla kielellaan, jotta linkki on tunnistettava.
visu_lang_name <- function(lang) {
  nimet <- c(fi = "Suomeksi", sv = "P\u00e5 svenska", en = "In English")
  if (lang %in% names(nimet)) unname(nimet[lang]) else lang
}
