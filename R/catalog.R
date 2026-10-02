# Kuvioluettelo ja kuvion haku sivuston ulkopuolelle.
#
# Sivuston kuviot ovat .qmd-lohkoissa funktioina, jotka saavat kielen ja
# palauttavat ggplotin. Funktioiden nimet toistuvat sivun sisalla - lahes
# jokaisessa lohkossa on jokin `kuvio` - joten sivua ei voi vain ajaa ja hakea
# kuviota nimella. Ainoa kohta, jossa kuvio on sidottu pysyvaan tunnisteeseen,
# on `visu_downloads(builder, id)`. Siksi rekisterointi tehdaan siella.

#' Kuvioluettelon tiedosto
#'
#' @param site_dir Sivuston hakemisto, ks. [visu_site_dir()].
#' @return Tiedostopolku merkkijonona.
#' @export
visu_catalog_path <- function(site_dir = NULL) {
  file.path(visu_site_dir(site_dir), "_visu_charts.json")
}

#' Lue kuvioluettelo
#'
#' Luettelo syntyy sivuston rakentamisen sivutuotteena: jokainen
#' [visu_downloads()]-kutsu kirjaa kuvionsa, ja sivun valmistuttua merkinnat
#' kirjoitetaan tiedostoon. Luettelo on se, mihin esitystyökalut viittaavat,
#' kun kuvio pitää löytää nimellä.
#'
#' @param site_dir Sivuston hakemisto.
#' @return Data frame sarakkeilla `id`, `page`, `title_fi`, `title_sv`,
#'   `title_en`, `source`, `png_fi`, `png_sv`, `png_en`. Nollarivinen jos
#'   luetteloa ei vielä ole.
#' @export
visu_catalog_read <- function(site_dir = NULL) {
  path <- visu_catalog_path(site_dir)
  tyhja <- visu_catalog_frame(list())
  if (!file.exists(path)) return(tyhja)
  luettelo <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(luettelo) || length(luettelo) == 0L) return(tyhja)
  visu_catalog_frame(luettelo)
}

visu_catalog_frame <- function(luettelo) {
  kentat <- c("id", "page", "title_fi", "title_sv", "title_en", "source",
              "png_fi", "png_sv", "png_en")
  sarakkeet <- lapply(kentat, function(k) {
    vapply(luettelo, function(rivi) {
      arvo <- rivi[[k]]
      if (is.null(arvo) || length(arvo) != 1L) NA_character_ else as.character(arvo)
    }, character(1))
  })
  names(sarakkeet) <- kentat
  as.data.frame(sarakkeet, stringsAsFactors = FALSE)
}

# Kuvion kirjaus ajon aikana. Kutsutaan visu_downloads():sta, joka on ainoa
# paikka jossa kuvio ja sen tunniste ovat yhta aikaa kasilla.
visu_chart_register <- function(id, builder, titles = NULL, pngs = NULL) {
  the$charts[[id]] <- list(builder = builder, titles = titles, pngs = pngs)
  invisible(id)
}

# Kuvion otsikko ja lahde kielittain. Kuvio rakennetaan mutta ei piirreta,
# joten tama on halpa: ggplot-olio syntyy ilman renderointia.
visu_chart_labels <- function(builder, langs = c("fi", "sv", "en")) {
  ulos <- list()
  for (lang in langs) {
    p <- tryCatch(builder(lang), error = function(e) NULL)
    if (is.null(p)) next
    ulos[[lang]] <- list(
      title = p$labels$title %||% NA_character_,
      caption = p$labels$caption %||% NA_character_
    )
  }
  ulos
}

# Kuvion merkinta luetteloon. Kutsutaan visu_downloads():sta renderoinnin
# aikana, jolloin kuvio ja sen tunniste ovat kasilla. Merkinta kirjoitetaan
# heti eika ajon lopuksi, koska quarto renderoi jokaisen sivun omassa
# prosessissaan: ajon lopussa muistissa ei olisi mitaan.
visu_catalog_touch <- function(id, page, titles, pngs,
                               path = file.path("..", "_visu_charts.json")) {
  luettelo <- if (file.exists(path)) {
    jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else {
    list()
  }
  luettelo <- Filter(function(rivi) !identical(rivi$id, id), luettelo)

  kielet <- c("fi", "sv", "en")
  merkinta <- c(
    list(id = id, page = page),
    stats::setNames(lapply(kielet, function(l) titles[[l]]$title %||% NA),
                    paste0("title_", kielet)),
    list(source = titles$fi$caption %||% NA),
    stats::setNames(lapply(kielet, function(l) pngs[[l]] %||% NA),
                    paste0("png_", kielet))
  )

  luettelo <- c(luettelo, list(merkinta))
  jarjestys <- order(vapply(luettelo, function(r) paste(r$page, r$id), character(1)))
  jsonlite::write_json(luettelo[jarjestys], path, auto_unbox = TRUE,
                       pretty = TRUE, null = "null", na = "null")
  invisible(path)
}

#' Siivoa luettelosta poistuneet kuviot
#'
#' Luetteloon kirjataan kuvio kerrallaan renderoinnin aikana, joten poistetun
#' kuvion merkinta jaisi muuten eloon. Tama poistaa merkinnat, joiden
#' tunnistetta ei enaa ole missaan .qmd-tiedostossa.
#'
#' @param site_dir Sivuston hakemisto.
#' @return Poistettujen tunnisteiden vektori nakymattomana.
#' @export
visu_catalog_prune <- function(site_dir = NULL) {
  path <- visu_catalog_path(site_dir)
  if (!file.exists(path)) return(invisible(character()))
  luettelo <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  elossa <- visu_chart_scan(site_dir)$id

  poistuneet <- setdiff(vapply(luettelo, function(r) r$id, character(1)), elossa)
  if (length(poistuneet) == 0L) return(invisible(character()))

  jaljelle <- Filter(function(r) r$id %in% elossa, luettelo)
  jsonlite::write_json(jaljelle, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  invisible(poistuneet)
}

#' Rakenna kuvioluettelo kerralla
#'
#' Luettelo taydentyy normaalisti sivuston rakentamisen yhteydessa kuvio
#' kerrallaan. Tama rakentaa sen kerralla ajamalla jokaisen sivun koodin:
#' hyodyllinen tuoreessa kloonissa ja silloin, kun luettelo halutaan ajan
#' tasalle ilman sivuston renderointia.
#'
#' Kuvia ei piirreta, joten PNG-polut ovat ne, joihin [visu_downloads()]
#' kuvat kirjoittaa. Sivujen data haetaan rajapinnasta, joten ajo kestaa
#' muutaman minuutin.
#'
#' @param site_dir Sivuston hakemisto.
#' @param langs Kielet, joilla kuvien polut kirjataan.
#' @return Luettelo data framena nakymattomana.
#' @export
visu_catalog_build <- function(site_dir = NULL, langs = c("fi", "sv", "en")) {
  sivut <- unique(visu_chart_scan(site_dir)$page)
  polku <- visu_catalog_path(site_dir)
  for (sivu in sivut) {
    rekisteri <- visu_page_charts(sivu, site_dir)
    for (id in names(rekisteri)) {
      visu_catalog_touch(
        id, sivu, visu_chart_labels(rekisteri[[id]]$builder),
        stats::setNames(
          as.list(file.path("kuviot", "kuvat", paste0(id, "-", langs, ".png"))),
          langs),
        path = polku)
    }
    message("luetteloon: ", sivu, " (", length(rekisteri), " kuviota)")
  }
  visu_catalog_prune(site_dir)
  invisible(visu_catalog_read(site_dir))
}
