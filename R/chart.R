#' Hae sivuston kuvio ggplot-oliona
#'
#' Ajaa kuvion .qmd-sivun koodin erillisessä ympäristössä ja palauttaa
#' tunnisteen `id` kuvion halutulla kielellä. Näin sivuston kuvion saa
#' esitykseen tai julkaisuun ilman, että koodi monistetaan toiseen paikkaan:
#' kuvio on edelleen määritelty vain sivullaan.
#'
#' Sivun ajaminen hakee sen kaikkien kuvioiden datan rajapinnasta, mikä vie
#' sekunteja. Ajettu sivu jää istunnon muistiin, joten saman sivun seuraavat
#' kuviot ovat ilmaisia.
#'
#' @param id Kuvion tunniste, sama kuin [visu_downloads()]-kutsussa. Luettelon
#'   tunnisteet saa [visu_catalog_read()]:llä.
#' @param lang Kieli: `"fi"` (oletus), `"sv"` tai `"en"`.
#' @param start,end Valinnainen aikarajaus. Oletuksena kuvio on sellaisenaan.
#' @param titles Pidätäänkö kuvion omat otsikot. `FALSE` poistaa otsikon ja
#'   alaotsikon mutta jättää lähteen; käytä kun otsikko tulee dian tekstiksi.
#' @param site_dir Sivuston hakemisto.
#' @return ggplot-objekti.
#' @export
visu_chart <- function(id, lang = "fi", start = NULL, end = NULL,
                       titles = TRUE, legend = NULL, scale = 1, width = 6.28,
                       site_dir = NULL) {
  if (!is.character(id) || length(id) != 1L) {
    stop("`id` pitää olla yksi kuvion tunniste merkkijonona.", call. = FALSE)
  }
  builder <- visu_chart_builder(id, site_dir)
  p <- builder(lang)
  if (!inherits(p, "ggplot")) {
    stop("Kuvio '", id, "' ei palauttanut ggplot-kuviota kielellä '", lang,
         "'.", call. = FALSE)
  }
  visu_restyle(p, start = start, end = end, titles = titles,
               legend = legend, scale = scale, width = width)
}

#' Tallenna sivuston kuvio kuvaksi
#'
#' Kuten [visu_chart()], mutta kirjoittaa tuloksen PNG-tiedostoksi. Mitat
#' annetaan tuumina, jotta ne voi ottaa suoraan esityspohjan
#' paikkamerkistä — kuvio piirretään silloin tasan siihen tilaan, johon se
#' dialla menee, eikä sitä tarvitse venyttää jälkikäteen.
#'
#' @inheritParams visu_chart
#' @param file Tiedostopolku.
#' @param width,height Kuvan mitat tuumina.
#' @param dpi Tarkkuus. Oletus 300 riittää myös tulostukseen.
#' @return Tiedostopolku näkymättömänä.
#' @export
visu_chart_png <- function(id, file, lang = "fi", start = NULL, end = NULL,
                           titles = TRUE, legend = "auto", scale = 1.15,
                           width = 6.28, height = 4.49, dpi = 300,
                           site_dir = NULL) {
  p <- visu_chart(id, lang = lang, start = start, end = end, titles = titles,
                  legend = legend, scale = scale, width = width,
                  site_dir = site_dir)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(file, p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(file)
}

# Kuvion rakentajafunktio sivun koodista. Sivu ajetaan hiljaisessa tilassa,
# jolloin visu_downloads() vain kirjaa kuviot eika piirra PNG-kaannoksia,
# visu_metadata() ei hae taulujen metatietoja ja visu_interactive() ei rakenna
# plotly-widgettia. Ilman niita sivu on pelkkaa datan hakua ja kuvioiden
# maarittelya.
visu_chart_builder <- function(id, site_dir = NULL) {
  sivu <- visu_chart_page(id, site_dir)
  rekisteri <- visu_page_charts(sivu, site_dir)
  if (!id %in% names(rekisteri)) {
    stop("Kuviota '", id, "' ei löytynyt sivulta '", sivu, "'. Sivun kuviot: ",
         paste(names(rekisteri), collapse = ", "), call. = FALSE)
  }
  rekisteri[[id]]$builder
}

# Mille sivulle kuvio kuuluu. Ensisijaisesti luettelosta; jos luetteloa ei ole,
# tunniste etsitaan qmd-tiedostojen visu_downloads-kutsuista.
visu_chart_page <- function(id, site_dir = NULL) {
  luettelo <- visu_catalog_read(site_dir)
  osuma <- luettelo$page[luettelo$id == id]
  if (length(osuma) == 1L) return(osuma)

  sivut <- visu_chart_scan(site_dir)
  osuma <- sivut$page[sivut$id == id]
  if (length(osuma) == 1L) return(osuma)
  stop("Tunnistetta '", id, "' ei ole missään sivun kuviossa. Tarjolla: ",
       paste(utils::head(sort(sivut$id), 20L), collapse = ", "),
       if (nrow(sivut) > 20L) " ..." else "", call. = FALSE)
}

# Tunniste ja sivu suoraan qmd-lahteesta. Ei aja mitaan, joten tama toimii
# myos ennen ensimmaista sivuston rakennusta.
visu_chart_scan <- function(site_dir = NULL) {
  polut <- list.files(visu_charts_dir(site_dir), pattern = "\\.qmd$",
                      full.names = TRUE)
  osat <- lapply(polut, function(polku) {
    rivit <- readLines(polku, warn = FALSE)
    osumat <- regmatches(rivit, regexec(
      "visu_downloads\\([^,]+,\\s*\"([^\"]+)\"", rivit))
    tunnisteet <- vapply(osumat, function(o) if (length(o) == 2L) o[2] else NA_character_,
                         character(1))
    tunnisteet <- tunnisteet[!is.na(tunnisteet)]
    if (length(tunnisteet) == 0L) return(NULL)
    data.frame(id = tunnisteet, page = tools::file_path_sans_ext(basename(polku)),
               stringsAsFactors = FALSE)
  })
  osat <- Filter(Negate(is.null), osat)
  if (length(osat) == 0L) {
    return(data.frame(id = character(), page = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, osat)
}

# Sivun kuviot ajamalla sivun koodi. Tulos jaa istunnon muistiin, koska yhden
# sivun ajo hakee kaikkien sen kuvioiden datan ja esitykseen otetaan usein
# monta kuviota samalta sivulta.
visu_page_charts <- function(page, site_dir = NULL) {
  if (!is.null(the$pages[[page]])) return(the$pages[[page]])
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Paketti knitr tarvitaan sivun koodin lukemiseen.", call. = FALSE)
  }

  polku <- file.path(visu_charts_dir(site_dir), paste0(page, ".qmd"))
  if (!file.exists(polku)) {
    stop("Sivua '", polku, "' ei löydy.", call. = FALSE)
  }

  koodi <- tempfile(fileext = ".R")
  on.exit(unlink(koodi), add = TRUE)
  knitr::purl(polku, output = koodi, quiet = TRUE, documentation = 0L)

  vanha_charts <- the$charts
  vanha_quiet <- the$quiet
  the$charts <- list()
  the$quiet <- TRUE
  # Sivun koodi lukee suhteellisia polkuja omasta hakemistostaan.
  tyohakemisto <- setwd(dirname(polku))
  on.exit({
    setwd(tyohakemisto)
    the$quiet <- vanha_quiet
  }, add = TRUE)

  ymparisto <- new.env(parent = globalenv())
  source(koodi, local = ymparisto, echo = FALSE)

  rekisteri <- the$charts
  the$charts <- vanha_charts
  the$pages[[page]] <- rekisteri
  rekisteri
}
