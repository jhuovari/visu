#' Tulosta kuvion taulun ja sarjojen metatiedot
#'
#' Kirjoittaa kuvion alle lyhyen erittelyn siitä, mistä taulusta luvut ovat,
#' milloin taulu on päivittynyt, mitkä sarjat kuviossa ovat ja miten aineisto
#' on rajattu. Näin kuvion voi jäljittää lähteeseen ilman että koodilohko
#' pitää avata.
#'
#' Tarkoitettu koodilohkoon, jonka asetuksina ovat `#| output: asis` ja
#' `#| echo: false`.
#'
#' @param data `pxwebtools::pxw_get_data()`:n palauttama data frame. Sarjat ja
#'   rajaukset luetaan sen `codes_names`-attribuutista, joten ne vastaavat
#'   täsmälleen sitä mitä kuvioon haettiin. Usean taulun kuviossa anna lista
#'   data frameja ja `url`-vektori samassa järjestyksessä.
#' @param url Taulun API-osoite, sama jolla data haettiin.
#' @param contents Sisältömuuttujan koodi. Oletuksena `"contentscode"`, joka on
#'   StatFinin nykyinen nimi Tiedot-muuttujalle.
#' @return Metatiedot merkkijonona näkymättömänä; kutsutaan tulosteen vuoksi.
#' @export
visu_metadata <- function(data, url, contents = "contentscode") {
  if (is.list(data) && !is.data.frame(data)) {
    if (length(data) != length(url)) {
      stop("`data`-listassa ja `url`-vektorissa pit\u00e4\u00e4 olla yht\u00e4 monta ",
           "alkiota; nyt ", length(data), " ja ", length(url), ".", call. = FALSE)
    }
    lohkot <- vapply(seq_along(data), function(i) {
      visu_metadata_one(data[[i]], url[[i]], contents)
    }, character(1))
    return(visu_metadata_emit(lohkot))
  }

  visu_metadata_emit(visu_metadata_one(data, url, contents))
}

# Erittely kaytetaan harvoin, joten se on taitoksen takana kuten koodikin.
# Sisalto erotetaan tyhjilla riveilla, jotta pandoc kasittelee sen markdownina
# eika raakana HTML:na.
visu_metadata_emit <- function(lohkot) {
  ulos <- paste0(
    "<details class=\"kuvion-tiedot\">\n",
    "<summary>Kuvion tiedot</summary>\n\n",
    paste(lohkot, collapse = "\n"),
    "\n</details>\n\n"
  )
  cat(ulos)
  invisible(ulos)
}

visu_metadata_one <- function(data, url, contents = "contentscode") {
  meta <- visu_table_meta(url)
  codes <- attr(data, "codes_names") %||% list()

  rivit <- character()
  rivit <- c(rivit, visu_def("Taulu", visu_table_link(meta, url)))

  updated <- visu_table_updated(url)
  if (!is.na(updated)) {
    rivit <- c(rivit, visu_def("P\u00e4ivitetty", visu_format_stamp(updated)))
  }

  # Sarjat ensin ja muut rajaukset perassa taulun jarjestyksessa: sarjat ovat
  # se mita kuviossa nakyy, rajaus vain tarkennus.
  jarjestys <- c(intersect(contents, names(codes)), setdiff(names(codes), contents))
  for (muuttuja in jarjestys) {
    arvot <- visu_value_labels(data, codes, muuttuja)
    if (length(arvot) == 0L) next
    otsikko <- if (identical(muuttuja, contents)) {
      "Sarjat"
    } else {
      visu_variable_text(meta, muuttuja)
    }
    rivit <- c(rivit, visu_def(otsikko, paste(arvot, collapse = " \u00b7 ")))
  }

  paste(rivit, collapse = "\n")
}

# Pandocin maaritelmalista: otsikkorivi ja sisennetty selitysrivi.
visu_def <- function(otsikko, arvo) {
  paste0(otsikko, "\n:   ", arvo, "\n")
}

# Datassa esiintyvien arvojen selitteet taulun jarjestyksessa.
visu_value_labels <- function(data, codes, muuttuja) {
  if (!muuttuja %in% names(data)) return(character())
  selitteet <- codes[[muuttuja]]
  arvot <- unique(as.character(data[[muuttuja]]))
  osuvat <- selitteet[names(selitteet) %in% arvot]
  if (length(osuvat) == 0L) arvot else unname(osuvat)
}

visu_variable_text <- function(meta, koodi) {
  osuma <- meta$variables$text[meta$variables$code == koodi]
  if (length(osuma) == 1L && !is.na(osuma)) as.character(osuma) else koodi
}

# Taulun tunnus ja otsikko linkkina PxWebin selausnakymaan.
# Osoitteesta luettava otsikko: palvelin ja polun viimeinen osa riittavat
# kertomaan mista sarja on.
visu_url_title <- function(url) {
  ilman <- sub("[?#].*$", "", url)
  palvelin <- sub("^https?://([^/]+).*$", "\\1", url)
  tunnus <- utils::tail(strsplit(ilman, "/", fixed = TRUE)[[1]], 1L)
  kysely <- sub("^[^?]*\\??", "", url)
  if (nzchar(kysely)) tunnus <- paste0(tunnus, "?", kysely)
  paste0(palvelin, " ", tunnus)
}

visu_table_link <- function(meta, url) {
  parts <- visu_split_table_url(url)
  tunnus <- if (is.null(parts)) NULL else sub("\\.px$", "", parts$table)
  otsikko <- meta$title %||% tunnus %||% url
  # PxWebin otsikko kertaa lopussa kyselyn muuttujat, mika ei kuulu erittelyyn.
  otsikko <- sub(" muuttujina .*$", "", otsikko)
  teksti <- if (is.null(tunnus)) otsikko else paste0(tunnus, " ", otsikko)
  paste0("[", teksti, "](", sub("/+$", "", url), ")")
}

#' Muotoile aikaleima Suomen aikaan
#'
#' Näyttää aikaleiman muodossa `11.9.2026 klo 8:08`. Vyöhyke luetaan
#' leimasta: `Z`-päätteinen on UTC:tä ja muunnetaan Suomen aikaan, vyöhykkeetön
#' on jo Suomen aikaa, kuten StatFinin `updated`-kentässä. Pelkästä
#' päivämäärästä jätetään kellonaika pois, koska sitä ei tiedetä.
#'
#' @param stamp Aikaleima merkkijonona, esim. `"2026-09-11T05:08:15Z"`.
#' @return Muotoiltu merkkijono, tai syöte sellaisenaan jos sitä ei voi lukea.
#' @examples
#' visu_format_stamp("2026-09-11T05:08:15Z")
#' visu_format_stamp("2026-09-11T08:00:03")
#' @export
visu_format_stamp <- function(stamp) {
  aika <- visu_parse_stamp(stamp)
  if (is.na(aika)) return(as.character(stamp))
  osat <- as.POSIXlt(aika, tz = "Europe/Helsinki")
  paiva <- sprintf("%d.%d.%d", osat$mday, osat$mon + 1L, osat$year + 1900L)
  if (!visu_stamp_has_time(stamp)) return(paiva)
  sprintf("%s klo %d:%02d", paiva, osat$hour, osat$min)
}

# Aikaleiman vyohyke luetaan leimasta: StatFin antaa Suomen ajan ilman
# vyohyketta, oma tilatiedostomme UTC:n Z-paatteella. Kumpikin nakyy lopulta
# Suomen aikana, joten pelkka merkkijonon siivous ei riita.
visu_parse_stamp <- function(stamp) {
  s <- as.character(stamp)[1]
  if (is.na(s) || !nzchar(s)) return(as.POSIXct(NA))
  tz <- if (grepl("Z$", s)) "UTC" else "Europe/Helsinki"
  s <- sub("Z$", "", s)
  aika <- as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S", tz = tz)
  if (is.na(aika)) aika <- as.POSIXct(s, format = "%Y-%m-%d", tz = tz)
  aika
}

# Pelkka paivamaara nayttaisi kellonajan 0:00, mika olisi vaara tieto.
visu_stamp_has_time <- function(stamp) {
  grepl("T[0-9]{2}:[0-9]{2}", as.character(stamp)[1])
}

# Taulun metatiedot haetaan kerran ajoa kohti, kuten kansiolistauksetkin.
visu_table_meta <- function(url) {
  clean <- sub("/+$", "", url)
  cached <- the$table_meta[[clean]]
  if (!is.null(cached)) return(if (identical(cached, NA)) list() else cached)

  # Muilla kuin PxWeb-tauluilla ei ole vastaavaa metatietokyselya, joten
  # otsikoksi riittaa lahde ja sarjatunnus osoitteesta.
  if (is.null(visu_split_table_url(clean))) {
    meta <- list(title = visu_url_title(clean))
    the$table_meta[[clean]] <- meta
    return(meta)
  }

  meta <- visu_px_json(clean, "Taulun metatietoja")
  if (!is.list(meta)) meta <- NULL

  the$table_meta[[clean]] <- meta %||% NA
  meta %||% list()
}
