#' Hae EKP:n aikasarja
#'
#' Lukee sarjan Euroopan keskuspankin Data Portalin SDMX-rajapinnasta.
#' Rajapinta ei vaadi avainta, ja yhdellä osoitteella saa useita sarjoja
#' erottamalla koodit plusmerkillä.
#'
#' Paluuarvo on samassa muodossa kuin `visu_get_data()`:lla: aika ensin,
#' luokittelusarakkeet keskellä ja arvot sarakkeessa `values`.
#' Luokittelusarakkeiksi otetaan ne ulottuvuudet, joilla on kyselyssä useampi
#' kuin yksi arvo — yhden sarjan haussa niitä ei siis tule lainkaan.
#'
#' @param url Sarjan osoite ilman kyselyä, esim.
#'   `"https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A"`.
#'   Muoto ja rajaus lisätään itse, jotta koodissa oleva osoite on sama kuin
#'   etulehdessä ja `visu_check_charts()` voi verrata niitä.
#' @return Data frame, jossa `time` on `Date` ja `values` numeerinen.
#' @examples
#' \dontrun{
#' visu_get_ecb("https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A")
#' }
#' @export
visu_get_ecb <- function(url) {
  raaka <- visu_fetch_csv(paste0(url, "?format=csvdata&detail=dataonly"))
  if (!all(c("TIME_PERIOD", "OBS_VALUE") %in% names(raaka))) {
    stop("Osoite ", url, " ei palauttanut EKP:n csvdata-muotoa. ",
         "Sarakkeet: ", paste(names(raaka), collapse = ", "), call. = FALSE)
  }

  # Ulottuvuudet, joilla on useampi arvo, erottavat sarjat toisistaan; muut
  # ovat koko kyselylle yhteisia eivatka kerro kuviossa mitaan.
  ulottuvuudet <- setdiff(names(raaka), c("KEY", "TIME_PERIOD", "OBS_VALUE"))
  erottavat <- ulottuvuudet[vapply(
    raaka[ulottuvuudet], function(x) length(unique(x)) > 1L, logical(1))]

  d <- data.frame(time = as.Date(raaka$TIME_PERIOD), stringsAsFactors = FALSE)
  for (mu in erottavat) d[[mu]] <- factor(as.character(raaka[[mu]]))
  d$values <- suppressWarnings(as.numeric(raaka$OBS_VALUE))
  d <- d[!is.na(d$time) & !is.na(d$values), , drop = FALSE]
  d <- d[do.call(order, c(d[erottavat], list(d$time))), , drop = FALSE]
  rownames(d) <- NULL

  attr(d, "codes_names") <- visu_codes_names(d, erottavat)
  d
}

#' Hae FRED-aikasarja
#'
#' Lukee sarjan St. Louisin Fedin FRED-palvelun csv-viennistä, joka ei vaadi
#' avainta. FRED on jakelukanava: kerro kuviossa alkuperäinen lähde, esim.
#' Brentin kohdalla EIA.
#'
#' @param url Sarjan csv-osoite, esim.
#'   `"https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILBRENTEU"`.
#' @return Data frame sarakkeilla `time` ja `values`.
#' @examples
#' \dontrun{
#' visu_get_fred("https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILBRENTEU")
#' }
#' @export
visu_get_fred <- function(url) {
  raaka <- visu_fetch_csv(url)
  if (ncol(raaka) < 2L) {
    stop("Osoite ", url, " ei palauttanut FREDin csv-muotoa.", call. = FALSE)
  }

  # FRED merkitsee puuttuvan pisteella, joten arvot luetaan varoituksetta
  # numeroksi ja puuttuvat rivit jaavat pois.
  d <- data.frame(
    time = as.Date(raaka[[1]]),
    values = suppressWarnings(as.numeric(raaka[[2]]))
  )
  d <- d[!is.na(d$time) & !is.na(d$values), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# Sarjojen selitteet visu_metadata():lle. Koodit ovat lahteen omia, joten ne
# kelpaavat sellaisenaan seka avaimeksi etta selitteeksi.
visu_codes_names <- function(data, muuttujat) {
  koodit <- lapply(muuttujat, function(mu) {
    arvot <- levels(droplevels(data[[mu]]))
    stats::setNames(arvot, arvot)
  })
  stats::setNames(koodit, muuttujat)
}

# Csv-haku uudelleenyrityksin. Verkkovirhe yhden aamun ajossa ei saa kaataa
# koko sivustoa, mutta pysyva virhe pitaa nakya.
visu_fetch_csv <- function(url) {
  yritykset <- visu_px_tries()
  for (yritys in seq_len(yritykset)) {
    tulos <- tryCatch(
      utils::read.csv(url, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) e
    )
    if (!inherits(tulos, "error")) return(tulos)
    if (yritys >= yritykset) stop(tulos)
    odota <- visu_csv_backoff(yritys)
    message("Haku osoitteesta ", url, " epäonnistui (",
            conditionMessage(tulos), "). Odotetaan ", odota,
            " s ja yritetään uudelleen (", yritys + 1L, "/",
            yritykset, ").")
    Sys.sleep(odota)
  }
}

visu_csv_backoff <- function(yritys) {
  portaat <- getOption("visu.csv_backoff", c(2, 5, 10))
  portaat[min(as.integer(yritys), length(portaat))]
}

# Milloin muu kuin PxWeb-osoite on paivittynyt. Last-Modified on se mita
# rajapinnat kertovat ilman erillista metatietokyselya; EKP ja FRED lahettavat
# sen, ja se riittaa samaan inkrementaaliseen paattelyyn kuin PxWebin updated.
visu_http_updated <- function(url) {
  otsakkeet <- tryCatch(curlGetHeaders(url), error = function(e) NULL)
  if (is.null(otsakkeet)) return(NA_character_)

  rivit <- grep("^last-modified:", tolower(otsakkeet), value = TRUE)
  if (length(rivit) == 0L) return(NA_character_)

  # Kuukausien lyhenteet ovat englanniksi, joten aika luetaan C-localessa.
  vanha <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", vanha), add = TRUE)
  Sys.setlocale("LC_TIME", "C")

  teksti <- trimws(sub("^[^:]+:", "", rivit[length(rivit)]))
  aika <- as.POSIXct(teksti, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  if (is.na(aika)) return(NA_character_)
  format(aika, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}
