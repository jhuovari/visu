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

#' Hae Suomen Pankin korkosarja
#'
#' Lukee Suomen Pankin verkkosivujen korkorajapinnan, joka palauttaa JSONina
#' yhden sarjan kutakin maturiteettia kohti. Rajapinta ei vaadi avainta.
#'
#' Rajapinta antaa vain viimeisimmät noin kolme viikkoa eikä tottele
#' aikarajausparametreja, joten pidempi historia pitää kerätä itse:
#' ks. [visu_accumulate()].
#'
#' @param url Rajapinnan osoite, esim.
#'   `"https://www.suomenpankki.fi/api/interestrates/euribor"`.
#' @return Data frame sarakkeilla `time`, `sarja` ja `values`.
#' @examples
#' \dontrun{
#' visu_get_bof("https://www.suomenpankki.fi/api/interestrates/euribor")
#' }
#' @export
visu_get_bof <- function(url) {
  raaka <- visu_px_json(url, "Korkoja")
  if (is.null(raaka)) {
    stop("Osoitteesta ", url, " ei saatu korkoja.", call. = FALSE)
  }

  # Sarjat ovat listan nimettyja alkioita; muut kentat, kuten errors, eivat
  # ole havaintotauluja eivatka kuulu tulokseen.
  sarjat <- Filter(function(x) is.data.frame(x) &&
                     all(c("date", "value") %in% names(x)), raaka)
  if (length(sarjat) == 0L) {
    stop("Osoite ", url, " ei palauttanut odotettua JSON-muotoa. ",
         "Kentat: ", paste(names(raaka), collapse = ", "), call. = FALSE)
  }

  osat <- lapply(names(sarjat), function(nimi) {
    data.frame(
      time = as.Date(sarjat[[nimi]]$date),
      sarja = nimi,
      values = suppressWarnings(as.numeric(sarjat[[nimi]]$value)),
      stringsAsFactors = FALSE
    )
  })
  d <- do.call(rbind, osat)
  d <- d[!is.na(d$time) & !is.na(d$values), , drop = FALSE]
  d <- d[order(d$sarja, d$time), , drop = FALSE]
  rownames(d) <- NULL
  d
}

#' Kerää lyhyen ikkunan rajapinnasta pitkä sarja
#'
#' Yhdistää uudet havainnot aiemmin tallennettuun csv-tiedostoon ja palauttaa
#' koko kertyneen sarjan. Tarkoitettu lähteille, jotka näyttävät vain
#' viimeisimmät viikot: kun sivusto ajetaan säännöllisesti, historia karttuu
#' tiedostoon eikä katkea.
#'
#' Päällekkäisissä havainnoissa uusi arvo voittaa, jotta lähteen korjaukset
#' menevät läpi. Tiedosto kirjoitetaan aina järjestyksessä, jotta git-diff
#' näyttää vain uudet rivit.
#'
#' @param data Uudet havainnot. Sarake `values` on arvo, muut sarakkeet
#'   yhdessä yksilöivät havainnon.
#' @param path Csv-tiedoston polku. Luodaan jos sitä ei vielä ole.
#' @return Koko kertynyt sarja data framena.
#' @examples
#' \dontrun{
#' visu_get_bof("https://www.suomenpankki.fi/api/interestrates/euribor") |>
#'   visu_accumulate("../data/euribor.csv")
#' }
#' @export
visu_accumulate <- function(data, path) {
  if (!"values" %in% names(data)) {
    stop("Datassa pitää olla sarake `values`.", call. = FALSE)
  }
  avaimet <- setdiff(names(data), "values")

  vanha <- visu_read_accumulated(path, names(data))
  # Uudet rivit viimeisena, jotta duplicated() pudottaa vanhan arvon.
  kaikki <- rbind(vanha, data[names(vanha)])
  kaikki <- kaikki[!duplicated(kaikki[avaimet], fromLast = TRUE), , drop = FALSE]
  kaikki <- kaikki[do.call(order, kaikki[avaimet]), , drop = FALSE]
  rownames(kaikki) <- NULL

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(kaikki, path, row.names = FALSE, fileEncoding = "UTF-8")
  kaikki
}

# Aiemmin kertynyt sarja, tai tyhja kehys samoilla sarakkeilla. Aika luetaan
# Date-tyypiksi, jotta yhdistetty kehys kelpaa kuviolle sellaisenaan.
visu_read_accumulated <- function(path, sarakkeet) {
  if (!file.exists(path)) {
    tyhja <- lapply(sarakkeet, function(x) character())
    return(as.data.frame(stats::setNames(tyhja, sarakkeet),
                         stringsAsFactors = FALSE)[0, , drop = FALSE])
  }
  vanha <- utils::read.csv(path, stringsAsFactors = FALSE)
  if ("time" %in% names(vanha)) vanha$time <- as.Date(vanha$time)
  vanha[sarakkeet]
}
