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

  visu_sdmx_frame(raaka, as.Date(raaka$TIME_PERIOD),
                  c("KEY", "TIME_PERIOD", "OBS_VALUE"))
}

#' Hae Eurostatin aikasarja
#'
#' Lukee sarjan Eurostatin levitysrajapinnan SDMX-vientinä. Rajapinta ei vaadi
#' avainta, ja yhdellä osoitteella saa useita sarjoja erottamalla koodit
#' plusmerkillä. Osoitteen avain on paikkasidonnainen: ulottuvuudet tulevat
#' aineiston omassa järjestyksessä pistein eroteltuina, esimerkiksi
#' `M.RCH_A.CP00.FI+EA20` on kuukausittainen vuosimuutos koko indeksistä
#' Suomessa ja euroalueella.
#'
#' Paluuarvo on samassa muodossa kuin [visu_get_data()]:lla: aika ensin,
#' luokittelusarakkeet keskellä ja arvot sarakkeessa `values`.
#' Luokittelusarakkeiksi otetaan ne ulottuvuudet, joilla on kyselyssä useampi
#' kuin yksi arvo — yhden sarjan haussa niitä ei siis tule lainkaan.
#'
#' Eurostatin aikajakso on jakson tunnus eikä päivämäärä (`2026`, `2026-Q1`,
#' `2026-02`, `2026-W05`). Se luetaan jakson ensimmäiseksi päiväksi, jolloin
#' sarja asettuu aika-akselille samoin kuin muidenkin lähteiden sarjat.
#'
#' @param url Sarjan osoite ilman kyselyä, esim.
#'   `"https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/prc_hicp_manr/M.RCH_A.CP00.FI+EA20"`.
#'   Muoto lisätään itse, jotta koodissa oleva osoite on sama kuin etulehdessä
#'   ja [visu_check_charts()] voi verrata niitä.
#' @return Data frame, jossa `time` on `Date` ja `values` numeerinen.
#' @examples
#' \dontrun{
#' visu_get_eurostat(paste0(
#'   "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
#'   "prc_hicp_manr/M.RCH_A.CP00.FI+EA20"
#' ))
#' }
#' @export
visu_get_eurostat <- function(url) {
  raaka <- visu_fetch_csv(visu_eurostat_query(url))
  if (!all(c("TIME_PERIOD", "OBS_VALUE") %in% names(raaka))) {
    stop("Osoite ", url, " ei palauttanut Eurostatin SDMX-csv-muotoa. ",
         "Sarakkeet: ", paste(names(raaka), collapse = ", "), call. = FALSE)
  }

  visu_sdmx_frame(raaka, visu_eurostat_time(raaka$TIME_PERIOD),
                  c("DATAFLOW", "LAST UPDATE", "TIME_PERIOD", "OBS_VALUE",
                    "OBS_FLAG", "CONF_STATUS"))
}

# SDMX-csv samaan muotoon kuin visu_get_data(): aika ensin, erottavat
# ulottuvuudet keskella ja arvot sarakkeessa values. Erottavia ovat ne
# ulottuvuudet, joilla on useampi arvo; muut ovat koko kyselylle yhteisia
# eivatka kerro kuviossa mitaan.
visu_sdmx_frame <- function(raaka, time, ohita) {
  ulottuvuudet <- setdiff(names(raaka), ohita)
  erottavat <- ulottuvuudet[vapply(
    raaka[ulottuvuudet], function(x) length(unique(x)) > 1L, logical(1))]

  d <- data.frame(time = time, stringsAsFactors = FALSE)
  for (mu in erottavat) d[[mu]] <- factor(as.character(raaka[[mu]]))
  d$values <- suppressWarnings(as.numeric(raaka$OBS_VALUE))
  d <- d[!is.na(d$time) & !is.na(d$values), , drop = FALSE]
  d <- d[do.call(order, c(d[erottavat], list(d$time))), , drop = FALSE]
  rownames(d) <- NULL

  attr(d, "codes_names") <- visu_codes_names(d, erottavat)
  d
}

# Eurostatin jaksotunnus jakson ensimmaiseksi paivaksi. Vuosineljannes,
# puolivuosi ja viikko ovat kirjaintunnuksia, kuukausi ja paiva suoraan
# paivamaaran alkuja. Tuntematon muoto jaa NA:ksi ja putoaa kuviosta.
visu_eurostat_time <- function(x) {
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))

  vuosi  <- grepl("^[0-9]{4}$", x)
  nelj   <- grepl("^[0-9]{4}-Q[1-4]$", x)
  puoli  <- grepl("^[0-9]{4}-S[12]$", x)
  viikko <- grepl("^[0-9]{4}-W[0-9]{2}$", x)
  kk     <- grepl("^[0-9]{4}-[0-9]{2}$", x)
  paiva  <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)

  # Jokainen muoto asetetaan vain kun sita loytyy: tyhjalla osajoukolla
  # paste0 palauttaisi pituuden 1 eika nollaa, jolloin muunnos kaatuisi
  # katkelmaan kuten "-01-01".
  if (any(vuosi))  out[vuosi]  <- as.Date(paste0(x[vuosi], "-01-01"))
  if (any(nelj))   out[nelj]   <- visu_period_start(x[nelj], 3L)
  if (any(puoli))  out[puoli]  <- visu_period_start(x[puoli], 6L)
  if (any(viikko)) out[viikko] <- as.Date(paste0(x[viikko], "-1"),
                                          format = "%Y-W%U-%u")
  if (any(kk))     out[kk]     <- as.Date(paste0(x[kk], "-01"))
  if (any(paiva))  out[paiva]  <- as.Date(x[paiva])

  out
}

# Vuosineljanneksen ja puolivuoden ensimmainen paiva. Jakson numero on
# kirjaimen jalkeen ja jakson pituus kertoo, monennestako kuukaudesta se alkaa.
visu_period_start <- function(x, pituus) {
  jakso <- as.integer(substr(x, 7, 7))
  as.Date(sprintf("%s-%02d-01", substr(x, 1, 4), (jakso - 1L) * pituus + 1L))
}

# Hakuosoite: muoto lisataan vasta tassa, jotta etulehden ja koodin osoitteet
# ovat samat.
visu_eurostat_query <- function(url) {
  paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", "format=SDMX-CSV")
}

visu_is_eurostat <- function(url) {
  grepl("^https?://[^/]*ec\\.europa\\.eu/eurostat/", as.character(url))
}

# Milloin Eurostatin aineisto on paivitetty. Eurostat ei laheta
# Last-Modified-otsaketta, mutta kertoo paivitysajan datan omassa
# LAST UPDATE -sarakkeessa. Probe maksaa yhden haun, ja tulos muistetaan ajon
# ajaksi, koska sama osoite voi olla usean kuvion lahteena.
visu_eurostat_updated <- function(url) {
  avain <- visu_url_key(url)
  muistissa <- the$eurostat[[avain]]
  if (!is.null(muistissa)) return(muistissa)

  raaka <- tryCatch(visu_fetch_csv(visu_eurostat_query(url)),
                    error = function(e) NULL)
  leima <- visu_eurostat_stamp(raaka)
  the$eurostat[[avain]] <- leima
  leima
}

visu_eurostat_stamp <- function(raaka) {
  if (!is.data.frame(raaka) || !"LAST UPDATE" %in% names(raaka) ||
      nrow(raaka) == 0L) {
    return(NA_character_)
  }
  # Muoto on pp/kk/vv hh:mm:ss ja aika Keski-Euroopan aikaa; vuosiluku on
  # kaksinumeroinen, jonka R lukee 2000-luvulle.
  aika <- as.POSIXct(as.character(raaka[["LAST UPDATE"]][1]),
                     format = "%d/%m/%y %H:%M:%S", tz = "CET")
  if (is.na(aika)) return(NA_character_)
  format(aika, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
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
#' Jos haku epäonnistuu, funktio palauttaa aiemmin kertyneen sarjan ja
#' varoittaa. Näin yhden rajapinnan hetkellinen katko jättää kuvion
#' edellisiin havaintoihin sen sijaan että se kaataisi koko renderöinnin.
#' Tiedosto kirjoitetaan vain kun uusia havaintoja saatiin.
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
  # Argumentti on viela lupaus, joten haun virhe syntyy vasta tassa ja saadaan
  # kiinni. Lyhyen ikkunan lahteessa katkos ei vie mitaan: kertynyt tiedosto
  # on jo levylla, joten kuvio piirtyy edellisilla havainnoilla sen sijaan
  # etta yhden rajapinnan hetkellinen katko kaataisi koko renderoinnin.
  data <- tryCatch(force(data), error = function(e) {
    visu_note("L\u00e4hteen haku ep\u00e4onnistui (", conditionMessage(e),
              "), k\u00e4ytet\u00e4\u00e4n aiemmin kertynytt\u00e4 tiedostoa ", path, ".")
    NULL
  })
  if (is.null(data)) return(visu_stored_series(path))

  if (!"values" %in% names(data)) {
    stop("Datassa pit\u00e4\u00e4 olla sarake `values`.", call. = FALSE)
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
  visu_stored_series(path)[sarakkeet]
}

# Kertynyt sarja sellaisenaan, kun uutta ei saatu. Ilman tiedostoa ei ole
# mitaan nayttaa, joten silloin virhe on oikea lopputulos.
visu_stored_series <- function(path) {
  if (!file.exists(path)) {
    stop("L\u00e4hteen haku ep\u00e4onnistui eik\u00e4 tiedostoa ", path,
         " ole, joten sarjaa ei ole mist\u00e4 lukea.", call. = FALSE)
  }
  vanha <- utils::read.csv(path, stringsAsFactors = FALSE)
  if ("time" %in% names(vanha)) vanha$time <- as.Date(vanha$time)
  vanha
}
