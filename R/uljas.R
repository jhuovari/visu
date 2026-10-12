#' Hae Tullin Uljas-tietokannasta
#'
#' Lukee Suomen tullin Uljas-tietokannan kuution [uljas][uljas::uljas_data]-
#' paketilla ja palauttaa tuloksen samassa muodossa kuin [visu_get_data()]:
#' aika ensin, luokittelusarakkeet keskellä ja arvot sarakkeessa `values`.
#'
#' Kuutio nimetään `ifile`-polulla, ei osoitteella — se on Uljaksen oma tapa
#' yksilöidä tilasto, ja sen saa [uljas::uljas_stats()]:lla. Kysely on lista,
#' jonka nimet ovat kuution luokitusten nimiä ja arvot niiden koodeja; koodit
#' näkee [uljas::uljas_class()]:lla. Kokonaisuuden koodi on tyhjä merkkijono,
#' jota rajapinta ei hyväksy yksin, joten kokonaisuus haetaan `"=ALL"`:lla.
#'
#' Luokitteluarvot palautetaan koodeina ja selitteet erikseen
#' `codes_names`-attribuutissa, jotta kuvion koodi ei riipu pitkistä
#' nimistä. Selitteet haetaan kuution luokituksista.
#'
#' Tulli uudisti kuutiot 30.3.2026: nykyinen kuutio alkaa vuodesta 2025 ja
#' pidempi historia on rinnakkaisessa kuutiossa, jonka nimen lopussa on 2.
#' Pitkä sarja vaatii siis molemmat.
#'
#' @param ifile Kuution polku, esim.
#'   `"/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC"`.
#' @param classifiers Kyselylista, ks. [uljas::uljas_data()].
#' @param lang Kyselyn ja selitteiden kieli. Vaikuttaa molempiin: luokitusten
#'   nimet `classifiers`-listassa ovat sen kielisiä, joten kielen vaihtaminen
#'   vaatii myös kyselyn nimien vaihtamisen. Oletuksena `"en"`, jolla nimet
#'   ovat samat kuin uljas-paketin ohjeissa.
#' @return Data frame, jossa `time` on `Date` ja `values` numeerinen.
#' @examples
#' \dontrun{
#' visu_get_uljas(
#'   "/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC",
#'   classifiers = list(
#'     "SITC Products" = "=ALL", "Time period" = "=ALL", Flow = c(1, 2),
#'     "Continents and groups" = "00", Indicators = "V1"
#'   )
#' )
#' }
#' @export
visu_get_uljas <- function(ifile, classifiers, lang = "en") {
  visu_require_uljas()

  raaka <- visu_fetch(ifile, function(x) {
    # Koodit eika selitteet: selitteet ovat pitkia ja vaihtuvat kielen mukana,
    # joten kuvion koodi olisi niiden varassa hauras.
    suppressMessages(uljas::uljas_data(
      classifiers = classifiers, ifile = x, lang = lang, naming = "id"
    ))
  })
  raaka <- as.data.frame(raaka, stringsAsFactors = FALSE)

  aika <- visu_uljas_time_col(raaka)
  muut <- setdiff(names(raaka), c(aika, "values"))

  d <- data.frame(time = visu_uljas_time(raaka[[aika]]), stringsAsFactors = FALSE)
  for (mu in muut) d[[mu]] <- factor(as.character(raaka[[mu]]))
  d$values <- suppressWarnings(as.numeric(raaka$values))
  d <- d[!is.na(d$time) & !is.na(d$values), , drop = FALSE]
  d <- d[do.call(order, c(d[muut], list(d$time))), , drop = FALSE]
  rownames(d) <- NULL

  attr(d, "codes_names") <- visu_uljas_labels(ifile, muut, lang)
  d
}

visu_require_uljas <- function() {
  if (!requireNamespace("uljas", quietly = TRUE)) {
    stop("Paketti uljas tarvitaan Tullin tietokannan lukemiseen: ",
         "pak::pak(\"jhuovari/uljas\").", call. = FALSE)
  }
  invisible(NULL)
}

# Mika sarakkeista on aika. Etsitaan sisallosta eika nimesta, koska kuution
# aikaluokitus voi olla kuukausi, neljannes tai vuosi ja nimi vaihtuu kielen
# mukana.
visu_uljas_time_col <- function(raaka) {
  ehdokkaat <- setdiff(names(raaka), "values")
  osuu <- vapply(ehdokkaat, function(mu) {
    all(grepl(visu_uljas_period_pattern(), trimws(as.character(raaka[[mu]]))))
  }, logical(1))
  if (!any(osuu)) {
    stop("Uljas-kyselyn tuloksessa ei ole aikasaraketta: yhdenkään ",
         "sarakkeen arvot eivät ole jaksotunnuksia, esimerkiksi '202607'.",
         call. = FALSE)
  }
  ehdokkaat[which(osuu)[1]]
}

# Jaksotunnus jakson ensimmaiseksi paivaksi: 202607, 2026Q3 ja 2026.
visu_uljas_time <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))

  kk <- grepl("^[0-9]{4}(0[1-9]|1[0-2])$", x)
  nelj <- grepl("^[0-9]{4}Q[1-4]$", x)
  vuosi <- grepl("^[0-9]{4}$", x)

  # Tyhjalla osajoukolla paste0 palauttaisi pituuden 1 eika nollaa, joten
  # jokainen muoto kasitellaan vain kun sita loytyy.
  if (any(kk)) {
    out[kk] <- as.Date(paste0(substr(x[kk], 1, 4), "-", substr(x[kk], 5, 6), "-01"))
  }
  if (any(nelj)) out[nelj] <- visu_period_start(sub("Q", "-Q", x[nelj]), 3L)
  if (any(vuosi)) out[vuosi] <- as.Date(paste0(x[vuosi], "-01-01"))

  out
}

visu_uljas_period_pattern <- function() "^[0-9]{4}((0[1-9]|1[0-2])|Q[1-4])?$"

# Luokitusten selitteet kuviota ja metatietoja varten. Yksi pyynto luokitusta
# kohti, ja tulos muistetaan ajon ajaksi, koska sama kuutio voi olla usean
# kuvion lahteena.
visu_uljas_labels <- function(ifile, luokitukset, lang) {
  koodit <- lapply(luokitukset, function(cl) {
    avain <- paste(ifile, lang, cl, sep = "|")
    muistissa <- the$uljas[[avain]]
    if (is.null(muistissa)) {
      muistissa <- tryCatch({
        k <- uljas::uljas_class(class = cl, ifile = ifile, lang = lang)[[1]]
        stats::setNames(as.character(k$text), as.character(k$code))
      }, error = function(e) character())
      the$uljas[[avain]] <- muistissa
    }
    muistissa
  })
  stats::setNames(koodit, luokitukset)
}

visu_is_uljas <- function(url) {
  grepl("^/DATABASE/", as.character(url))
}

# Milloin kuutio on paivitetty. Uljas kertoo sen tilastoluettelon
# utime-kentassa, joten yksi luettelopyynto kattaa kaikki kuutiot.
visu_uljas_updated <- function(ifile) {
  avain <- visu_url_key(ifile)
  muistissa <- the$uljas[[avain]]
  if (!is.null(muistissa)) return(muistissa)

  leima <- NA_character_
  if (requireNamespace("uljas", quietly = TRUE)) {
    luettelo <- tryCatch(uljas::uljas_stats(lang = "en"), error = function(e) NULL)
    leima <- visu_uljas_stamp(luettelo, avain)
  }
  the$uljas[[avain]] <- leima
  leima
}

visu_uljas_stamp <- function(luettelo, ifile) {
  if (!is.data.frame(luettelo) || !all(c("ifile", "utime") %in% names(luettelo))) {
    return(NA_character_)
  }
  osuma <- luettelo$utime[visu_url_key(luettelo$ifile) == ifile]
  if (length(osuma) != 1L || is.na(osuma)) return(NA_character_)

  # Muoto on p.k.vvvv t.mm.ss Suomen aikaa; tila kirjaa leimat UTC:na.
  aika <- as.POSIXct(as.character(osuma), format = "%d.%m.%Y %H.%M.%S",
                     tz = "Europe/Helsinki")
  if (is.na(aika)) return(NA_character_)
  format(aika, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}
