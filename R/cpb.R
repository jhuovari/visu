#' Hae CPB:n maailmankauppamittari
#'
#' Lukee CPB World Trade Monitorin, joka julkaistaan vain Excel-tiedostona
#' CPB:n verkkosivuilla. Osoitteeksi annetaan julkaisusivu — `.../latest`
#' ohjaa aina tuoreimpaan kuukausijulkaisuun — ja funktio etsii sivulta
#' työkirjan osoitteen itse. Tiedoston nimessä on julkaisukuukausi, joten
#' suoraan tiedostoon osoittava linkki vanhenisi kuukaudessa.
#'
#' Sarjat tunnistetaan CPB:n omalla koodilla, joka on työkirjassa nimen
#' vieressä. Koodi kertoo muuttujan, alueen ja mittarin: esimerkiksi
#' `mgz_i1_qnmi_sn` on kehittyneiden talouksien tuonnin volyymi-indeksi
#' kausitasoitettuna ja `mgz_i1_pdmi_sn` saman erän hintaindeksi. Nimi on
#' työkirjassa sisennetty hierarkian mukaan, joten sama nimi voi toistua eri
#' osioissa — koodi on se, joka yksilöi sarjan.
#'
#' Paluuarvo on samassa muodossa kuin [visu_get_data()]:lla: aika ensin,
#' luokittelusarake `sarja` keskellä ja arvot sarakkeessa `values`.
#'
#' @param url Julkaisusivun osoite, esim.
#'   `"https://www.cpb.nl/en/worldtrademonitor/latest"`. Myös suora
#'   `.xlsx`-osoite kelpaa.
#' @param codes Haettavien sarjojen koodit. Oletuksena `NULL`, jolloin
#'   palautetaan välilehden kaikki sarjat.
#' @param sheet Työkirjan välilehti. Oletuksena `"trade_out"`, jolla on
#'   maailmankaupan volyymit ja hinnat; `"inpro_out"` on teollisuustuotanto.
#' @return Data frame, jossa `time` on `Date` ja `values` numeerinen.
#' @examples
#' \dontrun{
#' visu_get_cpb("https://www.cpb.nl/en/worldtrademonitor/latest",
#'              codes = c("tgz_w1_qnmi_sn", "mgz_i1_qnmi_sn"))
#' }
#' @export
visu_get_cpb <- function(url, codes = NULL, sheet = "trade_out") {
  tiedosto <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tiedosto), add = TRUE)
  visu_fetch_file(visu_cpb_file_url(url), tiedosto)

  taulu <- visu_cpb_read(tiedosto, sheet)
  paikka <- visu_cpb_layout(taulu, sheet)

  koodit <- trimws(as.character(taulu[[paikka$code]]))
  nimet <- trimws(as.character(taulu[[paikka$label]]))
  rivit <- which(!is.na(koodit) & grepl(visu_cpb_code_pattern(), koodit))

  if (!is.null(codes)) {
    puuttuu <- setdiff(codes, koodit[rivit])
    if (length(puuttuu) > 0L) {
      stop("Välilehdellä '", sheet, "' ei ole sarjoja: ",
           paste(puuttuu, collapse = ", "), call. = FALSE)
    }
    rivit <- rivit[koodit[rivit] %in% codes]
  }
  if (length(rivit) == 0L) {
    stop("Välilehdeltä '", sheet, "' ei löytynyt yhtään sarjaa.",
         call. = FALSE)
  }

  sarakkeet <- seq(paikka$first, ncol(taulu))
  aika <- visu_cpb_time(as.character(unlist(taulu[paikka$header, sarakkeet],
                                            use.names = FALSE)))

  osat <- lapply(rivit, function(i) {
    arvot <- suppressWarnings(as.numeric(unlist(taulu[i, sarakkeet],
                                                use.names = FALSE)))
    data.frame(time = aika, sarja = koodit[i], values = arvot,
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, osat)
  d <- d[!is.na(d$time) & !is.na(d$values), , drop = FALSE]
  d$sarja <- factor(d$sarja, levels = koodit[rivit])
  d <- d[order(d$sarja, d$time), , drop = FALSE]
  rownames(d) <- NULL

  # Selitteiksi tyokirjan omat nimet, jotta visu_metadata() kertoo sarjan
  # nimen eika pelkkaa koodia.
  attr(d, "codes_names") <- list(
    sarja = stats::setNames(nimet[rivit], koodit[rivit])
  )
  d
}

# Tyokirjan osoite. Julkaisusivulta se haetaan ensimmaisesta xlsx-linkista;
# suora tiedosto-osoite kelpaa sellaisenaan.
visu_cpb_file_url <- function(url) {
  if (grepl("\\.xlsx?($|[?#])", url, ignore.case = TRUE)) return(url)

  sivu <- paste(visu_fetch_text(url), collapse = "\n")
  osumat <- regmatches(
    sivu, gregexpr("href=\"[^\"]+\\.xlsx?\"", sivu, ignore.case = TRUE)
  )[[1]]
  if (length(osumat) == 0L) {
    stop("Sivulta ", url, " ei löytynyt Excel-linkkiä. Tarkista osoite tai ",
         "anna työkirjan osoite suoraan.", call. = FALSE)
  }
  visu_absolute_url(sub("\"$", "", sub("^href=\"", "", osumat[1])), url)
}

# Suhteellinen osoite absoluuttiseksi sivun osoitteen suhteen.
visu_absolute_url <- function(href, base) {
  if (grepl("^https?://", href)) return(href)
  if (startsWith(href, "//")) return(paste0(sub("^(https?:).*", "\\1", base), href))
  juuri <- sub("^(https?://[^/]+).*", "\\1", base)
  if (startsWith(href, "/")) return(paste0(juuri, href))
  paste0(sub("[^/]+$", "", base), href)
}

visu_cpb_read <- function(path, sheet) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Paketti readxl tarvitaan CPB:n työkirjan lukemiseen.", call. = FALSE)
  }
  # Kaikki tekstina: tyokirjassa on samassa sarakkeessa seka nimia etta
  # lukuja, ja tyyppien arvailu tekisi lukemisesta sisallosta riippuvaa.
  as.data.frame(
    readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                       col_types = "text", .name_repair = "minimal"),
    stringsAsFactors = FALSE
  )
}

# Tyokirjan rakenne: otsikkorivi, ensimmainen havaintosarake seka koodi- ja
# nimisarake. Paikat etsitaan sisallosta eika kiinniteta numeroina, koska
# tyhjat reunasarakkeet ja osioiden maara vaihtelevat julkaisusta toiseen.
visu_cpb_layout <- function(taulu, sheet) {
  if (nrow(taulu) == 0L || ncol(taulu) == 0L) {
    stop("Välilehti '", sheet, "' on tyhjä.", call. = FALSE)
  }

  osuu <- vapply(taulu, function(x) {
    grepl(visu_cpb_period_pattern(), trimws(as.character(x)))
  }, logical(nrow(taulu)))
  dim(osuu) <- c(nrow(taulu), ncol(taulu))
  if (!any(osuu)) {
    stop("Välilehdeltä '", sheet, "' ei löytynyt jaksotunnuksia, ",
         "esimerkiksi '2026m07'. Onko työkirjan muoto muuttunut?",
         call. = FALSE)
  }
  paikat <- which(osuu, arr.ind = TRUE)
  header <- min(paikat[, "row"])
  first <- min(paikat[paikat[, "row"] == header, "col"])

  # Koodisarake on se otsikkorivin vasemmalta puolelta, jossa koodeja on
  # eniten; nimi on sen vieressa vasemmalla.
  vasen <- seq_len(first - 1L)
  if (length(vasen) < 2L) {
    stop("Välilehdellä '", sheet, "' ei ole nimi- ja koodisaraketta ",
         "havaintojen vasemmalla puolella.", call. = FALSE)
  }
  osumat <- vapply(vasen, function(j) {
    sum(grepl(visu_cpb_code_pattern(), trimws(as.character(taulu[[j]]))))
  }, integer(1))
  if (max(osumat) == 0L) {
    stop("Välilehdeltä '", sheet, "' ei löytynyt sarjakoodeja, ",
         "esimerkiksi 'mgz_i1_qnmi_sn'.", call. = FALSE)
  }
  code <- vasen[which.max(osumat)]
  if (code < 2L) {
    stop("Välilehdellä '", sheet, "' koodisarake on ensimmäisenä, ",
         "joten nimisaraketta ei ole.", call. = FALSE)
  }

  list(header = header, first = first, code = code, label = code - 1L)
}

# Jaksotunnus jakson ensimmaiseksi paivaksi: 2026m07 ja 2026q3.
visu_cpb_time <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))

  kk <- grepl("^[0-9]{4}m[0-9]{2}$", x)
  nelj <- grepl("^[0-9]{4}q[1-4]$", x)

  # Tyhjalla osajoukolla paste0 palauttaisi pituuden 1 eika nollaa, joten
  # kumpikin muoto kasitellaan vain kun sita loytyy.
  if (any(kk)) {
    out[kk] <- as.Date(paste0(substr(x[kk], 1, 4), "-", substr(x[kk], 6, 7), "-01"))
  }
  if (any(nelj)) {
    out[nelj] <- visu_period_start(sub("q", "-Q", x[nelj]), 3L)
  }
  out
}

visu_cpb_period_pattern <- function() "^[0-9]{4}(m[0-9]{2}|q[1-4])$"

# CPB:n sarjakoodi: muuttuja, alue, mittari ja kausitasoitus alaviivoin.
visu_cpb_code_pattern <- function() "^[a-z]{3}_[a-z0-9]{2}_[a-z0-9]{4}_[a-z]{2}$"

visu_is_cpb <- function(url) {
  grepl("^https?://[^/]*cpb\\.nl/", as.character(url))
}

# Milloin tyokirja on julkaistu. Julkaisusivun oma Last-Modified muuttuu myos
# sivua muokatessa, joten leima luetaan tyokirjasta: se vaihtuu tasan silloin
# kun uusi kuukausijulkaisu ilmestyy. Tulos muistetaan ajon ajaksi, koska
# osoitteen selvittaminen maksaa sivun haun.
visu_cpb_updated <- function(url) {
  avain <- visu_url_key(url)
  muistissa <- the$cpb[[avain]]
  if (!is.null(muistissa)) return(muistissa)

  leima <- tryCatch(visu_http_updated(visu_cpb_file_url(url)),
                    error = function(e) NA_character_)
  the$cpb[[avain]] <- leima
  leima
}
