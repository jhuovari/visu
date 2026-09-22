#' Kausitasoita aikasarja
#'
#' Tasoittaa sarjan kausivaihtelusta X-13ARIMA-SEATS-menetelmällä
#' ([seasonal::seas()]) ja lisää dataan kaksi saraketta: `kausi` on
#' kausitasoitettu sarja ja `trendi` siitä laskettu trendi. Menetelmä on sama
#' kuin Tilastokeskuksella, joten omat sarjat ovat samanmuotoisia kuin
#' tilaston valmiiksi tasoittamat.
#'
#' Poikkeamista korjataan oletuksena vain yksittäiset havainnot
#' (`outlier_types = "ao"`). Tasosiirtymää ei haluta kausitasoitettuun
#' sarjaan: jos menetelmä saa poistaa tasohyppäyksen, kuviosta katoaa juuri se
#' muutos, jota siitä luetaan.
#'
#' Malli kiinnitetään. Ensimmäisellä kerralla menetelmä valitsee mallin itse,
#' ja valinta kirjataan tiedostoon `site/_visu_seasonal.json`. Sen jälkeen
#' samaa mallia käytetään uudelleen ja vain sen parametrit estimoidaan joka
#' ajolla — sama periaate kuin Tilastokeskuksella. Malli valitaan uudelleen,
#' kun kirjauksesta on vuosi; kiinnittäminen tekee ajosta myös selvästi
#' nopeamman. Mallitiedosto on versionhallinnassa, joten valinnat näkyvät ja
#' ne voi tarkistaa.
#'
#' Tulos ei ole sama kuin tilaston oma kausitasoitus, koska mallit ja
#' korjaukset valitaan eri tavalla. Työllisten 15-74-vuotiaiden sarjalla, josta
#' Tilastokeskus julkaisee myös oman tasoituksensa, ero on keskimäärin
#' 8 000 henkeä eli noin 0,3 prosenttia tasosta; paketin testit valvovat, ettei
#' ero kasva. Kerro kuviossa, että kausitasoitus on tehty sivustolla itse.
#'
#' @param data Data frame, jossa on aikasarake ja arvot.
#' @param id Sarjan tunnus mallitiedostossa, esim. `"tyti-135y-tyolliset"`.
#'   Yhdessä `by`-sarakkeiden arvojen kanssa se yksilöi mallin, joten pidä se
#'   samana ajosta toiseen — muuttunut tunnus valitsee mallin uudelleen.
#' @param by Ryhmittelysarakkeet merkkijonovektorina. Jokainen ryhmä
#'   tasoitetaan erikseen, koska kausikuvio on eri esimerkiksi eri
#'   ikäluokissa.
#' @param x Aikasarake. Oletuksena datan ensimmäinen sarake. Kuukausi- ja
#'   neljännesvuosisarjat tunnistetaan havaintojen väliltä.
#' @param y Arvosarake. Oletuksena `values`.
#' @param outlier_types Korjattavat poikkeamatyypit, ks. yllä. `NULL` jättää
#'   poikkeamahaun kokonaan pois.
#' @param site_dir Sivuston hakemisto. Oletuksena etsitään `_quarto.yml`:n
#'   perusteella, jolloin funktio toimii sekä koodilohkosta että
#'   repositorion juuresta.
#' @return `data` sarakkeilla `kausi` ja `trendi`.
#' @examples
#' \dontrun{
#' visu_get_data(url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/135y.px/",
#'               query = list(timeperiod_m = "*", contentscode = "tyti-Tyolliset")) |>
#'   visu_seasonal("tyti-135y-tyolliset", by = "ikaryhma_19_20190101")
#' }
#' @export
visu_seasonal <- function(data, id, by = NULL, x = NULL, y = NULL,
                          outlier_types = "ao", site_dir = NULL) {
  if (!requireNamespace("seasonal", quietly = TRUE)) {
    stop("Paketti seasonal tarvitaan kausitasoitukseen.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` pit\u00e4\u00e4 olla data frame, ei ", class(data)[1], ".", call. = FALSE)
  }
  if (missing(id) || !is.character(id) || length(id) != 1L || !nzchar(id)) {
    stop("`id` pit\u00e4\u00e4 olla yksi merkkijono, joka yksil\u00f6i sarjan ",
         "mallitiedostossa.", call. = FALSE)
  }

  x <- x %||% names(data)[1]
  y <- y %||% visu_value_col(data)
  visu_require_col(data, x, "x")
  visu_require_col(data, y, "y")
  for (sarake in by) visu_require_col(data, sarake, "by")

  mallit <- visu_seasonal_read(site_dir)
  data$kausi <- NA_real_
  data$trendi <- NA_real_

  ryhmat <- visu_seasonal_groups(data, by)
  avaimet <- names(ryhmat)
  # Silmukka kulkee paikan mukaan, koska ryhmittelemattoman datan avain on
  # tyhja merkkijono eika kelpaa listan nimeksi.
  for (k in seq_along(ryhmat)) {
    rivit <- ryhmat[[k]]
    rivit <- rivit[order(data[[x]][rivit])]
    tunnus <- if (nzchar(avaimet[k])) paste(id, avaimet[k], sep = "|") else id

    sovite <- visu_seasonal_fit(
      visu_seasonal_ts(data[[x]][rivit], data[[y]][rivit], tunnus),
      mallit[[tunnus]], outlier_types, tunnus
    )
    mallit[[tunnus]] <- sovite$malli
    data$kausi[rivit] <- sovite$kausi
    data$trendi[rivit] <- sovite$trendi
  }

  visu_seasonal_write(mallit, site_dir)
  data
}

# Ryhmien riviosoitteet. Ilman ryhmittelya koko data on yksi sarja, ja tyhja
# nimi kertoo kutsujalle, ettei avaimeen tule ryhmaosaa.
visu_seasonal_groups <- function(data, by) {
  if (is.null(by)) return(stats::setNames(list(seq_len(nrow(data))), ""))
  avaimet <- lapply(by, function(sarake) as.character(data[[sarake]]))
  split(seq_len(nrow(data)), avaimet, drop = TRUE, sep = "|")
}

# Sovitus kiinnitetylla mallilla, tai mallin valinta kun sita ei viela ole tai
# se on vanha. Kiinnitetty sovitus voi kaatua, jos data on muuttunut paljon;
# silloin malli valitaan uudelleen eika sivuston rakentaminen jaa kiinni
# vuoden takaiseen valintaan.
visu_seasonal_fit <- function(tsd, malli, outlier_types, tunnus) {
  if (!visu_seasonal_stale(malli)) {
    m <- tryCatch(visu_seasonal_fixed(tsd, malli), error = function(e) e)
    if (!inherits(m, "error")) return(visu_seasonal_result(m, malli))
    warning("Kiinnitetty malli ", malli$malli, " ei sopinut sarjaan ", tunnus,
            " (", conditionMessage(m), "). Malli valitaan uudelleen.",
            call. = FALSE)
  }
  m <- seasonal::seas(tsd, outlier.types = outlier_types)
  visu_seasonal_result(m, visu_seasonal_spec(m))
}

# Kiinnitetty sovitus. Mallin lisaksi kiinnitetaan muunnos ja regressorit:
# regressorit sisaltavat seka loydetyt poikkeamat etta menetelman itse
# valitsemat kalenterimuuttujat, ja aictest suljetaan pois, jottei valinta
# tehdy uudelleen.
visu_seasonal_fixed <- function(tsd, malli) {
  regressorit <- as.character(unlist(malli$regressorit, use.names = FALSE))
  seasonal::seas(
    tsd,
    arima.model = malli$malli,
    transform.function = malli$muunnos,
    regression.variables = if (length(regressorit)) regressorit else NULL,
    regression.aictest = NULL,
    outlier = NULL,
    automdl = NULL
  )
}

visu_seasonal_result <- function(m, malli) {
  list(malli = malli,
       kausi = as.numeric(seasonal::final(m)),
       trendi = as.numeric(seasonal::trend(m)))
}

# Mallin kirjaus tiedostoon. Valintapaiva kertoo, milloin malli on viimeksi
# tarkistettu, ja se ohjaa vuosittaisen uudelleenvalinnan.
visu_seasonal_spec <- function(m) {
  list(
    valittu = format(Sys.Date()),
    malli = m$model$arima$model,
    muunnos = seasonal::transformfunction(m),
    regressorit = as.character(m$model$regression$variables)
  )
}

visu_seasonal_stale <- function(malli, max_age = NULL) {
  if (!is.list(malli) || is.null(malli$malli) || is.null(malli$valittu)) return(TRUE)
  raja <- max_age %||% getOption("visu.seasonal_max_age", 365)
  valittu <- suppressWarnings(as.Date(as.character(malli$valittu)))
  is.na(valittu) || as.numeric(Sys.Date() - valittu) > raja
}

# Aikasarake ts-objektiksi. Havaintovali paattelee tiheyden, ja tasainen vali
# takaa samalla, ettei sarjassa ole aukkoja: X-13 lukee ts-objektia pelkkana
# lukujonona eika huomaisi puuttuvaa kuukautta.
visu_seasonal_ts <- function(aika, arvot, tunnus) {
  aika <- as.Date(aika)
  if (anyNA(aika)) {
    stop("Sarjan ", tunnus, " aikasarake ei ole p\u00e4iv\u00e4m\u00e4\u00e4r\u00e4.",
         call. = FALSE)
  }
  tiheys <- visu_seasonal_freq(aika, tunnus)
  if (length(arvot) < 3L * tiheys) {
    stop("Sarjassa ", tunnus, " on ", length(arvot), " havaintoa. ",
         "Kausitasoitus vaatii v\u00e4hint\u00e4\u00e4n kolme vuotta.", call. = FALSE)
  }
  if (anyNA(arvot)) {
    stop("Sarjassa ", tunnus, " on puuttuvia arvoja.", call. = FALSE)
  }
  askel <- 12L / tiheys
  stats::ts(
    as.numeric(arvot),
    start = c(as.integer(format(aika[1], "%Y")),
              (as.integer(format(aika[1], "%m")) - 1L) %/% askel + 1L),
    frequency = tiheys
  )
}

visu_seasonal_freq <- function(aika, tunnus) {
  if (length(aika) < 2L) {
    stop("Sarjassa ", tunnus, " on liian v\u00e4h\u00e4n havaintoja.", call. = FALSE)
  }
  valit <- as.numeric(diff(aika))
  if (all(valit >= 28 & valit <= 31)) return(12L)
  if (all(valit >= 89 & valit <= 92)) return(4L)
  stop("Sarjan ", tunnus, " havaintov\u00e4li ei ole kuukausi eik\u00e4 ",
       "nelj\u00e4nnesvuosi, tai sarjassa on aukko.", call. = FALSE)
}

# Mallitiedosto on sivuston juuressa _visu_state.jsonin rinnalla. Koodilohkot
# ajetaan site/kuviot-hakemistossa ja visu_update_site() repositorion
# juuresta, joten juuri etsitaan _quarto.yml:n perusteella.
visu_seasonal_dir <- function(site_dir = NULL) {
  if (!is.null(site_dir)) return(site_dir)
  ehdot <- c(getOption("visu.site_dir", "site"), "..", ".", "../..")
  for (dir in ehdot) {
    if (file.exists(file.path(dir, "_quarto.yml"))) return(dir)
  }
  stop("Sivuston hakemistoa ei l\u00f6ydy (_quarto.yml). Anna `site_dir`.",
       call. = FALSE)
}

visu_seasonal_path <- function(site_dir = NULL) {
  file.path(visu_seasonal_dir(site_dir), "_visu_seasonal.json")
}

visu_seasonal_read <- function(site_dir = NULL) {
  path <- visu_seasonal_path(site_dir)
  if (!file.exists(path)) return(list())
  mallit <- jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  if (!is.list(mallit)) list() else mallit
}

visu_seasonal_write <- function(mallit, site_dir = NULL) {
  path <- visu_seasonal_path(site_dir)
  mallit <- mallit[order(names(mallit))]
  jsonlite::write_json(mallit, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}
