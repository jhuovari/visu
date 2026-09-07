#' Hae StatFin-taulu rajapinnasta
#'
#' Kuori `pxwebtools::pxw_get_data()`:n ympärille, joka tekee kaksi asiaa,
#' joita koko sivuston rakentaminen yhdellä ajolla vaatii: pitää pyyntötahdin
#' StatFinin rajoissa ja yrittää uudelleen kun rajapinta silti vastaa
#' rajoituksella.
#'
#' StatFin rajoittaa pyyntöjen määrää aikaikkunassa ja vastaa ylityksestä
#' koodilla 429. Rajoitus jää päälle hetkeksi ylityksen jälkeen, ja silloin
#' myös taulun metatietopyyntö palauttaa jotain muuta kuin PxWeb-konfiguraation
#' — `pxweb` kertoo siitä virheellä "This is not a PXWEB API". Molemmat
#' tunnistetaan rajoitukseksi ja odotetaan pidentyvä hetki ennen uutta
#' yritystä.
#'
#' Tahdinpito on ajokohtainen: paketti muistaa tehdyt pyynnöt ja odottaa itse,
#' jos ikkunaan ei mahdu enempää. Yksi datahaku on kaksi pyyntöä, koska
#' `pxweb` lukee ensin taulun konfiguraation ja tekee vasta sitten kyselyn.
#'
#' @param url Taulun API-osoite, esim.
#'   `"https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/135z.px/"`.
#' @param query Kyselylista muuttujakoodeilla, kuten
#'   `pxwebtools::pxw_get_data()`:ssa.
#' @param ... Muut argumentit `pxwebtools::pxw_get_data()`:lle.
#' @return `pxwebtools::pxw_get_data()`:n paluuarvo.
#' @examples
#' \dontrun{
#' visu_get_data(
#'   url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/135z.px/",
#'   query = list(timeperiod_m = "*", contentscode = "tyti-Tyottomyysaste")
#' )
#' }
#' @export
visu_get_data <- function(url, query = NULL, ...) {
  if (!requireNamespace("pxwebtools", quietly = TRUE)) {
    stop("Paketti pxwebtools tarvitaan datan hakuun.", call. = FALSE)
  }

  yritykset <- visu_px_tries()
  for (yritys in seq_len(yritykset)) {
    # Kaksi pyyntoa: taulun konfiguraatio ja itse kysely.
    visu_px_throttle(2L)
    tulos <- tryCatch(
      pxwebtools::pxw_get_data(url = url, query = query, ...),
      error = function(e) e
    )
    if (!inherits(tulos, "error")) return(tulos)
    if (yritys >= yritykset || !visu_px_limited(tulos)) stop(tulos)
    visu_px_wait(yritys, yritykset, conditionMessage(tulos))
  }
}

# Rajapinnan tahdinpito. Ikkuna pidetaan StatFinin rajaa loivempana, koska
# pyyntoja tulee myos tuoreustarkistuksesta ja metatiedoista, ja koska
# ajurin IP on GitHubin jakama.
visu_px_throttle <- function(hinta = 1L) {
  raja <- getOption("visu.px_max", 20L)
  ikkuna <- getOption("visu.px_window", 10)
  repeat {
    nyt <- as.numeric(Sys.time())
    the$px_calls <- the$px_calls[the$px_calls > nyt - ikkuna]
    if (length(the$px_calls) + hinta <= raja) break
    Sys.sleep(max(0.1, min(the$px_calls) + ikkuna - nyt))
  }
  the$px_calls <- c(the$px_calls, rep(as.numeric(Sys.time()), hinta))
  invisible(NULL)
}

# Onko virhe rajapinnan rajoitus? "This is not a PXWEB API" tulee myos
# vaarasta osoitteesta, mutta osoitteet tarkistaa visu_check_charts(), ja
# turha uudelleenyritys maksaa vain odotuksen.
visu_px_limited <- function(e) {
  msg <- paste(conditionMessage(e), collapse = " ")
  grepl("429|too many requests|not a PXWEB API", msg, ignore.case = TRUE)
}

# Odotus rajoituksen jalkeen. Ikkuna tyhjennetaan, koska rajoituksen aikana
# tehdyt pyynnot eivat kerro enaa mitaan jaljella olevasta kvootista.
visu_px_wait <- function(yritys, yritykset, msg) {
  odota <- visu_px_backoff(yritys)
  message(
    "StatFin rajoitti pyynt\u00f6j\u00e4 (", msg, "). Odotetaan ", odota,
    " s ja yritet\u00e4\u00e4n uudelleen (", yritys + 1L, "/", yritykset, ")."
  )
  Sys.sleep(odota)
  the$px_calls <- numeric()
  invisible(NULL)
}

visu_px_tries <- function() {
  as.integer(getOption("visu.px_tries", 4L))
}

visu_px_backoff <- function(yritys) {
  portaat <- getOption("visu.px_backoff", c(20, 45, 90))
  portaat[min(as.integer(yritys), length(portaat))]
}

# Rajapinnan JSON-haku samalla tahdinpidolla ja uudelleenyrityksilla kuin
# datahaku. Palauttaa NULL kun tietoa ei saada, koska kutsujat (kansiolistaus
# ja taulun metatiedot) tayttavat puuttuvan tiedon muuten.
visu_px_json <- function(url, mita = "Tietoja") {
  yritykset <- visu_px_tries()
  for (yritys in seq_len(yritykset)) {
    visu_px_throttle(1L)
    tulos <- tryCatch(
      jsonlite::fromJSON(url, simplifyDataFrame = TRUE),
      error = function(e) e
    )
    if (!inherits(tulos, "error")) return(tulos)
    if (yritys >= yritykset || !visu_px_limited(tulos)) {
      warning(mita, " ei saatu osoitteesta ", url, ": ",
              conditionMessage(tulos), call. = FALSE)
      return(NULL)
    }
    visu_px_wait(yritys, yritykset, conditionMessage(tulos))
  }
  NULL
}
