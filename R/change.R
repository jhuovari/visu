#' Laske sarjan muutos vuodentakaisesta
#'
#' StatFin julkaisee vuosimuutoksen vain osalle sarjoista, joten se on usein
#' laskettava itse. Muutos lasketaan `lag` havainnon takaiseen, eli
#' kuukausisarjalla 12 ja neljännesvuosisarjalla 4 havaintoa taaksepäin.
#' Kuukausi- ja neljännesvuosiaineiston vuosimuutos on samalla
#' kausivaihtelusta riippumaton.
#'
#' @param x Numeerinen vektori aikajärjestyksessä.
#' @param lag Havaintojen määrä vuodessa: 12 kuukausi-, 4 neljännesvuosi- ja
#'   1 vuosisarjalle.
#' @param type `"percent"` (oletus) antaa prosenttimuutoksen ja `"diff"`
#'   erotuksen. Käytä erotusta, kun sarja on jo prosentti, kuten työttömyysaste
#'   — silloin muutos on prosenttiyksikköjä.
#' @param by Valinnainen ryhmittelevä vektori, kun `x` sisältää useita sarjoja
#'   peräkkäin. Data pitää olla järjestetty ryhmittäin ja ajan mukaan.
#' @return Numeerinen vektori, jonka `lag` ensimmäistä havaintoa ryhmää kohti
#'   ovat `NA`.
#' @examples
#' visu_change(c(100, 102, 104, 103), lag = 1)
#' visu_change(c(5.0, 5.4, 6.1), lag = 1, type = "diff")
#' @export
visu_change <- function(x, lag = 12, type = c("percent", "diff"), by = NULL) {
  type <- match.arg(type)
  if (!is.numeric(x)) {
    stop("`x` pit\u00e4\u00e4 olla numeerinen, ei ", class(x)[1], ".", call. = FALSE)
  }
  if (!is.numeric(lag) || length(lag) != 1L || is.na(lag) || lag < 1) {
    stop("`lag` pit\u00e4\u00e4 olla v\u00e4hint\u00e4\u00e4n 1.", call. = FALSE)
  }
  lag <- as.integer(lag)

  muutos <- function(v) {
    # Lyhyt sarja jaa kokonaan NA:ksi sen sijaan etta pituus muuttuisi.
    prev <- if (length(v) > lag) {
      c(rep(NA_real_, lag), utils::head(v, -lag))
    } else {
      rep(NA_real_, length(v))
    }
    if (type == "percent") 100 * (v / prev - 1) else v - prev
  }

  if (is.null(by)) muutos(x) else stats::ave(x, by, FUN = muutos)
}

#' Erien kasvukontribuutiot
#'
#' Purkaa kokonaiserän muutoksen osiensa kontribuutioihin. Kunkin erän
#' kontribuutio on sen muutos suhteessa kokonaiserän viivästettyyn tasoon,
#' joten kontribuutiot summautuvat kokonaiserän muutosprosenttiin.
#'
#' Menoerille annetaan `negate`, koska niiden kasvu pienentää kokonaiserää:
#' esimerkiksi maksettujen verojen kasvu vähentää käytettävissä olevaa tuloa.
#'
#' Erittely ei ole täydellinen, jos kaikkia eriä ei anneta. `residual` nimeää
#' jäännöserän, joka kattaa loput ja varmistaa että summa täsmää.
#'
#' @param data Pitkä data frame, jossa on aika-, sarja- ja arvosarake.
#' @param series Sarjan koodit sisältävän sarakkeen nimi.
#' @param total Kokonaiserän koodi sarakkeessa `series`.
#' @param time Aikasarakkeen nimi. Oletuksena `"time"`.
#' @param values Arvosarakkeen nimi. Oletuksena `"values"`.
#' @param lag Havaintojen määrä vuodessa: 4 neljännesvuosi- ja 12
#'   kuukausidatalle.
#' @param negate Koodit, joiden kasvu pienentää kokonaiserää.
#' @param residual Jäännöserän nimi, tai `NULL` jos jäännöstä ei haluta.
#' @return Data frame sarakkeilla `time`, `series` ja `values`, jossa arvot
#'   ovat prosenttiyksikköjä kokonaiserän muutoksesta.
#' @examples
#' d <- data.frame(
#'   time = rep(1:8, each = 2),
#'   era = rep(c("yht", "osa"), 8),
#'   values = c(rbind(100 + (1:8), 60 + (1:8)))
#' )
#' visu_contributions(d, series = "era", total = "yht", lag = 4)
#' @export
visu_contributions <- function(data, series, total, time = "time",
                               values = "values", lag = 4,
                               negate = character(), residual = NULL) {
  for (sarake in c(time, series, values)) {
    if (!sarake %in% names(data)) {
      stop("Saraketta '", sarake, "' ei ole datassa. Tarjolla: ",
           paste(names(data), collapse = ", "), call. = FALSE)
    }
  }
  koodit <- unique(as.character(data[[series]]))
  if (!total %in% koodit) {
    stop("Kokonaiserää '", total, "' ei ole sarakkeessa '", series,
         "'. Tarjolla: ", paste(koodit, collapse = ", "), call. = FALSE)
  }

  ajat <- sort(unique(data[[time]]))
  if (length(ajat) <= lag) {
    stop("Havaintoja on ", length(ajat), ", mikä ei riitä viiveelle ", lag, ".",
         call. = FALSE)
  }

  m <- matrix(NA_real_, nrow = length(ajat), ncol = length(koodit),
              dimnames = list(NULL, koodit))
  m[cbind(match(data[[time]], ajat), match(as.character(data[[series]]), koodit))] <-
    as.numeric(data[[values]])

  viivastetty <- function(x) c(rep(NA_real_, lag), utils::head(x, -lag))
  pohja <- viivastetty(m[, total])

  osat <- setdiff(koodit, total)
  kontribuutiot <- vapply(osat, function(k) {
    merkki <- if (k %in% negate) -1 else 1
    merkki * (m[, k] - viivastetty(m[, k])) / pohja * 100
  }, numeric(length(ajat)))
  kontribuutiot <- matrix(kontribuutiot, nrow = length(ajat),
                          dimnames = list(NULL, osat))

  if (!is.null(residual)) {
    kokonaismuutos <- (m[, total] - pohja) / pohja * 100
    jaannos <- kokonaismuutos - rowSums(kontribuutiot)
    kontribuutiot <- cbind(kontribuutiot, jaannos)
    colnames(kontribuutiot)[ncol(kontribuutiot)] <- residual
    osat <- c(osat, residual)
  }

  ulos <- data.frame(
    time = rep(ajat, times = length(osat)),
    series = rep(osat, each = length(ajat)),
    values = as.vector(kontribuutiot),
    stringsAsFactors = FALSE
  )
  names(ulos) <- c(time, series, values)
  ulos[!is.na(ulos[[values]]), , drop = FALSE]
}
