#' Laske sarjan muutos vuodentakaisesta
#'
#' StatFin julkaisee vuosimuutoksen vain osalle sarjoista — trendisarjoista ei
#' lainkaan — joten se on usein laskettava itse indeksi- tai tasosarjasta.
#'
#' Tarkoitettu dplyr-putkeen, jossa ryhmittely erottaa sarjat toisistaan:
#'
#' ```
#' data |>
#'   dplyr::group_by(contentscode) |>
#'   dplyr::mutate(muutos = visu_change(values, time)) |>
#'   dplyr::ungroup()
#' ```
#'
#' Kun `time` annetaan, muutos lasketaan ajan mukaan järjestettynä ja
#' palautetaan alkuperäisessä rivijärjestyksessä. Se on putkessa olennaista:
#' `group_by()` ei järjestä rivejä, ja väärässä järjestyksessä laskettu muutos
#' olisi hiljaa väärin. Samalla havaintotiheys päätellään aikasarakkeesta,
#' joten sitä ei tarvitse muistaa oikein sarjaa kohti.
#'
#' @param x Numeerinen vektori.
#' @param time Aikasarake, josta havaintotiheys päätellään: 12 kuukausi-, 4
#'   neljännesvuosi- ja 1 vuosisarjalle. Sarjassa ei saa olla aukkoja, koska
#'   viive lasketaan havaintoina eikä päivämäärinä. Jätä pois vain, jos
#'   aikasaraketta ei ole — silloin anna `lag` ja pidä rivit aikajärjestyksessä.
#' @param lag Havaintojen määrä vuodessa. Oletuksena päätellään `time`:sta.
#' @param type `"percent"` (oletus) antaa prosenttimuutoksen ja `"diff"`
#'   erotuksen. Käytä erotusta, kun sarja on jo prosentti, kuten työttömyysaste
#'   — silloin muutos on prosenttiyksikköjä.
#' @return Numeerinen vektori samassa järjestyksessä kuin `x`. Vuoden
#'   ensimmäiset havainnot ovat `NA`, koska niille ei ole vertailukohtaa.
#' @examples
#' kk <- seq(as.Date("2024-01-01"), by = "month", length.out = 24)
#' visu_change(seq(100, 123), time = kk)
#' visu_change(c(100, 102, 104, 103), lag = 1)
#' visu_change(c(5.0, 5.4, 6.1), lag = 1, type = "diff")
#' @export
visu_change <- function(x, time = NULL, lag = NULL, type = c("percent", "diff")) {
  type <- match.arg(type)
  if (!is.numeric(x)) {
    stop("`x` pit\u00e4\u00e4 olla numeerinen, ei ", class(x)[1], ".", call. = FALSE)
  }
  if (is.null(time) && is.null(lag)) {
    stop("Anna `time`, josta havaintotiheys p\u00e4\u00e4tell\u00e4\u00e4n, tai ",
         "`lag` suoraan.", call. = FALSE)
  }

  if (!is.null(time)) {
    if (length(time) != length(x)) {
      stop("`time` ja `x` ovat eri pituisia: ", length(time), " ja ", length(x),
           ".", call. = FALSE)
    }
    lag <- lag %||% visu_freq(time)
    # Rivijarjestys ei saa vaikuttaa tulokseen, koska group_by() ei jarjesta.
    jarjestys <- order(time)
  } else {
    jarjestys <- seq_along(x)
  }

  if (!is.numeric(lag) || length(lag) != 1L || is.na(lag) || lag < 1) {
    stop("`lag` pit\u00e4\u00e4 olla v\u00e4hint\u00e4\u00e4n 1.", call. = FALSE)
  }
  lag <- as.integer(lag)

  arvot <- x[jarjestys]
  # Lyhyt sarja jaa kokonaan NA:ksi sen sijaan etta pituus muuttuisi.
  edellinen <- if (length(arvot) > lag) {
    c(rep(NA_real_, lag), utils::head(arvot, -lag))
  } else {
    rep(NA_real_, length(arvot))
  }
  muutos <- if (type == "percent") 100 * (arvot / edellinen - 1) else arvot - edellinen

  ulos <- rep(NA_real_, length(x))
  ulos[jarjestys] <- muutos
  ulos
}

# Havaintovalin tiheys vuodessa. Tasainen vali takaa samalla, ettei sarjassa
# ole aukkoja: viive lasketaan havaintoina, joten puuttuva kuukausi siirtaisi
# vertailukohdan hiljaa vaaraan kohtaan.
visu_freq <- function(time, mita = "Sarjan") {
  if (!inherits(time, c("Date", "POSIXct", "POSIXt"))) {
    stop(mita, " aikasarake pit\u00e4\u00e4 olla Date tai POSIXct, ei ",
         class(time)[1], ".", call. = FALSE)
  }
  if (length(time) < 2L) {
    stop(mita, " havaintoja on liian v\u00e4h\u00e4n tiheyden p\u00e4\u00e4ttelyyn.",
         call. = FALSE)
  }
  valit <- as.numeric(diff(sort(as.Date(time))))
  if (all(valit >= 28 & valit <= 31)) return(12L)
  if (all(valit >= 89 & valit <= 92)) return(4L)
  if (all(valit >= 365 & valit <= 366)) return(1L)
  stop(mita, " havaintov\u00e4li ei ole kuukausi, nelj\u00e4nnesvuosi eik\u00e4 ",
       "vuosi, tai sarjassa on aukko.", call. = FALSE)
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
    stop("Kokonaiser\u00e4\u00e4 '", total, "' ei ole sarakkeessa '", series,
         "'. Tarjolla: ", paste(koodit, collapse = ", "), call. = FALSE)
  }

  ajat <- sort(unique(data[[time]]))
  if (length(ajat) <= lag) {
    stop("Havaintoja on ", length(ajat), ", mik\u00e4 ei riit\u00e4 viiveelle ", lag, ".",
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
