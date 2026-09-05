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
