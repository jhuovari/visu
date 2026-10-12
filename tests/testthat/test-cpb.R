# CPB julkaisee vain Excel-tyokirjan, jonka osoite vaihtuu kuukausittain.
# Rakenne etsitaan sisallosta, joten testit kuvaavat sita eivatka solupaikkoja.

cpb_taulu <- function(otsikkorivi = 4L, eka_sarake = 6L) {
  taulu <- as.data.frame(matrix(NA_character_, nrow = 8L, ncol = eka_sarake + 2L),
                         stringsAsFactors = FALSE)
  taulu[otsikkorivi, eka_sarake + 0:2] <- c("2026m05", "2026m06", "2026m07")
  taulu[otsikkorivi + 2L, eka_sarake - 2L] <- "World trade"
  taulu[otsikkorivi + 2L, eka_sarake - 1L] <- "tgz_w1_qnmi_sn"
  taulu[otsikkorivi + 2L, eka_sarake + 0:2] <- c("115.1", "116.0", "116.8")
  taulu[otsikkorivi + 3L, eka_sarake - 2L] <- "  Advanced economies"
  taulu[otsikkorivi + 3L, eka_sarake - 1L] <- "mgz_i1_qnmi_sn"
  taulu[otsikkorivi + 3L, eka_sarake + 0:2] <- c("109.9", "110.2", "110.5")
  taulu
}

test_that("rakenne loytyy sisallosta eika kiinteista solupaikoista", {
  for (siirto in list(c(4L, 6L), c(1L, 3L), c(6L, 9L))) {
    taulu <- cpb_taulu(siirto[1], siirto[2])
    paikka <- visu_cpb_layout(taulu, "trade_out")

    expect_equal(paikka$header, siirto[1])
    expect_equal(paikka$first, siirto[2])
    expect_equal(paikka$code, siirto[2] - 1L)
    expect_equal(paikka$label, siirto[2] - 2L)
  }
})

test_that("jaksotunnus luetaan jakson ensimmaiseksi paivaksi", {
  expect_equal(visu_cpb_time(c("2026m01", "2026m07", "2026q3")),
               as.Date(c("2026-01-01", "2026-07-01", "2026-07-01")))
})

test_that("tuntematon jakso jaa NA:ksi ja yksi muoto riittaa", {
  # Tyhjalla osajoukolla paste0 palauttaisi pituuden 1 eika nollaa.
  expect_equal(visu_cpb_time("2026q1"), as.Date("2026-01-01"))
  expect_equal(visu_cpb_time(c("2026m02", "roska")),
               as.Date(c("2026-02-01", NA)))
  expect_length(visu_cpb_time(character()), 0L)
})

test_that("ilman jaksotunnuksia virhe kertoo mita odotettiin", {
  tyhja <- as.data.frame(matrix(NA_character_, 3L, 3L), stringsAsFactors = FALSE)
  expect_error(visu_cpb_layout(tyhja, "trade_out"), "2026m07")
})

test_that("ilman sarjakoodeja virhe kertoo mita odotettiin", {
  taulu <- cpb_taulu()
  taulu[6L, 5L] <- "ei koodi"
  taulu[7L, 5L] <- "ei tamakaan"
  expect_error(visu_cpb_layout(taulu, "trade_out"), "mgz_i1_qnmi_sn")
})

test_that("tyokirjan osoite etsitaan julkaisusivulta", {
  testthat::local_mocked_bindings(
    visu_fetch_text = function(...) c(
      "<p>Lue <a href=\"/en/world-trade-monitor/kesakuu\">lisaa</a></p>",
      "<a href=\"/system/files/cpbmedia/CPB-world-trade-monitor-july-2026.xlsx\">xlsx</a>"
    )
  )

  expect_equal(
    visu_cpb_file_url("https://www.cpb.nl/en/worldtrademonitor/latest"),
    "https://www.cpb.nl/system/files/cpbmedia/CPB-world-trade-monitor-july-2026.xlsx"
  )
})

test_that("suora tyokirjan osoite kelpaa sellaisenaan", {
  # Sivua ei haeta lainkaan: mockattu haku kaataisi testin jos sita kutsuttaisiin.
  testthat::local_mocked_bindings(
    visu_fetch_text = function(...) stop("sivua ei pitaisi hakea")
  )
  u <- "https://www.cpb.nl/system/files/cpbmedia/CPB-world-trade-monitor-july-2026.xlsx"

  expect_equal(visu_cpb_file_url(u), u)
})

test_that("ilman Excel-linkkia virhe neuvoo antamaan osoitteen suoraan", {
  testthat::local_mocked_bindings(
    visu_fetch_text = function(...) "<p>ei linkkeja</p>"
  )
  expect_error(visu_cpb_file_url("https://www.cpb.nl/en/worldtrademonitor/latest"),
               "suoraan")
})

test_that("suhteellinen osoite ratkaistaan sivun osoitteen suhteen", {
  sivu <- "https://www.cpb.nl/en/world-trade-monitor/heinakuu"

  expect_equal(visu_absolute_url("https://muu.fi/a.xlsx", sivu), "https://muu.fi/a.xlsx")
  expect_equal(visu_absolute_url("//www.cpb.nl/a.xlsx", sivu), "https://www.cpb.nl/a.xlsx")
  expect_equal(visu_absolute_url("/system/a.xlsx", sivu), "https://www.cpb.nl/system/a.xlsx")
  expect_equal(visu_absolute_url("a.xlsx", sivu),
               "https://www.cpb.nl/en/world-trade-monitor/a.xlsx")
})

test_that("tuoreustarkistus tunnistaa CPB:n osoitteen", {
  expect_true(visu_is_cpb("https://www.cpb.nl/en/worldtrademonitor/latest"))
  expect_false(visu_is_cpb("https://ec.europa.eu/eurostat/api/x"))
  expect_false(visu_is_cpb("https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/khi/15b5.px/"))
})

test_that("leima luetaan tyokirjasta ja muistetaan ajon ajaksi", {
  visu_clear_cache()
  on.exit(visu_clear_cache(), add = TRUE)
  kerrat <- 0L
  testthat::local_mocked_bindings(
    visu_cpb_file_url = function(...) {
      kerrat <<- kerrat + 1L
      "https://www.cpb.nl/system/files/cpbmedia/CPB-world-trade-monitor-july-2026.xlsx"
    },
    # Argumentti otetaan vastaan nimella eika pisteilla, jotta laiska
    # evaluointi ei jata osoitteen selvittamista tekematta.
    visu_http_updated = function(url) {
      expect_match(url, "\\.xlsx$")
      "2026-09-24T08:35:39Z"
    }
  )

  a <- visu_table_updated("https://www.cpb.nl/en/worldtrademonitor/latest")
  b <- visu_table_updated("https://www.cpb.nl/en/worldtrademonitor/latest/")

  expect_equal(a, "2026-09-24T08:35:39Z")
  expect_equal(b, a)
  expect_equal(kerrat, 1L)
})
