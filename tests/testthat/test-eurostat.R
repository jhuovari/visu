# Eurostatin SDMX-csv: aikajakso on jakson tunnus eika paivamaara, ja
# paivitysleima tulee datan mukana omassa sarakkeessaan.

eurostat_csv <- function(aika = c("2026-01", "2026-02"), geo = "FI",
                         arvot = c(1.5, 1.7), leima = "06/02/26 23:00:00") {
  data.frame(
    DATAFLOW = "ESTAT:PRC_HICP_MANR(1.0)",
    `LAST UPDATE` = leima,
    freq = "M", unit = "RCH_A", coicop = "CP00", geo = geo,
    TIME_PERIOD = aika, OBS_VALUE = arvot, OBS_FLAG = "", CONF_STATUS = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that("jaksotunnus luetaan jakson ensimmaiseksi paivaksi", {
  expect_equal(
    visu_eurostat_time(c("2026", "2026-Q3", "2026-S2", "2026-02", "2026-02-14")),
    as.Date(c("2026-01-01", "2026-07-01", "2026-07-01", "2026-02-01", "2026-02-14"))
  )
})

test_that("tuntematon jakso jaa NA:ksi eika kaada muunnosta", {
  expect_equal(visu_eurostat_time(c("2026-02", "roska")),
               as.Date(c("2026-02-01", NA)))
})

test_that("yhden muodon vektori ei sekoita muita muotoja", {
  # Tyhjalla osajoukolla paste0 palauttaa pituuden 1 eika nollaa, joten
  # muunnos yritti kerran lukea katkelman "-01-01" paivamaaraksi.
  expect_equal(visu_eurostat_time("2000-12"), as.Date("2000-12-01"))
  expect_equal(visu_eurostat_time("2000"), as.Date("2000-01-01"))
  expect_length(visu_eurostat_time(character()), 0L)
})

test_that("useamman arvon ulottuvuus erottaa sarjat, yhden arvon jaa pois", {
  testthat::local_mocked_bindings(
    visu_fetch_csv = function(...) eurostat_csv(
      aika = rep(c("2026-01", "2026-02"), 2),
      geo = rep(c("FI", "EA20"), each = 2),
      arvot = c(1.5, 1.7, 2.1, 2.3)
    )
  )

  d <- visu_get_eurostat("https://ec.europa.eu/eurostat/api/x/y")

  expect_equal(names(d), c("time", "geo", "values"))
  expect_setequal(levels(d$geo), c("FI", "EA20"))
  expect_s3_class(d$time, "Date")
})

test_that("odottamaton vastaus kaataa selvalla viestilla", {
  testthat::local_mocked_bindings(
    visu_fetch_csv = function(...) data.frame(jotain = 1)
  )
  expect_error(visu_get_eurostat("https://ec.europa.eu/eurostat/api/x/y"),
               "SDMX-csv-muotoa")
})

test_that("paivitysleima luetaan LAST UPDATE -sarakkeesta", {
  # Lahde kertoo ajan Keski-Euroopan aikaa, tila kirjaa UTC:na.
  expect_equal(visu_eurostat_stamp(eurostat_csv()), "2026-02-06T22:00:00Z")
  expect_true(is.na(visu_eurostat_stamp(eurostat_csv(leima = "roska"))))
  expect_true(is.na(visu_eurostat_stamp(NULL)))
})

test_that("tuoreustarkistus tunnistaa Eurostatin osoitteen", {
  expect_true(visu_is_eurostat("https://ec.europa.eu/eurostat/api/x"))
  expect_false(visu_is_eurostat("https://data-api.ecb.europa.eu/service/data/EXR/D"))
  expect_false(visu_is_eurostat("https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/khi/15b5.px/"))
})

test_that("leima muistetaan ajon ajaksi, jotta sama osoite haetaan kerran", {
  visu_clear_cache()
  on.exit(visu_clear_cache(), add = TRUE)
  kerrat <- 0L
  testthat::local_mocked_bindings(
    visu_fetch_csv = function(...) {
      kerrat <<- kerrat + 1L
      eurostat_csv()
    }
  )

  a <- visu_table_updated("https://ec.europa.eu/eurostat/api/x/y")
  b <- visu_table_updated("https://ec.europa.eu/eurostat/api/x/y/")

  expect_equal(a, b)
  expect_equal(kerrat, 1L)
})

test_that("muoto lisataan osoitteeseen vasta haussa", {
  expect_equal(visu_eurostat_query("https://ec.europa.eu/x"),
               "https://ec.europa.eu/x?format=SDMX-CSV")
  expect_equal(visu_eurostat_query("https://ec.europa.eu/x?a=1"),
               "https://ec.europa.eu/x?a=1&format=SDMX-CSV")
})
