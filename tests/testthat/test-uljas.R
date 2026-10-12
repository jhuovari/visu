# Uljaksen kuutio nimetaan polulla eika osoitteella, ja jaksotunnus on
# kuusinumeroinen luku. Testit kuvaavat naita, eivat rajapintaa.

test_that("jaksotunnus luetaan jakson ensimmaiseksi paivaksi", {
  expect_equal(visu_uljas_time(c("202601", "202607", "2026Q3", "2026")),
               as.Date(c("2026-01-01", "2026-07-01", "2026-07-01", "2026-01-01")))
})

test_that("yhden muodon vektori ja tuntematon jakso kasitellaan oikein", {
  # Tyhjalla osajoukolla paste0 palauttaisi pituuden 1 eika nollaa.
  expect_equal(visu_uljas_time("200201"), as.Date("2002-01-01"))
  expect_equal(visu_uljas_time(c("202607", "roska")),
               as.Date(c("2026-07-01", NA)))
  expect_length(visu_uljas_time(character()), 0L)
})

test_that("kuukausitunnus ei sekoitu nelinumeroiseen koodiin", {
  # Kuukausi on 01-12, joten 202613 ei ole kuukausi vaan tuntematon.
  expect_true(is.na(visu_uljas_time("202613")))
  expect_true(is.na(visu_uljas_time("202600")))
})

test_that("aikasarake loytyy sisallosta eika nimesta", {
  raaka <- data.frame(
    `SITC Products` = c("7", "7"), `Time period` = c("202606", "202607"),
    Flow = c("2", "2"), values = c(1, 2),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  expect_equal(visu_uljas_time_col(raaka), "Time period")

  # Nimi voi olla toinenkin: ratkaisee se, etta kaikki arvot ovat jaksoja.
  names(raaka)[2] <- "Kuukausi"
  expect_equal(visu_uljas_time_col(raaka), "Kuukausi")
})

test_that("ilman aikasaraketta virhe kertoo mita odotettiin", {
  raaka <- data.frame(a = "7", b = "2", values = 1, stringsAsFactors = FALSE)
  expect_error(visu_uljas_time_col(raaka), "202607")
})

test_that("paivitysleima luetaan tilastoluettelosta", {
  luettelo <- data.frame(
    ifile = c("/DATABASE/01 X/ULJAS_SITC", "/DATABASE/01 X/ULJAS_SITC2"),
    utime = c("25.9.2026 9.32.54", "26.8.2026 1.13.18"),
    stringsAsFactors = FALSE
  )

  # Lahde kertoo ajan Suomen aikaa, tila kirjaa UTC:na.
  expect_equal(visu_uljas_stamp(luettelo, "/DATABASE/01 X/ULJAS_SITC"),
               "2026-09-25T06:32:54Z")
  expect_true(is.na(visu_uljas_stamp(luettelo, "/DATABASE/01 X/EI_OLE")))
  expect_true(is.na(visu_uljas_stamp(NULL, "/DATABASE/01 X/ULJAS_SITC")))
})

test_that("tuoreustarkistus tunnistaa Uljaksen kuution", {
  expect_true(visu_is_uljas("/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC"))
  expect_false(visu_is_uljas("https://www.cpb.nl/en/worldtrademonitor/latest"))
  expect_false(visu_is_uljas("https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/khi/15b5.px/"))
})

test_that("leima muistetaan ajon ajaksi", {
  visu_clear_cache()
  on.exit(visu_clear_cache(), add = TRUE)
  skip_if_not_installed("uljas")
  kerrat <- 0L
  testthat::local_mocked_bindings(
    visu_uljas_stamp = function(luettelo, ifile) {
      kerrat <<- kerrat + 1L
      "2026-09-25T06:32:54Z"
    }
  )

  a <- visu_table_updated("/DATABASE/01 X/ULJAS_SITC")
  b <- visu_table_updated("/DATABASE/01 X/ULJAS_SITC")

  expect_equal(a, b)
  expect_equal(kerrat, 1L)
})

test_that("metatiedoissa kuution nimi ja linkki tietokantaan", {
  testthat::local_mocked_bindings(
    visu_table_updated = function(...) NA_character_,
    visu_table_meta = function(...) NULL
  )
  d <- data.frame(time = as.Date("2026-07-01"), values = 1)
  ulos <- visu_metadata_one(d, "/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC")

  expect_match(ulos, "ULJAS_SITC", fixed = TRUE)
  expect_match(ulos, "tilastot.tulli.fi", fixed = TRUE)
  # Koko polku ei kuulu linkkitekstiin.
  expect_no_match(ulos, "ULKOMAANKAUPPATILASTOT", fixed = TRUE)
})
