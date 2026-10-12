# Kuvion tiedoissa oleva taululinkki vie PxWebin selausnakymaan eika
# JSON-rajapintaan, jotta linkista paasee katsomaan taulua.

test_that("api-osoite kaantyy selausnakymaksi", {
  expect_equal(
    visu:::visu_browse_url("https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/ntp/11tj.px/"),
    "https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__ntp/11tj.px/"
  )
})

test_that("kieli ja tietokanta sailyvat osoitteessa", {
  expect_equal(
    visu:::visu_browse_url("https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/khi/15b7.px/"),
    "https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__khi/15b7.px/"
  )
})

test_that("paattava kauttaviiva ei ole pakollinen", {
  expect_equal(
    visu:::visu_browse_url("https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/135y.px"),
    "https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__tyti/135y.px/"
  )
})

test_that("useampi kansiotaso erotetaan kahdella alaviivalla", {
  expect_equal(
    visu:::visu_browse_url("https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/kan/ntp/11tj.px/"),
    "https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__kan__ntp/11tj.px/"
  )
})

test_that("muut lahteet palautetaan sellaisenaan", {
  for (url in c("https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A",
                "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILBRENTEU",
                "https://www.suomenpankki.fi/api/interestrates/euribor")) {
    expect_equal(visu:::visu_browse_url(url), url)
  }
})

test_that("linkin teksti on taulun tunnus ja otsikko, osoite selausnakyma", {
  meta <- list(title = "Bruttokansantuote muuttujina Vuosineljännes ja Tiedot")

  ulos <- visu:::visu_table_link(
    meta, "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/ntp/11tj.px/")

  expect_equal(
    ulos,
    "[11tj Bruttokansantuote](https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__ntp/11tj.px)"
  )
})

test_that("luokittelusarakkeet nakyvat vaikka codes_names olisi kadonnut", {
  # dplyr ei sailyta attribuutteja, joten putken jalkeen selitteita ei ole.
  # Metatiedot kertovat silloin koodit eivatka jata rivia kokonaan pois.
  d <- data.frame(
    time = as.Date(c("2026-01-01", "2026-01-01")),
    sarja = factor(c("tgz_w1_qnmi_sn", "mgz_i1_qnmi_sn")),
    values = c(116.8, 110.5)
  )

  expect_equal(visu_class_cols(d), "sarja")

  testthat::local_mocked_bindings(
    visu_table_meta = function(...) NULL,
    visu_table_updated = function(...) NA_character_
  )
  ulos <- visu_metadata_one(d, "https://www.cpb.nl/en/worldtrademonitor/latest")

  expect_match(ulos, "tgz_w1_qnmi_sn")
  expect_match(ulos, "mgz_i1_qnmi_sn")
})

test_that("selitteet voittavat koodit kun codes_names on tallella", {
  d <- data.frame(
    time = as.Date("2026-01-01"),
    sarja = factor("tgz_w1_qnmi_sn"),
    values = 116.8
  )
  attr(d, "codes_names") <- list(sarja = c(tgz_w1_qnmi_sn = "World trade"))

  testthat::local_mocked_bindings(
    visu_table_meta = function(...) NULL,
    visu_table_updated = function(...) NA_character_
  )
  ulos <- visu_metadata_one(d, "https://www.cpb.nl/en/worldtrademonitor/latest")

  expect_match(ulos, "World trade")
  expect_no_match(ulos, "tgz_w1_qnmi_sn")
})
