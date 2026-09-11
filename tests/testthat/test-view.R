test_that("oletusalku on viimeisin viidella jaollinen vuosi miinus kymmenen", {
  expect_equal(visu:::visu_default_start(as.Date("2026-09-11")), as.Date("2015-01-01"))
  expect_equal(visu:::visu_default_start(as.Date("2029-12-31")), as.Date("2015-01-01"))
  expect_equal(visu:::visu_default_start(as.Date("2030-01-01")), as.Date("2020-01-01"))
  expect_equal(visu:::visu_default_start(as.Date("2025-01-01")), as.Date("2015-01-01"))
})

test_that("oletusalku jattaa nakyviin 10-15 kokonaista vuotta", {
  vuodet <- vapply(2024:2040, function(v) {
    paiva <- as.Date(sprintf("%d-06-01", v))
    v - as.integer(format(visu:::visu_default_start(paiva), "%Y"))
  }, numeric(1))

  expect_true(all(vuodet >= 10 & vuodet <= 15))
})

test_that("rajaus jaa pois kun x ei ole aikaa", {
  d <- data.frame(ryhma = c("a", "b"), values = c(1, 2))

  expect_null(visu:::visu_view_start(d, "ryhma", NULL))
})

test_that("rajaus jaa pois kun data alkaa muutenkin rajan jalkeen", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2025-01-01")), values = c(1, 2))

  expect_null(visu:::visu_view_start(d, "time", NULL))
})

test_that("rajaus jaa pois kun se on nimenomaisesti pois", {
  d <- data.frame(time = as.Date(c("1990-01-01", "2026-01-01")), values = c(1, 2))

  expect_null(visu:::visu_view_start(d, "time", NA))
  expect_null(visu:::visu_view_start(d, "time", FALSE))
  expect_equal(visu:::visu_view_start(d, "time", as.Date("2000-01-01")),
               as.Date("2000-01-01"))
})

test_that("pitka sarja rajataan oletusalkuun", {
  d <- data.frame(time = as.Date(c("1990-01-01", "2026-01-01")), values = c(1, 2))

  expect_equal(visu:::visu_view_start(d, "time", NULL), visu:::visu_default_start())
})

test_that("data jaa kuvioon kokonaan, vain koordinaatisto rajataan", {
  d <- data.frame(
    time = seq(as.Date("1990-01-01"), as.Date("2026-01-01"), by = "year"),
    values = seq_len(37)
  )

  p <- visu_plot(d)

  expect_equal(nrow(p$data), 37L)
  expect_s3_class(p$coordinates, "CoordCartesian")
  expect_equal(p$coordinates$limits$x[1], visu:::visu_default_start())
})

test_that("y-akseli rajataan nakyvaan dataan", {
  nakyva <- data.frame(values = c(5, 9))

  expect_equal(visu:::visu_view_ylim(nakyva, "values", "line"), c(5, 9))
  # Pylvaat lahtevat nollasta, joten nolla kuuluu aina mukaan.
  expect_equal(visu:::visu_view_ylim(nakyva, "values", "col"), c(0, 9))
  # Pinotun alueen summaa ei voi paatella riviarvoista.
  expect_null(visu:::visu_view_ylim(nakyva, "values", "area"))
})

test_that("nollaviiva paatellaan nakyvasta datasta, ei koko historiasta", {
  d <- data.frame(
    time = as.Date(c("1991-01-01", "2020-01-01", "2024-01-01")),
    values = c(-5, 2, 3)
  )

  # Vain vanha havainto on nollan alapuolella, joten nakymassa viivaa ei tarvita.
  kerrokset <- vapply(visu_plot(d)$layers, function(l) class(l$geom)[1], character(1))

  expect_false("GeomHline" %in% kerrokset)
})
