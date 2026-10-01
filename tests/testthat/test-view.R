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
  nakyva <- data.frame(time = as.Date(c("2024-01-01", "2024-02-01")), values = c(5, 9))

  expect_equal(visu:::visu_view_ylim(nakyva, "time", "values", "line"), c(5, 9))
  # Pylvaat ja alueet lahtevat nollasta, joten nolla kuuluu aina mukaan.
  expect_equal(visu:::visu_view_ylim(nakyva, "time", "values", "col"), c(0, 9))
  expect_equal(visu:::visu_view_ylim(nakyva, "time", "values", "area"), c(0, 9))
})

test_that("pinotun kuvion raja lasketaan pylvaan summasta eika riveista", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 3),
    values = c(3, 4, -2, 1, 1, -5)
  )

  # Rivien raja olisi -5...4, mutta pylvaat yltavat 7:aan ja -5:een.
  expect_equal(visu:::visu_view_ylim(d, "time", "values", "col", stack = TRUE),
               c(-5, 7))
  expect_equal(visu:::visu_view_ylim(d, "time", "values", "area", stack = FALSE),
               c(-5, 7))
  # Vierekkaiset pylvaat eivat summaudu, joten niille riittaa rivien raja.
  expect_equal(visu:::visu_view_ylim(d, "time", "values", "col", stack = FALSE),
               c(-5, 4))
})

test_that("paallysviiva mahtuu y-akselille", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))
  viiva <- data.frame(time = d$time, values = c(1, 9))

  expect_equal(visu:::visu_view_ylim(d, "time", "values", "col", line = viiva),
               c(0, 9))
})

test_that("pinottu kuvio ei leikkaa pylvaita nakymassa", {
  d <- data.frame(
    time = rep(seq(as.Date("2020-01-01"), by = "quarter", length.out = 8), each = 2),
    era = rep(c("a", "b"), 8),
    values = rep(c(3, 4), 8)
  )

  p <- visu_plot(d, colour = "era", type = "col", stack = TRUE,
                 start = as.Date("2020-01-01"))
  rakennettu <- ggplot2::ggplot_build(p)

  expect_equal(max(rakennettu$data[[1]]$ymax), 7)
  expect_gte(rakennettu$layout$panel_params[[1]]$y.range[2], 7)
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
