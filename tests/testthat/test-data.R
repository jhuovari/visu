test_that("rajoitusvirhe tunnistetaan sen kaikista muodoista", {
  expect_true(visu:::visu_px_limited(
    simpleError("Too Many Requests (RFC 6585) (HTTP 429).")))
  expect_true(visu:::visu_px_limited(
    simpleError("\nThis is not a PXWEB API: \nhttps://pxdata.stat.fi/x.px/")))
  expect_true(visu:::visu_px_limited(
    simpleError("cannot open URL: HTTP status was '429 Too Many Requests'")))
})

test_that("muu virhe ei ole rajoitus", {
  expect_false(visu:::visu_px_limited(simpleError("Bad Request (HTTP 400).")))
  expect_false(visu:::visu_px_limited(simpleError("Tuntematon muuttujakoodi.")))
})

test_that("odotus pitenee yrityksittain ja pysahtyy viimeiseen portaaseen", {
  expect_equal(visu:::visu_px_backoff(1), 20)
  expect_equal(visu:::visu_px_backoff(2), 45)
  expect_equal(visu:::visu_px_backoff(3), 90)
  expect_equal(visu:::visu_px_backoff(9), 90)
})

test_that("ikkunaan mahtuvat pyynnot eivat odota", {
  withr_max <- getOption("visu.px_max")
  options(visu.px_max = 5L)
  on.exit(options(visu.px_max = withr_max), add = TRUE)
  visu_clear_cache()

  kesto <- system.time(for (i in 1:5) visu:::visu_px_throttle(1L))[["elapsed"]]

  expect_lt(kesto, 1)
  expect_length(visu:::the$px_calls, 5L)
})

test_that("ikkunan taytyttya odotetaan sen verran etta vanhin vanhenee", {
  vanha_max <- getOption("visu.px_max")
  vanha_ikkuna <- getOption("visu.px_window")
  options(visu.px_max = 2L, visu.px_window = 1)
  on.exit(options(visu.px_max = vanha_max, visu.px_window = vanha_ikkuna),
          add = TRUE)
  visu_clear_cache()

  visu:::visu_px_throttle(1L)
  visu:::visu_px_throttle(1L)
  kesto <- system.time(visu:::visu_px_throttle(1L))[["elapsed"]]

  expect_gt(kesto, 0.5)
  # Vanhentuneet aikaleimat siivotaan pois, joten ikkuna ei kasva rajattomasti.
  expect_lte(length(visu:::the$px_calls), 2L)
})

test_that("datahaun hinta on kaksi pyyntoa", {
  vanha_max <- getOption("visu.px_max")
  options(visu.px_max = 10L)
  on.exit(options(visu.px_max = vanha_max), add = TRUE)
  visu_clear_cache()

  visu:::visu_px_throttle(2L)

  expect_length(visu:::the$px_calls, 2L)
})

test_that("valimuistin tyhjennys nollaa myos pyyntoikkunan", {
  visu:::visu_px_throttle(1L)
  expect_gt(length(visu:::the$px_calls), 0L)

  visu_clear_cache()

  expect_length(visu:::the$px_calls, 0L)
})

test_that("rajoitettu haku yritetaan uudelleen ja onnistuu", {
  skip_if_not_installed("pxwebtools")
  options(visu.px_backoff = 0.05)
  on.exit(options(visu.px_backoff = NULL), add = TRUE)
  visu_clear_cache()

  kutsuja <- 0L
  testthat::local_mocked_bindings(
    pxw_get_data = function(...) {
      kutsuja <<- kutsuja + 1L
      if (kutsuja < 3L) stop("Too Many Requests (RFC 6585) (HTTP 429).")
      data.frame(time = as.Date("2024-01-01"), values = 1)
    },
    .package = "pxwebtools"
  )

  tulos <- suppressMessages(visu_get_data("https://example.org/x.px/"))

  expect_equal(kutsuja, 3L)
  expect_equal(tulos$values, 1)
})

test_that("muu virhe kaataa heti eika kulu yrityksia", {
  skip_if_not_installed("pxwebtools")
  visu_clear_cache()

  kutsuja <- 0L
  testthat::local_mocked_bindings(
    pxw_get_data = function(...) {
      kutsuja <<- kutsuja + 1L
      stop("Bad Request (HTTP 400).")
    },
    .package = "pxwebtools"
  )

  expect_error(visu_get_data("https://example.org/x.px/"), "400")
  expect_equal(kutsuja, 1L)
})

test_that("sitkea rajoitus kaataa vasta yritysten jalkeen", {
  skip_if_not_installed("pxwebtools")
  options(visu.px_backoff = 0.05, visu.px_tries = 3L)
  on.exit(options(visu.px_backoff = NULL, visu.px_tries = NULL), add = TRUE)
  visu_clear_cache()

  kutsuja <- 0L
  testthat::local_mocked_bindings(
    pxw_get_data = function(...) {
      kutsuja <<- kutsuja + 1L
      stop("Too Many Requests (RFC 6585) (HTTP 429).")
    },
    .package = "pxwebtools"
  )

  expect_error(suppressMessages(visu_get_data("https://example.org/x.px/")), "429")
  expect_equal(kutsuja, 3L)
})
