test_that("uudet havainnot kertyvat tiedostoon", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  eka <- data.frame(time = as.Date(c("2026-09-01", "2026-09-02")),
                    sarja = "a", values = c(1, 2))
  toka <- data.frame(time = as.Date(c("2026-09-02", "2026-09-03")),
                     sarja = "a", values = c(2, 3))

  visu_accumulate(eka, path)
  kaikki <- visu_accumulate(toka, path)

  expect_equal(nrow(kaikki), 3L)
  expect_equal(kaikki$values, c(1, 2, 3))
  expect_true(file.exists(path))
})

test_that("saman havainnon uusi arvo voittaa, jotta korjaukset menevat lapi", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  vanha <- data.frame(time = as.Date("2026-09-01"), sarja = "a", values = 1)
  korjattu <- data.frame(time = as.Date("2026-09-01"), sarja = "a", values = 9)

  visu_accumulate(vanha, path)
  kaikki <- visu_accumulate(korjattu, path)

  expect_equal(nrow(kaikki), 1L)
  expect_equal(kaikki$values, 9)
})

test_that("eri sarjat eivat syo toisiaan", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  d <- data.frame(time = as.Date("2026-09-01"), sarja = c("a", "b"),
                  values = c(1, 2))

  kaikki <- visu_accumulate(d, path)

  expect_equal(nrow(kaikki), 2L)
})

test_that("aika sailyy Date-tyyppisena tiedoston kautta", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  d <- data.frame(time = as.Date("2026-09-01"), sarja = "a", values = 1)

  visu_accumulate(d, path)
  kaikki <- visu_accumulate(d, path)

  expect_s3_class(kaikki$time, "Date")
})

test_that("arvosarakkeen puuttuminen kaataa selvalla viestilla", {
  expect_error(visu_accumulate(data.frame(time = 1), tempfile()), "values")
})

test_that("Suomen Pankin JSON luetaan sarjoiksi ja errors jatetaan pois", {
  testthat::local_mocked_bindings(
    visu_px_json = function(...) list(
      euribor1vko = data.frame(date = c("2026-09-01", "2026-09-02"),
                               value = c(2.1, 2.2)),
      euribor3kk = data.frame(date = "2026-09-01", value = 2.6),
      errors = list()
    )
  )

  d <- visu_get_bof("https://example.org/api/interestrates/euribor")

  expect_equal(names(d), c("time", "sarja", "values"))
  expect_setequal(unique(d$sarja), c("euribor1vko", "euribor3kk"))
  expect_equal(nrow(d), 3L)
  expect_s3_class(d$time, "Date")
})

test_that("odottamaton vastaus kaataa selvalla viestilla", {
  testthat::local_mocked_bindings(visu_px_json = function(...) list(errors = list()))
  expect_error(visu_get_bof("https://example.org/x"), "JSON-muotoa")

  testthat::local_mocked_bindings(visu_px_json = function(...) NULL)
  expect_error(visu_get_bof("https://example.org/x"), "ei saatu")
})
