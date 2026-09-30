test_that("prosenttimuutos lasketaan lag-havainnon takaiseen", {
  out <- visu_change(c(100, 110, 121), lag = 1)

  expect_true(is.na(out[1]))
  expect_equal(out[2:3], c(10, 10))
})

test_that("erotus sopii sarjalle joka on jo prosentti", {
  out <- visu_change(c(7.0, 7.5, 6.5), lag = 1, type = "diff")

  expect_equal(out[2:3], c(0.5, -1.0))
})

test_that("ensimmaiset lag havaintoa jaavat NA:ksi", {
  out <- visu_change(seq_len(24), lag = 12)

  expect_equal(sum(is.na(out)), 12L)
  expect_false(anyNA(out[13:24]))
})

test_that("lyhyt sarja on kokonaan NA eika pituus muutu", {
  out <- visu_change(c(1, 2, 3), lag = 12)

  expect_length(out, 3L)
  expect_true(all(is.na(out)))
})

test_that("ryhmittely hoidetaan dplyr-putkessa eika argumentilla", {
  skip_if_not_installed("dplyr")
  kk <- seq(as.Date("2024-01-01"), by = "month", length.out = 13)
  d <- rbind(
    data.frame(time = kk, sarja = "a", values = seq(100, 112)),
    data.frame(time = kk, sarja = "b", values = seq(200, 224, by = 2))
  )

  out <- d |>
    dplyr::group_by(sarja) |>
    dplyr::mutate(muutos = visu_change(values, time)) |>
    dplyr::ungroup()

  # Kumpikin sarja saa oman vertailukohtansa: 12 ensimmaista havaintoa NA.
  expect_equal(sum(is.na(out$muutos)), 24L)
  expect_equal(out$muutos[out$sarja == "a" & out$time == max(kk)], 12)
  expect_equal(out$muutos[out$sarja == "b" & out$time == max(kk)], 12)
})

test_that("rivijarjestys ei vaikuta tulokseen kun time on annettu", {
  kk <- seq(as.Date("2024-01-01"), by = "month", length.out = 13)
  arvot <- seq(100, 112)
  sekoitus <- c(5, 1, 13, 7, 2, 9, 3, 11, 4, 12, 6, 10, 8)

  jarjestyksessa <- visu_change(arvot, kk)
  sekaisin <- visu_change(arvot[sekoitus], kk[sekoitus])

  # Tulos palautetaan syotteen jarjestyksessa, joten se on sama havainnoittain.
  expect_equal(sekaisin, jarjestyksessa[sekoitus])
  expect_equal(sekaisin[sekoitus == 13], 12)
})

test_that("ilman time-saraketta rivit oletetaan aikajarjestykseen", {
  # Sama sekoitettu sarja ilman aikaa laskee vaarin, joten time on oletusreitti.
  expect_equal(visu_change(c(100, 110, 121), lag = 1)[3], 10)
})

test_that("havaintotiheys paatellaan aikasarakkeesta", {
  kk <- seq(as.Date("2020-01-01"), by = "month", length.out = 40)
  nv <- seq(as.Date("2020-01-01"), by = "3 months", length.out = 40)
  vv <- seq(as.Date("2000-01-01"), by = "year", length.out = 20)

  expect_equal(visu:::visu_freq(kk), 12L)
  expect_equal(visu:::visu_freq(nv), 4L)
  expect_equal(visu:::visu_freq(vv), 1L)
})

test_that("aukko sarjassa kaataa eika siirra vertailukohtaa hiljaa", {
  kk <- seq(as.Date("2020-01-01"), by = "month", length.out = 40)[-5]

  expect_error(visu_change(seq_along(kk), kk), "aukko")
})

test_that("vuosisarja ei kelpaa kausitasoitukseen", {
  vv <- seq(as.Date("2000-01-01"), by = "year", length.out = 20)

  expect_error(visu:::visu_seasonal_freq(vv, "bkt"), "vuosisarja")
})

test_that("kelvoton syote kaataa selvalla viestilla", {
  expect_error(visu_change("a", lag = 1), "numeerinen")
  expect_error(visu_change(1:5, lag = 0), "lag")
  expect_error(visu_change(1:5), "time")
  expect_error(visu_change(1:5, time = 1:5), "Date")
  expect_error(visu_change(1:5, time = seq(as.Date("2020-01-01"), by = "month",
                                           length.out = 4)), "eri pituisia")
})
