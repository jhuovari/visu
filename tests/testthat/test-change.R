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

test_that("ryhmittely laskee muutoksen sarjoittain", {
  arvot <- c(10, 20, 100, 150)
  ryhma <- c("a", "a", "b", "b")

  out <- visu_change(arvot, lag = 1, by = ryhma)

  expect_true(all(is.na(out[c(1, 3)])))
  expect_equal(out[c(2, 4)], c(100, 50))
})

test_that("ryhmittelematon laskenta vuotaisi ryhmien yli", {
  arvot <- c(10, 20, 100, 150)

  ilman <- visu_change(arvot, lag = 1)

  # Kolmas havainto on eri sarjan ensimmainen, joten ilman ryhmittelya
  # muutokseksi tulisi 400 % sen sijaan etta se olisi NA.
  expect_equal(ilman[3], 400)
})

test_that("kelvoton syote kaataa selvalla viestilla", {
  expect_error(visu_change("a"), "numeerinen")
  expect_error(visu_change(1:5, lag = 0), "lag")
})
