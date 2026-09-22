# Sivustolla on oma kausitasoitus. Nama testit vahtivat kolmea asiaa: etta
# tulos on lahella tilaston omaa tasoitusta, etta kiinnitetty malli antaa
# saman tuloksen kuin sen valinnut ajo, ja etta mallitiedosto pysyy siistina.

make_seasonal_site <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines("project:\n  type: website", file.path(dir, "_quarto.yml"))
  dir
}

# Sarja, jossa on kausivaihtelu, trendi ja yksi yksittainen poikkeama.
fake_series <- function(n = 180, piikki = NULL) {
  set.seed(42)
  arvot <- 100 + cumsum(stats::rnorm(n, 0.2)) + 5 * sin(2 * pi * seq_len(n) / 12)
  if (!is.null(piikki)) arvot[piikki] <- arvot[piikki] + 40
  data.frame(
    time = seq(as.Date("2010-01-01"), by = "month", length.out = n),
    values = arvot
  )
}

test_that("kuukausi- ja neljannesvuosisarja tunnistetaan valista", {
  kk <- seq(as.Date("2020-01-01"), by = "month", length.out = 40)
  nv <- seq(as.Date("2020-01-01"), by = "3 months", length.out = 40)

  expect_equal(visu:::visu_seasonal_freq(kk, "x"), 12L)
  expect_equal(visu:::visu_seasonal_freq(nv, "x"), 4L)
})

test_that("aukko sarjassa kaataa haun eika mene hiljaa lapi", {
  kk <- seq(as.Date("2020-01-01"), by = "month", length.out = 40)[-5]

  expect_error(visu:::visu_seasonal_freq(kk, "tyolliset"), "aukko")
})

test_that("neljannesvuosisarjan ts alkaa oikeasta neljanneksesta", {
  aika <- seq(as.Date("2020-07-01"), by = "3 months", length.out = 20)

  tsd <- visu:::visu_seasonal_ts(aika, seq_along(aika), "x")

  expect_equal(stats::frequency(tsd), 4)
  expect_equal(stats::start(tsd), c(2020, 3))
})

test_that("liian lyhyt sarja kaataa selvalla viestilla", {
  d <- fake_series(n = 24)

  expect_error(visu:::visu_seasonal_ts(d$time, d$values, "lyhyt"),
               "kolme vuotta")
})

test_that("puuttuva arvo kaataa, koska X-13 ei huomaisi sita", {
  d <- fake_series(n = 60)
  d$values[10] <- NA

  expect_error(visu:::visu_seasonal_ts(d$time, d$values, "aukollinen"),
               "puuttuvia")
})

test_that("malli on vanhentunut kun sita ei ole tai valinnasta on vuosi", {
  tuore <- list(valittu = format(Sys.Date()), malli = "(0 1 1)(0 1 1)")
  vanha <- list(valittu = format(Sys.Date() - 400), malli = "(0 1 1)(0 1 1)")

  expect_true(visu:::visu_seasonal_stale(NULL))
  expect_true(visu:::visu_seasonal_stale(list(valittu = format(Sys.Date()))))
  expect_true(visu:::visu_seasonal_stale(vanha))
  expect_false(visu:::visu_seasonal_stale(tuore))
})

test_that("mallitiedosto kirjoitetaan aakkosjarjestyksessa", {
  dir <- make_seasonal_site()
  visu:::visu_seasonal_write(list(b = list(malli = "x"), a = list(malli = "y")), dir)

  luettu <- visu:::visu_seasonal_read(dir)

  expect_equal(names(luettu), c("a", "b"))
})

test_that("puuttuva mallitiedosto on tyhja lista eika virhe", {
  dir <- make_seasonal_site()

  expect_equal(visu:::visu_seasonal_read(dir), list())
})

test_that("malli valitaan kerran ja kiinnitetaan sen jalkeen", {
  skip_if_not_installed("seasonal")
  dir <- make_seasonal_site()
  d <- fake_series(piikki = 100)

  eka <- visu_seasonal(d, "testi", site_dir = dir)
  mallit <- visu:::visu_seasonal_read(dir)
  # Toinen ajo lukee mallin tiedostosta eika valitse uudelleen.
  toka <- visu_seasonal(d, "testi", site_dir = dir)

  expect_named(mallit, "testi")
  expect_match(mallit$testi$malli, "^\\([0-9] [0-9] [0-9]\\)")
  expect_equal(eka$kausi, toka$kausi, tolerance = 1e-4)
  expect_equal(eka$trendi, toka$trendi, tolerance = 1e-4)
})

test_that("yksittainen poikkeama loytyy, tasosiirtyma ei", {
  skip_if_not_installed("seasonal")
  dir <- make_seasonal_site()

  visu_seasonal(fake_series(piikki = 100), "piikki", site_dir = dir)
  loydetyt <- visu:::visu_seasonal_read(dir)$piikki$regressorit

  expect_true(any(grepl("^ao", loydetyt)))
  expect_false(any(grepl("^ls", loydetyt)))
})

test_that("ryhmat tasoitetaan erikseen ja saavat omat mallinsa", {
  skip_if_not_installed("seasonal")
  dir <- make_seasonal_site()
  d <- rbind(cbind(fake_series(), ryhma = "a"), cbind(fake_series(), ryhma = "b"))
  d$values[d$ryhma == "b"] <- d$values[d$ryhma == "b"] * 2

  r <- visu_seasonal(d, "kaksi", by = "ryhma", site_dir = dir)

  expect_setequal(names(visu:::visu_seasonal_read(dir)),
                  c("kaksi|a", "kaksi|b"))
  expect_false(anyNA(r$kausi))
  expect_equal(r$kausi[d$ryhma == "b"], 2 * r$kausi[d$ryhma == "a"],
               tolerance = 1e-6)
})

test_that("oma kausitasoitus on lahella Tilastokeskuksen omaa", {
  skip_if_not_installed("seasonal")
  dir <- make_seasonal_site()
  # Tyolliset 15-74 on ainoa sarja, josta tilasto julkaisee seka
  # alkuperaisen etta oman kausitasoituksensa, joten se kelpaa mitaksi.
  d <- readRDS(test_path("fixtures", "tyolliset-15-74.rds"))

  r <- visu_seasonal(d, "tyolliset", y = "alkuperainen", site_dir = dir)
  v <- r[!is.na(r$tk_kausi), ]

  # Taso on tuhatta henkea, eli 12 on 0,5 prosenttia sarjan tasosta.
  expect_lt(mean(abs(v$kausi - v$tk_kausi)), 12)
  expect_lt(mean(abs(v$trendi - v$tk_trendi)), 8)
  # Systemaattista eroa ei saa olla kumpaankaan suuntaan.
  expect_lt(abs(mean(v$kausi - v$tk_kausi)), 3)
  expect_gt(stats::cor(v$trendi, v$tk_trendi), 0.99)
})
