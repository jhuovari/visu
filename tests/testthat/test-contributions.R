# Kolme neljannesvuosisarjaa: kokonaisera, tuloera ja menoera.
aineisto <- function(yht, tulo, meno, n = length(yht)) {
  data.frame(
    time = rep(seq_len(n), times = 3),
    era = rep(c("yht", "tulo", "meno"), each = n),
    values = c(yht, tulo, meno),
    stringsAsFactors = FALSE
  )
}

test_that("kontribuutiot summautuvat kokonaiseran muutokseen", {
  # yht = tulo - meno kaikilla havainnoilla.
  tulo <- c(100, 102, 104, 106, 112, 115, 118, 120)
  meno <- c(20, 21, 22, 23, 25, 26, 27, 28)
  d <- aineisto(tulo - meno, tulo, meno)

  out <- visu_contributions(d, series = "era", total = "yht",
                            lag = 4, negate = "meno")

  summat <- tapply(out$values, out$time, sum)
  odotettu <- (tail(tulo - meno, 4) - head(tulo - meno, 4)) /
    head(tulo - meno, 4) * 100

  expect_equal(as.vector(summat), odotettu)
})

test_that("menoeran kasvu antaa negatiivisen kontribuution", {
  tulo <- rep(100, 8)
  meno <- c(rep(20, 4), rep(30, 4))
  d <- aineisto(tulo - meno, tulo, meno)

  out <- visu_contributions(d, series = "era", total = "yht",
                            lag = 4, negate = "meno")
  menot <- out[out$era == "meno", ]

  expect_true(all(menot$values < 0))
  # Menot kasvoivat 10 ja pohja oli 80, joten -12.5 prosenttiyksikkoa.
  expect_equal(unique(round(menot$values, 10)), -12.5)
})

test_that("jaannos taydentaa puuttuvat erat", {
  # Kokonaisera kasvaa enemman kuin annettu osa selittaa.
  yht <- c(100, 100, 100, 100, 120, 120, 120, 120)
  tulo <- c(50, 50, 50, 50, 55, 55, 55, 55)
  meno <- rep(0, 8)
  d <- aineisto(yht, tulo, meno)

  out <- visu_contributions(d, series = "era", total = "yht", lag = 4,
                            negate = "meno", residual = "Muut")

  summat <- tapply(out$values, out$time, sum)
  expect_equal(as.vector(summat), rep(20, 4))
  expect_true("Muut" %in% out$era)
  expect_equal(unique(out$values[out$era == "Muut"]), 15)
})

test_that("ilman jaannosta erittely voi jaada vajaaksi", {
  yht <- c(rep(100, 4), rep(120, 4))
  tulo <- c(rep(50, 4), rep(55, 4))
  meno <- rep(0, 8)
  d <- aineisto(yht, tulo, meno)

  out <- visu_contributions(d, series = "era", total = "yht", lag = 4)

  expect_false("Muut" %in% out$era)
  expect_equal(as.vector(tapply(out$values, out$time, sum)), rep(5, 4))
})

test_that("tuntematon sarake ja kokonaisera ovat virheita", {
  d <- aineisto(rep(1, 8), rep(1, 8), rep(0, 8))

  expect_error(visu_contributions(d, series = "puuttuu", total = "yht"),
               "ei ole datassa")
  expect_error(visu_contributions(d, series = "era", total = "vaara"),
               "Kokonaiser")
})

test_that("liian lyhyt sarja on virhe", {
  d <- aineisto(rep(1, 3), rep(1, 3), rep(0, 3))

  expect_error(visu_contributions(d, series = "era", total = "yht", lag = 4),
               "ei riit")
})

test_that("aikajarjestys ei riipu rivien jarjestyksesta", {
  tulo <- c(100, 102, 104, 106, 112, 115, 118, 120)
  meno <- c(20, 21, 22, 23, 25, 26, 27, 28)
  d <- aineisto(tulo - meno, tulo, meno)

  jarjestyksessa <- visu_contributions(d, series = "era", total = "yht",
                                       lag = 4, negate = "meno")
  sekaisin <- visu_contributions(d[sample(nrow(d)), ], series = "era",
                                 total = "yht", lag = 4, negate = "meno")

  jarj <- function(x) x[order(x$era, x$time), ]
  expect_equal(jarj(sekaisin), jarj(jarjestyksessa), ignore_attr = TRUE)
})
