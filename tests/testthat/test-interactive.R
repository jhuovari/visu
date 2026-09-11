test_that("desimaalierotin seuraa kielta", {
  expect_equal(visu:::visu_separators("fi"), ", ")
  expect_equal(visu:::visu_separators("sv"), ", ")
  expect_equal(visu:::visu_separators("en"), ".,")
})

test_that("aika-akselin kerroin tunnistaa paivat ja sekunnit", {
  paivat <- visu_plot(data.frame(
    time = as.Date(c("2024-01-01", "2024-02-01")), values = c(1, 2)))
  sekunnit <- visu_plot(data.frame(
    time = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 01:00:00"), tz = "UTC"),
    values = c(1, 2)))
  luokat <- visu_plot(data.frame(ryhma = c("a", "b"), values = c(1, 2)), type = "col")

  expect_equal(visu:::visu_time_scale(paivat), 86400000)
  expect_equal(visu:::visu_time_scale(sekunnit), 1000)
  expect_null(visu:::visu_time_scale(luokat))
})

test_that("aika-akselista tulee plotlyn date-akseli ja arvot skaalataan", {
  d <- data.frame(
    time = seq(as.Date("1990-01-01"), as.Date("2026-01-01"), by = "year"),
    values = seq_len(37)
  )

  b <- plotly::plotly_build(visu_interactive(visu_plot(d)))

  expect_equal(b$x$layout$xaxis$type, "date")
  # Kiintean merkintataulukon pitaa olla poissa, jotta plotly muodostaa
  # merkinnat zoomatessa myos alkunakyman ulkopuolelle.
  expect_null(b$x$layout$xaxis$tickvals)
  expect_null(b$x$layout$yaxis$tickvals)
  # 1990-01-01 millisekunteina.
  expect_equal(b$x$data[[1]]$x[1], 631152000000)
})

test_that("nollaviivasta tulee koko leveyden muoto eika jaljesta", {
  d <- data.frame(
    time = seq(as.Date("1990-01-01"), as.Date("2026-01-01"), by = "year"),
    values = rep(c(-1, 2), length.out = 37)
  )

  ilman <- plotly::plotly_build(plotly::ggplotly(visu_plot(d)))
  kanssa <- plotly::plotly_build(visu_interactive(visu_plot(d)))

  expect_length(ilman$x$data, 2L)
  expect_length(kanssa$x$data, 1L)
  expect_length(kanssa$x$layout$shapes, 1L)
  expect_equal(kanssa$x$layout$shapes[[1]]$xref, "paper")
})

test_that("kuvio ilman nollaviivaa sailyttaa kaikki jalkensa", {
  d <- data.frame(
    time = seq(as.Date("1990-01-01"), as.Date("2026-01-01"), by = "year"),
    values = seq_len(37)
  )

  b <- plotly::plotly_build(visu_interactive(visu_plot(d)))

  expect_length(b$x$data, 1L)
  expect_length(b$x$layout$shapes, 0L)
})

test_that("luokka-akselin nimet sailyvat, koska ne ovat vain merkinnoissa", {
  d <- data.frame(
    ryhma = factor(rep(c("Liikenne", "Asuminen"), 2),
                   levels = c("Liikenne", "Asuminen")),
    paiva = factor(rep(c("a", "b"), each = 2)),
    values = c(1, 2, 1.5, 2.5)
  )

  b <- plotly::plotly_build(visu_interactive(visu_plot(d, x = "ryhma", colour = "paiva")))

  expect_equal(as.character(b$x$layout$xaxis$ticktext), c("Liikenne", "Asuminen"))
})

test_that("numeeriset merkinnat tunnistetaan pilkusta ja valilyonnista", {
  expect_true(visu:::visu_numeric_ticks(c("0,5", "1,0")))
  expect_true(visu:::visu_numeric_ticks(c("250 000", "300 000")))
  expect_true(visu:::visu_numeric_ticks(c("−2", "0", "2")))
  expect_false(visu:::visu_numeric_ticks(c("Liikenne", "Asuminen")))
  expect_false(visu:::visu_numeric_ticks(NULL))
})

test_that("poikkileikkauskuvioon ei liiteta y-akselin zoomiskriptia", {
  luokat <- visu_plot(data.frame(ryhma = c("a", "b"), values = c(1, 2)), type = "col")
  aika <- visu_plot(data.frame(
    time = as.Date(c("2024-01-01", "2024-02-01")), values = c(1, 2)))

  expect_null(visu_interactive(luokat)$jsHooks$render)
  expect_length(visu_interactive(aika)$jsHooks$render, 1L)
})
