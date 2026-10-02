aikasarja <- function(n = 24, arvot = NULL) {
  data.frame(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = n),
    values = arvot %||% seq_len(n)
  )
}

test_that("aikarajaus laskee y-rajan uudelleen nakyvasta datasta", {
  d <- aikasarja(24)

  p <- visu_plot(d, start = NA)
  r <- visu_restyle(p, start = "2024-01-01")

  # Koko sarja on 1-24, mutta 2024 alkaen vain 17-24.
  expect_equal(r$coordinates$limits$y, c(17, 24))
  expect_equal(r$coordinates$limits$x, as.Date(c("2024-01-01", "2025-10-01")))
})

test_that("pinotun kuvion raja lasketaan pinosta myos rajauksen jalkeen", {
  d <- data.frame(
    time = rep(seq(as.Date("2020-01-01"), by = "quarter", length.out = 4), each = 2),
    era = rep(c("a", "b"), 4),
    values = c(1, 1, 1, 1, 5, 5, 2, 2)
  )

  p <- visu_plot(d, colour = "era", type = "col", stack = TRUE, start = NA)
  r <- visu_restyle(p, start = "2020-07-01")

  # Rivien suurin arvo on 5, mutta pinon korkeus 10.
  expect_equal(r$coordinates$limits$y, c(0, 10))
})

test_that("paallysviiva otetaan rajauksessa huomioon", {
  d <- aikasarja(8, arvot = rep(1, 8))
  viiva <- data.frame(time = d$time, values = c(rep(0, 4), 2, 3, 9, 4))

  p <- visu_plot(d, type = "col", line = viiva, start = NA)
  r <- visu_restyle(p, start = "2021-01-01")

  expect_equal(r$coordinates$limits$y, c(0, 9))
})

test_that("otsikko lahtee mutta lahde jaa", {
  p <- visu_plot(aikasarja(), title = "Otsikko", subtitle = "Alaotsikko",
                 caption = "Lähde: X")

  r <- visu_restyle(p, titles = FALSE)

  expect_null(r$labels$title)
  expect_null(r$labels$subtitle)
  expect_equal(r$labels$caption, "Lähde: X")
})

test_that("selite siirtyy alas vain kun sarjoja on vahan", {
  vahan <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 2),
    sarja = rep(c("a", "b"), 2), values = c(1, 2, 2, 1)
  )
  monta <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 6),
    sarja = rep(letters[1:6], 2), values = seq_len(12)
  )

  expect_equal(visu:::visu_legend_side(visu_plot(vahan, colour = "sarja")), "bottom")
  expect_equal(visu:::visu_legend_side(visu_plot(monta, colour = "sarja")), "right")
})

test_that("paallysviiva lasketaan selitteen sarjoihin", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 4),
    era = rep(letters[1:4], 2), values = seq_len(8)
  )
  viiva <- data.frame(time = unique(d$time), values = c(9, 9))

  nelja <- visu_plot(d, colour = "era", type = "col", stack = TRUE)
  viisi <- visu_plot(d, colour = "era", type = "col", stack = TRUE,
                     line = viiva, line_label = "Yhteensä")

  expect_equal(visu:::visu_legend_entries(nelja), 4L)
  expect_equal(visu:::visu_legend_entries(viisi), 5L)
})

test_that("rajaus kaatuu selvasti kun x ei ole aikaa tai valilla ei ole dataa", {
  luokat <- data.frame(ryhma = c("a", "b"), values = c(1, 2))

  expect_error(visu_restyle(visu_plot(luokat, type = "col"), start = "2024-01-01"),
               "ei ole aikaa")
  # Kahden neljanneksen valiin osuva ikkuna on kelvollinen mutta tyhja.
  expect_error(visu_restyle(visu_plot(aikasarja(), start = NA),
                            start = "2024-02-01", end = "2024-02-20"),
               "ei ole havaintoja")
  expect_error(visu_restyle(visu_plot(aikasarja(), start = NA),
                            start = "2025-01-01", end = "2024-01-01"),
               "aiemmin")
})

test_that("kokokerroin kasvattaa teeman tekstit", {
  p <- visu_restyle(visu_plot(aikasarja()), scale = 1.5)

  expect_equal(p$theme$text$size, 18)
  expect_error(visu_restyle(visu_plot(aikasarja()), scale = -1), "positiivinen")
})
