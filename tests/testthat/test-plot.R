test_that("viiva paksunee karkeimmasta siloitetuimpaan", {
  expect_equal(visu:::visu_linewidths(1), 0.8)
  expect_equal(visu:::visu_linewidths(2), c(0.45, 0.8))
  expect_equal(visu:::visu_linewidths(3), c(0.3, 0.55, 0.8))

  nelja <- visu:::visu_linewidths(4)
  expect_length(nelja, 4L)
  expect_false(is.unsorted(nelja))
  expect_equal(nelja[4], 0.8)
})

test_that("luokkien maara luetaan tekijan tasoista, ei riveista", {
  d <- data.frame(sarja = factor(c("a", "a", "b"), levels = c("a", "b", "c")))

  # Kayttamaton taso "c" ei saa varata omaa viivanpaksuutta.
  expect_equal(visu:::visu_level_count(d, "sarja"), 2L)
})

test_that("linewidth kartoitetaan ja saa oman skaalan", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-02-01")), 2),
    sarja = factor(rep(c("Alkuperäinen", "Trendi"), each = 2),
                   levels = c("Alkuperäinen", "Trendi")),
    values = c(1, 2, 1.5, 1.6)
  )

  p <- visu_plot(d, linewidth = "sarja")

  expect_true("linewidth" %in% names(p$mapping))
  expect_equal(p$scales$get_scales("linewidth")$palette(2), c(0.45, 0.8))
})

test_that("linewidth ei sovi pylvaille eika alueille", {
  d <- data.frame(time = as.Date("2024-01-01"), sarja = "a", values = 1)

  expect_error(visu_plot(d, linewidth = "sarja", type = "col"), "line")
  expect_error(visu_plot(d, linewidth = "sarja", type = "area"), "line")
})

test_that("tuntematon sarake kaataa selvalla viestilla", {
  d <- data.frame(time = as.Date("2024-01-01"), values = 1)

  expect_error(visu_plot(d, linewidth = "puuttuu"), "linewidth")
})

test_that("nollaviiva tulee kun sarja ylittaa nollan kumpaankin suuntaan", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-02-01")), values = c(-1, 2))

  expect_true(visu:::visu_needs_zeroline(d, "values", NULL))
})

test_that("kokonaan nollan yhdella puolella oleva sarja jaa ilman nollaviivaa", {
  ylla <- data.frame(values = c(1, 2))
  alla <- data.frame(values = c(-2, -1))

  expect_false(visu:::visu_needs_zeroline(ylla, "values", NULL))
  expect_false(visu:::visu_needs_zeroline(alla, "values", NULL))
})

test_that("zeroline ohittaa automatiikan kumpaankin suuntaan", {
  ylla <- data.frame(values = c(1, 2))
  ylitse <- data.frame(values = c(-1, 2))

  expect_true(visu:::visu_needs_zeroline(ylla, "values", TRUE))
  expect_false(visu:::visu_needs_zeroline(ylitse, "values", FALSE))
})

test_that("pelkat puuttuvat arvot eivat kaada nollaviivan paattelya", {
  d <- data.frame(values = c(NA_real_, NA_real_))

  expect_false(visu:::visu_needs_zeroline(d, "values", NULL))
})

test_that("nollaviiva piirtyy datan alle", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-02-01")), values = c(-1, 2))

  kerrokset <- vapply(visu_plot(d)$layers, function(l) class(l$geom)[1], character(1))

  expect_equal(unname(kerrokset), c("GeomHline", "GeomLine"))
})

test_that("suomi ja ruotsi saavat desimaalipilkun, englanti pisteen", {
  arvot <- c(-0.5, 0, 1.25)

  expect_equal(visu:::visu_axis_labels("fi")(arvot), c("-0,50", "0,00", "1,25"))
  expect_equal(visu:::visu_axis_labels("sv")(arvot), c("-0,50", "0,00", "1,25"))
  expect_equal(visu:::visu_axis_labels("en")(arvot), c("-0.50", "0.00", "1.25"))
})

test_that("akselin puuttuva arvo muotoillaan tyhjaksi", {
  expect_equal(visu:::visu_axis_labels("fi")(c(1, NA)), c("1", ""))
})

# Nollaviiva lisataan omana layerinaan ennen varsinaista geomia, joten
# paageom haetaan luokan eika indeksin perusteella.
paageom <- function(p) {
  Filter(function(l) !inherits(l$geom, "GeomHline"), p$layers)[[1]]
}

test_that("kartoittamaton vari annetaan kiinteana, jotta plotly nakee sen", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-02-01")), values = c(1, 2))

  # ggplot2 4.0 hakisi varin teemasta, mutta ggplotly ei ratkaise sita.
  expect_equal(paageom(visu_plot(d))$aes_params$colour, ggcustom::vm_pal(1))
  expect_equal(paageom(visu_plot(d, type = "col"))$aes_params$fill, ggcustom::vm_pal(1))
  expect_equal(paageom(visu_plot(d, type = "area"))$aes_params$fill, ggcustom::vm_pal(1))
})

test_that("kartoitettu vari jatetaan skaalan hoidettavaksi", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-02-01")), 2),
    sarja = factor(rep(c("a", "b"), each = 2)),
    values = c(1, 2, 3, 4)
  )

  expect_null(paageom(visu_plot(d, colour = "sarja"))$aes_params$colour)
  expect_null(paageom(visu_plot(d, colour = "sarja", type = "col"))$aes_params$fill)
})

test_that("kartoitettu paksuus ei vie kiinteaa varia", {
  # Suhdannekuvaajan tasokuvio kartoittaa paksuuden mutta ei varia.
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-02-01")), 2),
    sarja = factor(rep(c("Alkuperäinen", "Trendi"), each = 2),
                   levels = c("Alkuperäinen", "Trendi")),
    values = c(1, 2, 1.5, 1.6)
  )

  p <- visu_plot(d, linewidth = "sarja")

  expect_equal(paageom(p)$aes_params$colour, ggcustom::vm_pal(1))
  # Paksuuden antaa skaala, ei kiintea arvo.
  expect_null(paageom(p)$aes_params$linewidth)
})

test_that("pylvaat pinotaan vain pyydettaessa", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), 2),
    era = factor(rep(c("a", "b"), each = 2)),
    values = c(1, 2, -1, 3)
  )

  vierekkain <- paageom(visu_plot(d, colour = "era", type = "col"))
  pinottu <- paageom(visu_plot(d, colour = "era", type = "col", stack = TRUE))

  expect_s3_class(vierekkain$position, "PositionDodge")
  expect_s3_class(pinottu$position, "PositionStack")
})

test_that("paallysviiva piirtyy pylvaiden paalle ja saa nimen selitteeseen", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 2),
    era = rep(c("a", "b"), 2),
    values = c(1, 2, 2, 1)
  )
  viiva <- data.frame(time = unique(d$time), values = c(3, 3))

  p <- visu_plot(d, colour = "era", type = "col", stack = TRUE,
                 line = viiva, line_label = "Yhteens\u00e4")
  kerrokset <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

  # Viiva viimeisena, jotta pylvaat eivat peita sita.
  expect_equal(unname(utils::tail(kerrokset, 1L)), "GeomLine")
  expect_equal(p$scales$get_scales("colour")$palette(1),
               stats::setNames("grey15", "Yhteens\u00e4"))
})

test_that("nimeton paallysviiva jaa selitteesta pois", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))
  viiva <- data.frame(time = d$time, values = c(3, 3))

  p <- visu_plot(d, type = "col", line = viiva)

  expect_null(p$scales$get_scales("colour"))
})

test_that("paallysviiva ei sovi viivakuvioon eika vaaraan dataan", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))
  viiva <- data.frame(time = d$time, values = c(3, 3))

  expect_error(visu_plot(d, line = viiva), "line")
  expect_error(visu_plot(d, type = "col", line = viiva$values), "data frame")
  expect_error(visu_plot(d, type = "col", line = data.frame(time = d$time)), "values")
})

test_that("pylvaissa ja alueissa ei ole reunaviivaa", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 2),
    era = rep(c("a", "b"), 2), values = c(1, 2, 2, 1)
  )

  # ggplot2 4.0:ssa pylvaan reunavari tulee teemasta, ja ggcustom antaa sille
  # korostusvarin: ilman nimenomaista NA:ta jokaisen pylvaan ymparille piirtyy
  # viiva, jota plotly-versiossa ei ole.
  for (tyyppi in c("col", "area")) {
    p <- visu_plot(d, colour = "era", type = tyyppi, stack = TRUE)
    rakennettu <- ggplot2::ggplot_build(p)
    kerros <- which(vapply(p$layers, function(l) !inherits(l$geom, "GeomHline"),
                           logical(1)))[1]

    expect_true(all(is.na(rakennettu$data[[kerros]]$colour)),
                info = paste("tyyppi", tyyppi))
  }
})

test_that("viivakuvion vari ei muutu", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))

  rakennettu <- ggplot2::ggplot_build(visu_plot(d))
  kerros <- rakennettu$data[[length(rakennettu$data)]]

  expect_false(any(is.na(kerros$colour)))
})

test_that("erat ovat selitteessa ennen kokonaissarjaa", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 2),
    era = rep(c("a", "b"), 2), values = c(1, 2, 2, 1)
  )
  viiva <- data.frame(time = unique(d$time), values = c(3, 3))

  p <- visu_plot(d, colour = "era", type = "col", stack = TRUE,
                 line = viiva, line_label = "Yhteensä")

  expect_equal(p$guides$guides$fill$params$order, 1)
  expect_equal(p$guides$guides$colour$params$order, 2)
})
