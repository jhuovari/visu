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

test_that("pylvaan leveys skaalautuu aika-akselin mukana", {
  # Leveys on x-akselin datayksikoissa. Jos sita ei skaalata x:n mukana,
  # nelj.vuosipylvaan leveydeksi jaa 81 millisekuntia 81 paivan sijaan ja
  # pylvaat ovat kuviossa nakymattomia.
  d <- data.frame(
    time = seq(as.Date("2020-01-01"), by = "3 months", length.out = 12),
    values = seq_len(12)
  )

  b <- plotly::plotly_build(visu_interactive(visu_plot(d, type = "col")))

  paivina <- b$x$data[[1]]$width[1] / 86400000
  expect_gt(paivina, 60)
  expect_lt(paivina, 95)
})

test_that("luokka-akselilla leveys jatetaan rauhaan", {
  # Poikkileikkauskuviossa ei ole aika-akselia, joten leveys on jo oikein.
  d <- data.frame(ryhma = c("a", "b", "c"), values = c(1, 2, 3))

  b <- plotly::plotly_build(visu_interactive(visu_plot(d, type = "col")))

  expect_lt(b$x$data[[1]]$width[1], 2)
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

test_that("nollaviivasta tulee piirtoalan levyinen muoto eika jaljesta", {
  d <- data.frame(
    time = seq(as.Date("1990-01-01"), as.Date("2026-01-01"), by = "year"),
    values = rep(c(-1, 2), length.out = 37)
  )

  ilman <- plotly::plotly_build(plotly::ggplotly(visu_plot(d)))
  kanssa <- plotly::plotly_build(visu_interactive(visu_plot(d)))

  expect_length(ilman$x$data, 2L)
  expect_length(kanssa$x$data, 1L)
  expect_length(kanssa$x$layout$shapes, 1L)
  # Ruudun oma koordinaatisto eika paperi: muoto osuu piirtoalaan ja
  # pienruutukuviossa oikean ruudun nollaan.
  expect_equal(kanssa$x$layout$shapes[[1]]$xref, "x domain")
  expect_equal(kanssa$x$layout$shapes[[1]]$yref, "y")
})

test_that("pienruutukuvio saa nollaviivan jokaiseen ruutuun", {
  d <- data.frame(
    time = rep(seq(as.Date("2020-01-01"), as.Date("2026-01-01"), by = "year"), 4),
    ryhma = rep(c("a", "b", "c", "d"), each = 7),
    values = rep(c(-1, 2), length.out = 28)
  )

  b <- plotly::plotly_build(visu_interactive(visu_plot(d, facet = "ryhma")))

  # Muodoissa on myos ruutujen otsikkopalkit, joten nollaviivat erotetaan
  # tyypin mukaan.
  viivat <- Filter(function(m) identical(m$type, "line"), b$x$layout$shapes)

  # Nelja ruutua, nelja nollaviivaa, eika yhtakaan nollaviivaa jaljissa.
  expect_length(viivat, 4L)
  expect_length(b$x$data, 4L)
  expect_setequal(
    vapply(viivat, function(m) paste(m$xref, m$yref), character(1)),
    c("x domain y", "x2 domain y", "x domain y2", "x2 domain y2")
  )
})

test_that("pienruutukuvion kaikki aika-akselit skaalataan", {
  d <- data.frame(
    time = rep(seq(as.Date("2020-01-01"), as.Date("2026-01-01"), by = "year"), 4),
    ryhma = rep(c("a", "b", "c", "d"), each = 7),
    values = seq_len(28)
  )

  b <- plotly::plotly_build(visu_interactive(visu_plot(d, facet = "ryhma")))
  akselit <- grep("^xaxis[0-9]*$", names(b$x$layout), value = TRUE)

  expect_gt(length(akselit), 1L)
  for (a in akselit) {
    expect_equal(b$x$layout[[a]]$type, "date")
    # Paivat millisekunteina: skaalaamaton akseli jaisi alle miljoonan ja
    # sen ruutu nayttaisi tyhjalta.
    expect_gt(min(as.numeric(b$x$layout[[a]]$range)), 1e9)
  }
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

test_that("yhdistelmanimesta jaa selitteeseen vain oikea nimi", {
  f <- visu:::visu_plain_name

  expect_equal(f("(Palkat,1)"), "Palkat")
  expect_equal(f("(1,Palkat)"), "Palkat")
  # Pilkku nimen sisalla ei saa katketa nimea.
  expect_equal(f("(Verot, netto,1)"), "Verot, netto")
})

test_that("tavallinen ja aidosti kaksiosainen nimi jaavat rauhaan", {
  f <- visu:::visu_plain_name

  expect_null(f("Palkat"))
  expect_null(f("(a,b)"))
  expect_null(f("(1,2)"))
  expect_null(f(NULL))
})

test_that("paallysviiva ei sotke pylvaiden nimia selitteessa", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-04-01")), each = 2),
    era = rep(c("Palkat", "Verot"), 2),
    values = c(1, 2, 2, 1)
  )
  viiva <- data.frame(time = unique(d$time), values = c(3, 3))

  w <- visu_interactive(visu_plot(d, colour = "era", type = "col", stack = TRUE,
                                  line = viiva, line_label = "Yhteensä"))
  nimet <- vapply(w$x$data, function(tr) tr$name %||% "", character(1))

  expect_equal(nimet, c("Palkat", "Verot", "Yhteensä"))
  expect_equal(w$x$data[[1]]$legendgroup, "Palkat")
})

test_that("lahde ankkuroidaan pikseleina ja merkitaan tunnistettavaksi", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))

  # plotly::layout() jättää asettelun rakennusvaiheeseen, joten annotaatiot
  # löytyvät vasta rakennetusta widgetistä.
  w <- plotly::plotly_build(visu_interactive(visu_plot(d), caption = "Lähde: X"))
  lahde <- Filter(function(a) identical(a$name, "visu-caption"), w$x$layout$annotations)

  expect_length(lahde, 1L)
  # Paperiyksikko on osuus piirtoalan korkeudesta ja kutistuu selitteen
  # kasvaessa, joten paikka annetaan pikseleina piirtoalan alareunasta.
  expect_equal(lahde[[1]]$y, 0)
  expect_true(lahde[[1]]$yshift < 0)
})

test_that("alaotsikko ei saa lahteen ankkurointia", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))

  w <- plotly::plotly_build(visu_interactive(visu_plot(d), subtitle = "Alaotsikko"))
  nimet <- vapply(w$x$layout$annotations, function(a) if (is.null(a$name)) "" else a$name, character(1))

  expect_false("visu-caption" %in% nimet)
})

test_that("widgetille annetaan korkeus, jotta selite ja lahde mahtuvat", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))

  expect_equal(visu_interactive(visu_plot(d))$height, 450)
  expect_equal(visu_interactive(visu_plot(d), height = 600)$height, 600)
})

test_that("leveys on prosentteina, jotta korkeus on pikseleita eika kuvasuhde", {
  d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = c(1, 2))

  # Quarton fig-responsive skaalaa korkeuden leveyden suhteessa, jos molemmat
  # ovat numeerisia. Prosenttileveys jattaa korkeuden rauhaan.
  expect_equal(visu_interactive(visu_plot(d))$width, "100%")
})

test_that("lahde vaistaa myos x-akselin lukuja, ei vain selitetta", {
  # Varsinainen varmistus on selainmittaus: ilman selitetta lahde osui
  # x-akselin lukurivin paalle, koska mittaus katsoi vain selitetta. Tama
  # testi pitaa mittauksen laajennettuna.
  js <- visu_caption_js()

  expect_match(js, ".legend, .xtick, .g-xtitle", fixed = TRUE)
})
