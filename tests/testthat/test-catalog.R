# Luettelotestit katsovat repositorion oikeita kuvioita: luettelon tehtava on
# juuri vastata niita.
site <- testthat::test_path("..", "..", "site")

test_that("tunnisteet luetaan qmd-tiedostoista ilman ajoa", {
  skip_if_not(dir.exists(site), "sivustoa ei ole")
  sivut <- visu:::visu_chart_scan(site)

  expect_true(nrow(sivut) > 40L)
  expect_false(any(duplicated(sivut$id)))
  expect_true("ostovoima-erat" %in% sivut$id)
  expect_equal(sivut$page[sivut$id == "ostovoima-erat"], "ostovoima")
})

test_that("luettelo kattaa kaikki sivujen kuviot", {
  skip_if_not(dir.exists(site) && file.exists(visu_catalog_path(site)),
              "luetteloa ei ole")

  luettelo <- visu_catalog_read(site)
  sivut <- visu:::visu_chart_scan(site)

  expect_setequal(luettelo$id, sivut$id)
  # Otsikko on se, mihin kuvioon viitataan, joten sen on oltava joka rivilla.
  expect_true(all(nzchar(luettelo$title_fi) & !is.na(luettelo$title_fi)))
  expect_true(all(luettelo$page %in% sivut$page))
})

test_that("merkinnan kirjaus korvaa saman tunnisteen eika koske muihin", {
  path <- withr::local_tempfile(fileext = ".json")
  otsikot <- list(fi = list(title = "Eka", caption = "Lähde: X"))
  kuvat <- list(fi = "kuviot/kuvat/a-fi.png")

  visu:::visu_catalog_touch("a", "sivu", otsikot, kuvat, path = path)
  visu:::visu_catalog_touch("b", "sivu", otsikot, kuvat, path = path)
  visu:::visu_catalog_touch("a", "sivu",
                            list(fi = list(title = "Toka")), kuvat, path = path)

  luettelo <- visu:::visu_catalog_frame(
    jsonlite::fromJSON(path, simplifyVector = FALSE))

  expect_equal(nrow(luettelo), 2L)
  expect_equal(luettelo$title_fi[luettelo$id == "a"], "Toka")
  expect_equal(luettelo$title_fi[luettelo$id == "b"], "Eka")
})

test_that("otsikot luetaan kuviosta kielittain", {
  builder <- function(kieli) {
    d <- data.frame(time = as.Date(c("2024-01-01", "2024-04-01")), values = 1:2)
    visu_plot(d, title = paste("Otsikko", kieli), caption = "Lähde: X")
  }

  otsikot <- visu:::visu_chart_labels(builder)

  expect_equal(otsikot$fi$title, "Otsikko fi")
  expect_equal(otsikot$en$title, "Otsikko en")
  expect_equal(otsikot$sv$caption, "Lähde: X")
})

test_that("tuntematon tunniste kaatuu viestilla, joka listaa vaihtoehtoja", {
  skip_if_not(dir.exists(site), "sivustoa ei ole")

  expect_error(visu:::visu_chart_page("ei-ole-tallaista", site), "ei ole miss")
})
