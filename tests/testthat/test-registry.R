test_that("rekisteri lukee etulehdet ja tunnisteet tiedostonimista", {
  dir <- make_site(list(
    palkansaajat = chart_qmd("https://example.org/db/tbl.px/", "Palkansaajat"),
    tyottomat = chart_qmd("https://example.org/db/muu.px/", "Ty\u00f6tt\u00f6m\u00e4t")
  ))

  reg <- visu_chart_registry(dir)

  expect_equal(reg$id, c("palkansaajat", "tyottomat"))
  expect_equal(reg$title, c("Palkansaajat", "Ty\u00f6tt\u00f6m\u00e4t"))
  expect_equal(reg$table_url[1], "https://example.org/db/tbl.px/")
  expect_false(reg$code_hash[1] == reg$code_hash[2])
})

test_that("puuttuva table_url on virhe", {
  dir <- make_site(list(a = c("---", "title: \"A\"", "---", "", "teksti")))

  expect_error(visu_chart_registry(dir), "visu.table_url")
})

test_that("kuvioton sivusto antaa tyhjan rekisterin ilman virhetta", {
  dir <- make_site(list())

  reg <- visu_chart_registry(dir)

  expect_equal(nrow(reg), 0L)
  expect_equal(names(reg), c("id", "title", "table_url", "path", "code_hash"))
})

test_that("eheystarkistus huomaa etulehden ja koodilohkon eron", {
  dir <- make_site(list(
    ok = chart_qmd("https://example.org/db/tbl.px/"),
    rikki = chart_qmd("https://example.org/db/tbl.px/",
                      body_url = "https://example.org/db/eri.px/")
  ))

  problems <- visu_check_charts(dir)

  expect_length(problems, 1L)
  expect_match(problems, "'rikki'")
})

test_that("tila kirjoitetaan ja luetaan samana, avaimet jarjestyksessa", {
  dir <- make_site(list(a = chart_qmd("https://example.org/db/tbl.px/")))
  state <- list(b = list(built_at = "2026-08-30T05:01:00Z"),
                a = list(built_at = "2026-08-29T05:01:00Z"))

  visu_state_write(state, dir)
  out <- visu_state_read(dir)

  expect_equal(names(out), c("a", "b"))
  expect_equal(out$a$built_at, "2026-08-29T05:01:00Z")
})

test_that("puuttuva tilatiedosto luetaan tyhjana listana", {
  dir <- make_site(list())

  expect_equal(visu_state_read(dir), list())
})
