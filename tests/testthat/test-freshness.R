test_that("taulun URL pilkkoutuu kansioksi ja tunnisteeksi", {
  parts <- visu:::visu_split_table_url(
    "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/11pk.px/"
  )

  expect_equal(parts$folder, "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti")
  expect_equal(parts$table, "11pk.px")
})

test_that("kyselymerkkijono ja ylimaaraiset kauttaviivat eivat haittaa", {
  parts <- visu:::visu_split_table_url(
    "https://example.org/api/v1/fi/StatFin/tyti//11pk.px/?lang=fi"
  )

  expect_equal(parts$table, "11pk.px")
})

test_that("kelvoton URL ei kaada tuoreustarkistusta", {
  expect_null(visu:::visu_split_table_url(NA_character_))
  expect_null(visu:::visu_split_table_url(""))
  expect_true(is.na(visu_table_updated("")))
})

test_that("kansiolistaus haetaan vain kerran ajoa kohti", {
  visu_clear_cache()
  listing <- data.frame(id = c("11pk.px", "muu.px"),
                        updated = c("2026-08-30T06:00:00Z", "2026-01-01T06:00:00Z"),
                        stringsAsFactors = FALSE)
  kutsut <- 0L

  testthat::local_mocked_bindings(
    fromJSON = function(...) {
      kutsut <<- kutsut + 1L
      listing
    },
    .package = "jsonlite"
  )

  base <- "https://example.org/api/v1/fi/StatFin/tyti/"
  expect_equal(visu_table_updated(paste0(base, "11pk.px/")), "2026-08-30T06:00:00Z")
  expect_equal(visu_table_updated(paste0(base, "muu.px/")), "2026-01-01T06:00:00Z")
  expect_equal(kutsut, 1L)
})

test_that("tuntematon taulu ja kaatunut haku palauttavat NA", {
  visu_clear_cache()
  testthat::local_mocked_bindings(
    fromJSON = function(...) data.frame(id = "toinen.px", updated = "2026-08-30T06:00:00Z"),
    .package = "jsonlite"
  )
  expect_true(is.na(visu_table_updated("https://example.org/db/11pk.px/")))

  visu_clear_cache()
  testthat::local_mocked_bindings(
    fromJSON = function(...) stop("verkko poikki"),
    .package = "jsonlite"
  )
  expect_warning(
    expect_true(is.na(visu_table_updated("https://example.org/db/11pk.px/"))),
    "Kansiolistausta ei saatu"
  )
})
