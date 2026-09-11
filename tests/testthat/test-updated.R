test_that("taulut luetaan renderoitavan tiedoston etulehdesta", {
  path <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: \"Koe\"",
    "visu:",
    "  table_url:",
    "    - \"https://example.org/db/a.px/\"",
    "    - \"https://example.org/db/b.px/\"",
    "---",
    "",
    "Tekstia."
  ), path)
  on.exit(unlink(path), add = TRUE)

  expect_equal(
    visu:::visu_current_table_urls(path),
    c("https://example.org/db/a.px/", "https://example.org/db/b.px/")
  )
})

test_that("etulehdeton tiedosto ei kaada lukemista", {
  path <- tempfile(fileext = ".qmd")
  writeLines("Pelkkaa tekstia.", path)
  on.exit(unlink(path), add = TRUE)

  expect_length(visu:::visu_current_table_urls(path), 0L)
  expect_length(visu:::visu_current_table_urls("ei-ole.qmd"), 0L)
})

test_that("tuorein taulu kertoo sivun paivitysajan", {
  visu_clear_cache()
  testthat::local_mocked_bindings(
    visu_table_updated = function(url) {
      c("https://example.org/db/a.px/" = "2026-09-01T08:00:00",
        "https://example.org/db/b.px/" = "2026-09-11T08:00:03")[[url]]
    }
  )

  ulos <- capture.output(
    visu_updated_note(c("https://example.org/db/a.px/",
                        "https://example.org/db/b.px/"))
  )

  expect_match(paste(ulos, collapse = " "), "11.9.2026 klo 8:00", fixed = TRUE)
})

test_that("ilman aikaleimoja ei tulosteta mitaan", {
  testthat::local_mocked_bindings(
    visu_table_updated = function(url) NA_character_
  )

  expect_output(visu_updated_note("https://example.org/db/a.px/"), NA)
  expect_output(visu_updated_note(character()), NA)
})
