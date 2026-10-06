# Freeze-valimuisti on se mekanismi, jolla epaonnistunut kuvio sailyy
# edellisessa versiossaan eika kaada koko sivuston renderointia.

freeze_site <- function(ids, sisalto = "vanha") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  for (id in ids) {
    kansio <- file.path(dir, "_freeze", "kuviot", id)
    dir.create(kansio, recursive = TRUE)
    writeLines(sisalto, file.path(kansio, "html.json"))
  }
  dir
}

test_that("talteen otettu valimuisti palautuu entisellaan", {
  dir <- freeze_site("a")
  stash <- visu_freeze_stash(dir, "a")
  on.exit(unlink(stash, recursive = TRUE), add = TRUE)
  unlink(visu_freeze_dir(dir), recursive = TRUE)

  puuttuu <- visu_freeze_restore(dir, stash, "a")

  expect_equal(puuttuu, character())
  expect_equal(readLines(file.path(visu_freeze_dir(dir), "a", "html.json")), "vanha")
})

test_that("palautus korvaa renderoinnin jalkeensa jattaman valimuistin", {
  dir <- freeze_site("a")
  stash <- visu_freeze_stash(dir, "a")
  on.exit(unlink(stash, recursive = TRUE), add = TRUE)
  writeLines("puolivalmis", file.path(visu_freeze_dir(dir), "a", "html.json"))

  visu_freeze_restore(dir, stash, "a")

  expect_equal(readLines(file.path(visu_freeze_dir(dir), "a", "html.json")), "vanha")
})

test_that("kuvio ilman aiempaa versiota raportoidaan palautuksessa", {
  dir <- freeze_site("a")
  stash <- visu_freeze_stash(dir, c("a", "uusi"))
  on.exit(unlink(stash, recursive = TRUE), add = TRUE)

  expect_equal(visu_freeze_restore(dir, stash, c("a", "uusi")), "uusi")
})

test_that("muuttumaton koodi tunnistetaan, muuttunut ja uusi eivat", {
  reg <- data.frame(id = "a", code_hash = "koodi1", stringsAsFactors = FALSE)

  expect_true(visu_code_unchanged("a", reg, list(a = list(code_hash = "koodi1"))))
  expect_false(visu_code_unchanged("a", reg, list(a = list(code_hash = "koodi2"))))
  expect_false(visu_code_unchanged("a", reg, list()))
})
