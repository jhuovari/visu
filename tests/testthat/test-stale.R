registry_row <- function(id, url = "https://example.org/db/tbl.px/", code = "koodi1") {
  reg <- data.frame(id = id, title = id, path = paste0(id, ".qmd"),
                    code_hash = code, stringsAsFactors = FALSE)
  # table_url on listasarake, koska kuvio voi lukea useaa taulua.
  reg$table_url <- list(url)
  reg
}

# Vanhan tilatiedoston muoto: yksi taulu ja aikaleima ilman osoitetta.
state_row <- function(url = "https://example.org/db/tbl.px/", code = "koodi1",
                      updated = "2026-08-01T06:00:00Z") {
  list(table_url = url, code_hash = code, source_updated = updated,
       built_at = "2026-08-01T05:01:00Z")
}

test_that("tuntematon kuvio on aina vanhentunut", {
  reg <- registry_row("a")
  out <- visu_stale_charts(reg, list(), c(a = "2026-08-01T06:00:00Z"))

  expect_true(out$stale)
  expect_equal(out$reason, "uusi kuvio")
})

test_that("muuttumaton kuvio ja aikaleima eivat aiheuta uudelleenrakennusta", {
  reg <- registry_row("a")
  out <- visu_stale_charts(reg, list(a = state_row()), c(a = "2026-08-01T06:00:00Z"))

  expect_false(out$stale)
  expect_equal(out$reason, "ajan tasalla")
})

test_that("muuttunut lahdeaikaleima vanhentaa kuvion", {
  reg <- registry_row("a")
  out <- visu_stale_charts(reg, list(a = state_row()), c(a = "2026-08-30T06:00:00Z"))

  expect_true(out$stale)
  expect_equal(out$reason, "data p\u00e4ivittynyt")
})

test_that("muuttunut koodi vanhentaa kuvion vaikka data olisi ennallaan", {
  reg <- registry_row("a", code = "koodi2")
  out <- visu_stale_charts(reg, list(a = state_row()), c(a = "2026-08-01T06:00:00Z"))

  expect_true(out$stale)
  expect_equal(out$reason, "koodi muuttunut")
})

test_that("vaihtunut lahdetaulu vanhentaa kuvion", {
  reg <- registry_row("a", url = "https://example.org/db/muu.px/")
  out <- visu_stale_charts(reg, list(a = state_row()), c(a = "2026-08-01T06:00:00Z"))

  expect_true(out$stale)
  expect_equal(out$reason, "l\u00e4hdetaulu vaihtunut")
})

test_that("tuntematon aikaleima rakentaa varmuuden vuoksi uudelleen", {
  reg <- registry_row("a")
  out <- visu_stale_charts(reg, list(a = state_row()), c(a = NA_character_))

  expect_true(out$stale)
  expect_equal(out$reason, "aikaleima tuntematon")
})

test_that("aiemmin tuntemattomaksi jaanyt aikaleima vanhentaa kun se selviaa", {
  reg <- registry_row("a")
  state <- list(a = state_row(updated = NULL))
  out <- visu_stale_charts(reg, state, c(a = "2026-08-30T06:00:00Z"))

  expect_true(out$stale)
  expect_equal(out$reason, "data p\u00e4ivittynyt")
})

test_that("force pakottaa vain nimetyt kuviot", {
  reg <- rbind(registry_row("a"), registry_row("b"))
  state <- list(a = state_row(), b = state_row())
  updated <- c(a = "2026-08-01T06:00:00Z", b = "2026-08-01T06:00:00Z")

  out <- visu_stale_charts(reg, state, updated, force = "b")

  expect_equal(out$stale, c(FALSE, TRUE))
  expect_equal(out$reason, c("ajan tasalla", "pakotettu"))

  expect_true(all(visu_stale_charts(reg, state, updated, force = TRUE)$stale))
})

test_that("vain osa kuvioista voi olla vanhentunut kerralla", {
  reg <- rbind(registry_row("a"), registry_row("b"), registry_row("c"))
  state <- list(a = state_row(), b = state_row(), c = state_row())
  updated <- c(a = "2026-08-01T06:00:00Z",
               b = "2026-08-30T06:00:00Z",
               c = "2026-08-01T06:00:00Z")

  out <- visu_stale_charts(reg, state, updated)

  expect_equal(out$id[out$stale], "b")
})

test_that("muuttumattomien kuvioiden tila sailyy ennallaan", {
  reg <- rbind(registry_row("a"), registry_row("b"))
  state <- list(a = state_row(), b = state_row())
  updated <- c(a = "2026-08-01T06:00:00Z", b = "2026-08-30T06:00:00Z")
  decisions <- visu_stale_charts(reg, state, updated)

  out <- visu:::visu_new_state(reg, updated, state, decisions)

  expect_identical(out$a, state$a)
  expect_equal(out$b$source_updated,
               list("https://example.org/db/tbl.px" = "2026-08-30T06:00:00Z"))
  expect_false(identical(out$b$built_at, state$b$built_at))
})

test_that("tuntematon aikaleima ei jata vanhaa leimaa tilaan", {
  reg <- registry_row("a")
  decisions <- visu_stale_charts(reg, list(), c(a = NA_character_))

  out <- visu:::visu_new_state(reg, c(a = NA_character_), list(), decisions)

  expect_null(out$a$source_updated)
})

test_that("poistuneet kuviot putoavat tilasta", {
  reg <- registry_row("a")
  state <- list(a = state_row(), poistettu = state_row())
  decisions <- visu_stale_charts(reg, state, c(a = "2026-08-01T06:00:00Z"))

  out <- visu:::visu_new_state(reg, c(a = "2026-08-01T06:00:00Z"), state, decisions)

  expect_equal(names(out), "a")
})

test_that("epaonnistunut kuvio pitaa entisen tilansa", {
  reg <- registry_row("a", code = "koodi2")
  state <- list(a = state_row())
  decisions <- visu_stale_charts(reg, state, c(a = "2026-08-01T06:00:00Z"))

  out <- visu:::visu_new_state(reg, c(a = "2026-08-01T06:00:00Z"), state,
                               decisions, failed = "a")

  expect_identical(out$a, state$a)
})

test_that("epaonnistunut uusi kuvio jaa kokonaan ilman merkintaa", {
  reg <- rbind(registry_row("a"), registry_row("b"))
  updated <- c(a = "2026-08-01T06:00:00Z", b = "2026-08-01T06:00:00Z")
  decisions <- visu_stale_charts(reg, list(), updated)

  out <- visu:::visu_new_state(reg, updated, list(), decisions, failed = "a")

  expect_equal(names(out), "b")
})

test_that("epaonnistunut uusi kuvio on seuraavalla ajolla taas vanhentunut", {
  reg <- rbind(registry_row("a"), registry_row("b"))
  updated <- c(a = "2026-08-01T06:00:00Z", b = "2026-08-01T06:00:00Z")
  state <- visu:::visu_new_state(reg, updated, list(),
                                 visu_stale_charts(reg, list(), updated),
                                 failed = "a")

  out <- visu_stale_charts(reg, state, updated)

  expect_equal(out$reason, c("uusi kuvio", "ajan tasalla"))
})

# Usean taulun kuviot ----------------------------------------------------------

multi_registry_row <- function(id, urls = c("https://example.org/db/a.px/",
                                            "https://example.org/db/b.px/"),
                               code = "koodi1") {
  reg <- data.frame(id = id, title = id, path = paste0(id, ".qmd"),
                    code_hash = code, stringsAsFactors = FALSE)
  reg$table_url <- list(urls)
  reg
}

multi_state_row <- function(urls = c("https://example.org/db/a.px/",
                                     "https://example.org/db/b.px/"),
                            code = "koodi1",
                            updated = c("2026-08-01T06:00:00Z", "2026-08-01T06:00:00Z")) {
  list(table_url = urls, code_hash = code,
       source_updated = as.list(stats::setNames(updated, sub("/+$", "", urls))),
       built_at = "2026-08-01T05:01:00Z")
}

multi_updated <- function(id, updated = c("2026-08-01T06:00:00Z", "2026-08-01T06:00:00Z"),
                          urls = c("https://example.org/db/a.px/",
                                   "https://example.org/db/b.px/")) {
  stats::setNames(list(stats::setNames(updated, urls)), id)
}

test_that("usean taulun kuvio on ajan tasalla kun kaikki taulut ovat", {
  reg <- multi_registry_row("a")

  out <- visu_stale_charts(reg, list(a = multi_state_row()), multi_updated("a"))

  expect_false(out$stale)
  expect_equal(out$reason, "ajan tasalla")
})

test_that("yhden taulun paivittyminen vanhentaa usean taulun kuvion", {
  reg <- multi_registry_row("a")
  updated <- multi_updated("a", c("2026-08-01T06:00:00Z", "2026-08-30T06:00:00Z"))

  out <- visu_stale_charts(reg, list(a = multi_state_row()), updated)

  expect_true(out$stale)
  expect_equal(out$reason, "data p\u00e4ivittynyt")
})

test_that("yhden taulun tuntematon aikaleima riittaa uudelleenrakennukseen", {
  reg <- multi_registry_row("a")
  updated <- multi_updated("a", c("2026-08-01T06:00:00Z", NA_character_))

  out <- visu_stale_charts(reg, list(a = multi_state_row()), updated)

  expect_true(out$stale)
  expect_equal(out$reason, "aikaleima tuntematon")
})

test_that("taulun lisaaminen kuvioon vanhentaa sen", {
  reg <- multi_registry_row("a")
  state <- list(a = multi_state_row(urls = "https://example.org/db/a.px/",
                                    updated = "2026-08-01T06:00:00Z"))

  out <- visu_stale_charts(reg, state, multi_updated("a"))

  expect_true(out$stale)
  expect_equal(out$reason, "l\u00e4hdetaulu vaihtunut")
})

test_that("taulujen jarjestys ei vaikuta paatokseen", {
  urls <- c("https://example.org/db/b.px/", "https://example.org/db/a.px/")
  reg <- multi_registry_row("a", urls = urls)
  updated <- multi_updated("a", urls = urls)

  out <- visu_stale_charts(reg, list(a = multi_state_row()), updated)

  expect_false(out$stale)
})

test_that("usean taulun leimat kirjataan tilaan taulukohtaisesti", {
  reg <- multi_registry_row("a")
  updated <- multi_updated("a", c("2026-08-01T06:00:00Z", "2026-08-30T06:00:00Z"))
  decisions <- visu_stale_charts(reg, list(), updated)

  out <- visu:::visu_new_state(reg, updated, list(), decisions)

  expect_equal(out$a$table_url, c("https://example.org/db/a.px/",
                                  "https://example.org/db/b.px/"))
  expect_equal(out$a$source_updated,
               list("https://example.org/db/a.px" = "2026-08-01T06:00:00Z",
                    "https://example.org/db/b.px" = "2026-08-30T06:00:00Z"))
})

test_that("kirjattu tila kelpaa sellaisenaan seuraavan ajon vertailuun", {
  reg <- multi_registry_row("a")
  updated <- multi_updated("a")
  state <- visu:::visu_new_state(reg, updated, list(),
                                 visu_stale_charts(reg, list(), updated))

  out <- visu_stale_charts(reg, state, updated)

  expect_equal(out$reason, "ajan tasalla")
})
