# Rakentaa väliaikaisen sivuston, jotta rekisteri- ja tilatestit eivät
# riipu repositorion oikeista kuvioista.
make_site <- function(charts) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "kuviot"), recursive = TRUE)
  for (id in names(charts)) {
    # useBytes pitaa UTF-8-tavut ennallaan myos C-localessa.
    writeLines(charts[[id]], file.path(dir, "kuviot", paste0(id, ".qmd")), useBytes = TRUE)
  }
  dir
}

chart_qmd <- function(url, title = "Testikuvio", body_url = url) {
  c(
    "---",
    paste0("title: \"", title, "\""),
    "visu:",
    paste0("  table_url: \"", url, "\""),
    "---",
    "",
    "```{r}",
    paste0("dat <- pxwebtools::pxw_get_data(url = \"", body_url, "\")"),
    "```"
  )
}
