# Kertakaytto: pelkat kansiolistaukset, jotta loki ei katkea.
base <- "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/"
for (k in c("kbar", "ntp")) {
  cat("\n##### KANSIO", k, "\n")
  x <- try(jsonlite::fromJSON(paste0(base, k)), silent = TRUE)
  if (inherits(x, "try-error")) { cat("  EI SAATU\n"); next }
  for (i in seq_len(nrow(x))) cat(sprintf("  %-10s %s\n", x$id[i], x$text[i]))
}
