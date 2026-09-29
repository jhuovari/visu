# Kertakaytto: tulostaa StatFin-taulujen muuttujat ja arvokoodit CI-lokiin,
# koska kehitysymparistosta ei paase rajapintaan. Ei osa pakettia.
kansiot <- c("kbar", "ntp", "tjt", "tjkt")
taulut <- c(
  "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/ntp/15a8.px/",
  "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/kbar/11cc.px/"
)

listaa_kansio <- function(k) {
  url <- paste0("https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/", k)
  cat("\n########## KANSIO", k, "\n")
  x <- try(jsonlite::fromJSON(url), silent = TRUE)
  if (inherits(x, "try-error")) { cat("  EI SAATU\n"); return(invisible()) }
  for (i in seq_len(nrow(x))) cat(sprintf("  %-14s %s\n", x$id[i], x$text[i]))
}

kuvaa_taulu <- function(url) {
  cat("\n########## TAULU", url, "\n")
  m <- try(jsonlite::fromJSON(url, simplifyDataFrame = FALSE), silent = TRUE)
  if (inherits(m, "try-error")) { cat("  EI SAATU\n"); return(invisible()) }
  cat("  title:", m$title, "\n")
  for (v in m$variables) {
    n <- length(v$values)
    cat(sprintf("  MUUTTUJA %-34s %s (%d arvoa)\n", v$code, v$text, n))
    naytto <- seq_len(min(n, 60))
    for (i in naytto) cat(sprintf("      %-30s %s\n", v$values[[i]], v$valueTexts[[i]]))
    if (n > 60) cat("      ... ja", n - 60, "muuta\n")
  }
}

for (k in kansiot) listaa_kansio(k)
for (u in taulut) kuvaa_taulu(u)
