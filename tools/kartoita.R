# Kertakaytto: valittujen taulujen muuttujat. Suuresta taloustoimi-listasta
# naytetaan vain ostovoiman kannalta kiinnostavat koodit.
base <- "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/"
kiinnostavat <- "^(B6N|B7N|D1|D11|D12|D4|D41|D42|D44|D5|D61|D62|D63|D7|P3|P31|B8)"

kuvaa <- function(taulu, maxv = 45) {
  url <- paste0(base, taulu, "/")
  cat("\n##### ", taulu, "\n")
  m <- try(jsonlite::fromJSON(url, simplifyDataFrame = FALSE), silent = TRUE)
  if (inherits(m, "try-error")) { cat("  EI SAATU\n"); return(invisible()) }
  cat("  ", m$title, "\n")
  for (v in m$variables) {
    n <- length(v$values)
    if (grepl("^timeperiod", v$code)) {
      cat(sprintf("  AIKA %s (%s ... %s)\n", v$code, v$values[[1]], v$values[[n]]))
      next
    }
    koodit <- unlist(v$values); tekstit <- unlist(v$valueTexts)
    if (grepl("taloustoimi", v$code) && n > maxv) {
      pidä <- grepl(kiinnostavat, koodit)
      cat(sprintf("  MUUTTUJA %s -- %s (%d arvoa, suodatettu %d)\n",
                  v$code, v$text, n, sum(pidä)))
      koodit <- koodit[pidä]; tekstit <- tekstit[pidä]
    } else {
      cat(sprintf("  MUUTTUJA %s -- %s (%d)\n", v$code, v$text, n))
    }
    k <- seq_len(min(length(koodit), maxv))
    for (i in k) cat(sprintf("      %-14s %s\n", koodit[i], tekstit[i]))
    if (length(koodit) > maxv) cat("      ...", length(koodit) - maxv, "muuta\n")
  }
}

for (t in c("kbar/129h.px", "kbar/11vq.px", "ntp/15a7.px", "ntp/15a6.px")) kuvaa(t)
