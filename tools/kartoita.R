# Kertakaytto: vain taustamuuttujat ja valitut taloustoimet.
base <- "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/"
meta <- function(t) jsonlite::fromJSON(paste0(base, t), simplifyDataFrame = FALSE)

for (t in c("kbar/129h.px", "kbar/11vq.px", "kbar/11vr.px")) {
  m <- try(meta(t), silent = TRUE)
  cat("\n#####", t, "\n")
  if (inherits(m, "try-error")) { cat("  EI SAATU\n"); next }
  for (v in m$variables) {
    if (grepl("^timeperiod|^contentscode", v$code)) next
    cat(sprintf("  %s -- %s (%d)\n", v$code, v$text, length(v$values)))
    for (i in seq_along(v$values)) {
      cat(sprintf("      %-16s %s\n", v$values[[i]], v$valueTexts[[i]]))
    }
  }
}

cat("\n##### ntp/15a6.px taloustoimet joita etsitaan\n")
m <- try(meta("ntp/15a6.px"), silent = TRUE)
if (!inherits(m, "try-error")) {
  v <- Filter(function(x) grepl("taloustoimi", x$code), m$variables)[[1]]
  koodit <- unlist(v$values); tekstit <- unlist(v$valueTexts)
  etsi <- c("B6N", "B7N", "B8N", "D1R", "D5K", "D51K", "D61K", "D62R", "D63R",
            "D41K", "D41R", "D42R", "D7K", "D7R", "P31K", "P3K")
  for (k in etsi) {
    i <- match(k, koodit)
    cat(sprintf("  %-8s %s\n", k, if (is.na(i)) "-- EI OLE --" else tekstit[i]))
  }
}
