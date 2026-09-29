# Kertakaytto: tulostaa StatFin-taulujen rakenteen CI-lokiin. Aikamuuttujien
# arvoja ei tulosteta, koska ne tayttaisivat lokin.
base <- "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/"

kansio <- function(k) {
  x <- try(jsonlite::fromJSON(paste0(base, k)), silent = TRUE)
  if (inherits(x, "try-error")) return(NULL)
  x
}

listaa <- function(k) {
  x <- kansio(k)
  cat("\n##### KANSIO", k, "\n")
  if (is.null(x)) { cat("  EI SAATU\n"); return(invisible(NULL)) }
  for (i in seq_len(nrow(x))) cat(sprintf("  %-10s %s\n", x$id[i], x$text[i]))
  invisible(x)
}

kuvaa <- function(url, maxv = 40) {
  cat("\n##### TAULU", url, "\n")
  m <- try(jsonlite::fromJSON(url, simplifyDataFrame = FALSE), silent = TRUE)
  if (inherits(m, "try-error")) { cat("  EI SAATU\n"); return(invisible()) }
  cat("  ", m$title, "\n")
  for (v in m$variables) {
    n <- length(v$values)
    if (grepl("^timeperiod|^Vuosi$|^Kuukausi$", v$code)) {
      cat(sprintf("  AIKA %s (%d arvoa, %s ... %s)\n", v$code, n,
                  v$values[[1]], v$values[[n]]))
      next
    }
    cat(sprintf("  MUUTTUJA %s -- %s (%d)\n", v$code, v$text, n))
    for (i in seq_len(min(n, maxv))) {
      cat(sprintf("      %-26s %s\n", v$values[[i]], v$valueTexts[[i]]))
    }
    if (n > maxv) cat("      ...", n - maxv, "muuta\n")
  }
}

kb <- listaa("kbar")
nt <- listaa("ntp")

# Taustamuuttujataulut kuluttajabarometrista ja tulotaulut sektoritileilta.
poimi <- function(x, k, hae) {
  if (is.null(x)) return(character())
  osumat <- x$id[grepl(hae, x$text, ignore.case = TRUE)]
  paste0(base, k, "/", osumat, "/")
}

for (u in poimi(kb, "kbar", "luottamus|taustamuuttuj|väestö")) kuvaa(u)
for (u in poimi(nt, "ntp", "kotitalou|tulot|sektoritil")) kuvaa(u)
