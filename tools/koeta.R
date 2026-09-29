# Kertakaytto: mitka taloustoimet kantavat dataa sektorille S14.
library(visu)
kandidaatit <- c("D41K", "D411K", "D412K", "D4K", "D5K", "D51K",
                 "D61K", "D62R", "D1R", "B6N", "B7N")

d <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/ntp/15a6.px/",
  query = list(
    "timeperiod_q" = "*",
    "sektoriluokitus_7_20230101" = "S14",
    "taloustoimi_1_20180101" = kandidaatit,
    "contentscode" = "ntp-KAUSIT"
  )
)
cat("sarakkeet:", paste(names(d), collapse = ", "), "\n\n")
for (k in kandidaatit) {
  v <- d$values[d$taloustoimi_1_20180101 == k]
  v <- v[!is.na(v)]
  if (length(v) == 0) { cat(sprintf("  %-7s EI DATAA\n", k)); next }
  cat(sprintf("  %-7s n=%3d  min=%10.0f  max=%10.0f  viim=%s\n",
              k, length(v), min(v), max(v),
              paste(round(tail(v, 4)), collapse = " ")))
}
