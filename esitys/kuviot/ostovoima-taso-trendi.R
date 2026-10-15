# Kotitalouksien kaytettavissa oleva tulo trendisarjana, esityskoossa.
# Sama kuvio kuin sivuston ostovoima-taso, mutta kausitasoitetun sarjan
# sijaan Tilastokeskuksen trendisarjat. Reaalinen sarja on deflatoitu
# yksityisen kulutuksen implisiittisella hinnalla, joka lasketaan nyt
# trendisarjoista (kaypa hinta / volyymi, viitevuosi 2015).
#
# Ajo: Rscript esitys/kuviot/ostovoima-taso-trendi.R [tiedosto.png]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
tiedosto <- if (length(args)) args[[1]] else "esitys/kuviot/ostovoima-taso-trendi.png"

tulo <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/ntp/15a6.px/",
  query = list(
    "timeperiod_q" = "*",
    "sektoriluokitus_7_20230101" = "S14",
    "taloustoimi_1_20180101" = "B6N",
    "contentscode" = "ntp-TRENDI"
  )
)

kulutus <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/ntp/132h.px/",
  query = list(
    "timeperiod_q" = "*",
    "taloustoimi_1_20180101" = "P3KS14_S15",
    "contentscode" = c("trendicp", "trendivv2015")
  )
)

hinta <- kulutus |>
  group_by(time) |>
  summarise(hinta = values[contentscode == "trendicp"] /
                    values[contentscode == "trendivv2015"], .groups = "drop")

d <- inner_join(select(tulo, time, values), hinta, by = "time")
ostovoima <- bind_rows(
  transmute(d, time, sarja = "Nimellinen", values),
  transmute(d, time, sarja = "Reaalinen", values = values / hinta)
) |>
  mutate(sarja = factor(sarja, levels = c("Nimellinen", "Reaalinen")))

p <- visu_plot(
  ostovoima, colour = "sarja",
  title = "Kotitalouksien käytettävissä oleva tulo, milj. euroa",
  subtitle = "Trendi, reaalinen vuoden 2015 hinnoin",
  caption = "Lähde: Tilastokeskus, neljännesvuosittaiset sektoritilit ja neljännesvuositilinpito",
  lang = "fi"
)

# Samat mitat ja tyyli kuin visu_chart_png():n oletuksilla.
leveys <- 6.28
korkeus <- 4.49
p <- visu_restyle(p, legend = "auto", scale = 1.15, width = leveys)
ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
cat("Tallennettu:", tiedosto, "\n")
