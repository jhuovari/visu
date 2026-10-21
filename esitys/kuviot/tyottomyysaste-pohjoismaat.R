# Tyottomyysaste Suomessa, euroalueella ja muissa Pohjoismaissa, esityskoossa
# ja englanniksi. Kaikki maat Eurostatin kuukausittaisesta trendisarjasta
# (TC), jolloin ne on
# laskettu samalla menetelmalla. Suomi ja euroalue paksulla viivalla, Ruotsi,
# Tanska, Norja ja Islanti ohuella.
#
# Euroalue on EA21: tyottomyysaineistossa (une_rt_m) EA20-koodia ei enaa ole.
#
# Ajo: Rscript esitys/kuviot/tyottomyysaste-pohjoismaat.R [tiedosto.png]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
tiedosto <- if (length(args)) args[[1]] else
  "esitys/kuviot/tyottomyysaste-pohjoismaat.png"

# Suomi ensin: se saa paletin ensimmaisen varin ja piirtyy paallimmaiseksi.
maat <- c(FI = "Finland", EA21 = "Euro area", SE = "Sweden", DK = "Denmark",
          NO = "Norway", IS = "Iceland")

tyottomyys <- visu_get_eurostat(
  paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/une_rt_m/M.TC.TOTAL.PC_ACT.T.",
         paste(names(maat), collapse = "+"))
) |>
  filter(!is.na(values)) |>
  mutate(maa = factor(as.character(geo), levels = names(maat), labels = maat))

# Viivan paksuus ennen esitysskaalausta: visu_restyle() kertoo sen niin, etta
# perusviiva 0.8 on esityksessa 1.5.
paksuus <- stats::setNames(c(0.8, 0.8, 0.35, 0.35, 0.35, 0.35), maat)

# VM-paletin varit kiinnitettyina maihin. Islannille ei anneta paletin
# kuudetta turkoosia, joka on lahes Suomen vari.
varit <- stats::setNames(c("#006475", "#c48903", "#365ABD", "#0098e8",
                           "#1B396D", "#4E8E7C"), maat)

p <- visu_plot(tyottomyys, colour = "maa", linewidth = "maa",
               title = "Unemployment rate, aged 15–74, %",
               subtitle = "Trend",
               caption = "Source: Eurostat", lang = "en")
# Sama sarake varina ja paksuutena, joten selite yhdistyy yhdeksi.
p <- suppressMessages(
  p +
    ggplot2::scale_linewidth_manual(values = paksuus, breaks = unname(maat)) +
    ggplot2::scale_colour_manual(values = varit, breaks = unname(maat))
)

# Samat mitat ja tyyli kuin visu_chart_png():n oletuksilla.
leveys <- 6.28
korkeus <- 4.49
p <- visu_restyle(p, legend = "auto", scale = 1.15, width = leveys,
                  linewidth = 1.5, height = korkeus, top_first = TRUE)
ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
cat("Tallennettu:", tiedosto, "\n")
