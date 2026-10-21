# Kaytettavissa olevan tulon reaalimuutos erittain trendisarjoista,
# esityskoossa. Sama kuvio kuin sivuston ostovoima-erat-reaalinen
# (site/kuviot/ostovoima.qmd), mutta kausitasoitettujen sarjojen sijaan
# Tilastokeskuksen trendisarjat. Myos deflaattorina kaytettava yksityisen
# kulutuksen hinta lasketaan trendisarjoista.
#
# Ulkoasu kuten visu_chart_png():lla: ggcustomin VM-teema, tekstit 1.15-
# kertaisina ja viivan paksuus VM-maarittelyn 1.5.
#
# Ajo: Rscript esitys/kuviot/ostovoima-erat-reaalinen-trendi.R [tiedosto.png]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
tiedosto <- if (length(args)) args[[1]] else
  "esitys/kuviot/ostovoima-erat-reaalinen-trendi.png"

tulot <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/ntp/15a6.px/",
  query = list(
    "timeperiod_q" = "*",
    "sektoriluokitus_7_20230101" = "S14",
    "taloustoimi_1_20180101" = c("B6N", "B13N", "D1R", "D4R", "D62R", "D4K"),
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

# Yksityisen kulutuksen implisiittinen hinta: kayvin hinnoin / volyymi.
hinta <- kulutus |>
  group_by(time) |>
  summarise(hinta = values[contentscode == "trendicp"] /
                    values[contentscode == "trendivv2015"], .groups = "drop")

# Paaerat kuten sivulla: omaisuustulot, asunto- ja yrittajatulo yhdeksi
# netoksi. Verot ja maksut tulevat jaannosterana.
tulot_paaerat <- bind_rows(
  filter(tulot, taloustoimi_1_20180101 %in% c("B6N", "D1R", "D62R")),
  tulot |>
    filter(taloustoimi_1_20180101 %in% c("D4R", "B13N", "D4K")) |>
    group_by(time) |>
    summarise(values = sum(values * if_else(taloustoimi_1_20180101 == "D4K", -1, 1)),
              .groups = "drop") |>
    mutate(taloustoimi_1_20180101 = "D4")
)

# Hinnan kontribuutio on reaalisen ja nimellisen muutoksen erotus, jolloin
# pylvaat summautuvat tasan reaalimuutokseen.
reaalitulo <- tulot |>
  filter(taloustoimi_1_20180101 == "B6N") |>
  inner_join(hinta, by = "time") |>
  transmute(time,
            nimellinen = visu_change(values, time),
            reaalinen = visu_change(values / hinta, time)) |>
  filter(!is.na(reaalinen))

t <- list(
  otsikko = "Reaalitulon muutos erittäin, %-yksikköä",
  selite = "Kontribuutio vuodentakaiseen verrattuna, trendi",
  lahde = "Lähde: Tilastokeskus, neljännesvuosittaiset sektoritilit ja neljännesvuositilinpito",
  jaannos = "Verot ja maksut, netto",
  yhteensa = "Reaalitulo",
  hinnat = "Hinnat",
  sarjat = c(D1R = "Palkat", D4 = "Omaisuus- ja muut tulot, netto",
             D62R = "Saadut etuudet")
)

nimet <- c(t$sarjat, stats::setNames(c(t$jaannos, t$hinnat), c(t$jaannos, t$hinnat)))
hintaera <- reaalitulo |>
  transmute(time, taloustoimi_1_20180101 = t$hinnat,
            values = reaalinen - nimellinen)
d <- visu_contributions(tulot_paaerat, series = "taloustoimi_1_20180101",
                        total = "B6N", lag = 4, residual = t$jaannos) |>
  bind_rows(hintaera) |>
  mutate(era = factor(taloustoimi_1_20180101, levels = names(nimet), labels = nimet))
viiva <- transmute(reaalitulo, time, values = reaalinen)

p <- visu_plot(d, colour = "era", type = "col", stack = TRUE,
               line = viiva, line_label = t$yhteensa, title = t$otsikko,
               subtitle = t$selite, caption = t$lahde, lang = "fi")

# Samat mitat ja tyyli kuin visu_chart_png():n oletuksilla.
leveys <- 6.28
korkeus <- 4.49
p <- visu_restyle(p, legend = "bottom", scale = 1.15, width = leveys,
                  linewidth = 1.5, height = korkeus) +
  # Kuusi selitetta oikeassa reunassa veisi puolet piirtoalasta, joten erat
  # ladotaan alas kahteen sarakkeeseen ja reaalitulon viiva niiden alle.
  ggplot2::guides(fill = ggplot2::guide_legend(order = 1, ncol = 2),
                  colour = ggplot2::guide_legend(order = 2)) +
  ggplot2::theme(legend.margin = ggplot2::margin(0, 0, 0, 0),
                 legend.spacing.y = ggplot2::unit(4, "pt"))
ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
cat("Tallennettu:", tiedosto, "\n")
