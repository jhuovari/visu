# Ulkomaan meriliikenne ja Tullin tavarakauppa rinnakkain, esityskoossa.
# Meriliikenteen tonnit (Tilastokeskus) verrataan Tullin vienti- ja
# tuontiarvoon, joka on deflatoitu Tilastokeskuksen vienti- ja
# tuontihintaindeksillä. Molemmista 12 kuukauden liukuva summa, joka
# poistaa kausivaihtelun, ja summa indeksoidaan vuoteen 2021 = 100.
#
# Ulkoasu kuten visu_chart_png():lla: ggcustomin VM-teema, tekstit 1.15-
# kertaisina ja viivan paksuus VM-määrittelyn 1.5.
#
# Ajo: Rscript esitys/kuviot/meriliikenne-tulli.R
# Tallentaa meriliikenne-tulli-vienti.png ja meriliikenne-tulli-tuonti.png.

library(visu)
library(dplyr)

meri <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/uvliik/12is.px/",
  query = list(
    "satama_2_20200101" = "SSSSS",
    "tavaralaji_20_20200101" = "SSS",
    "suunta_10_20200101" = c("000", "001"),
    "timeperiod_m" = "*",
    "contentscode" = "uvliik-tonn"
  )
) |>
  transmute(time, suunta = if_else(suunta_10_20200101 == "000", "vienti", "tuonti"),
            sarja = "meri", values)

hinnat <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/thi/13m8.px/",
  query = list(
    "cpa_6_20180101" = "SSS",
    "timeperiod_m" = "*",
    "thi_hinkasind_1_20180101" = c("4", "5"),
    "contentscode" = "thi-pisteluku21"
  )
) |>
  transmute(time, suunta = if_else(thi_hinkasind_1_20180101 == "4", "vienti", "tuonti"),
            hinta = values)

# Tullin kuutio on jaettu kahtia (ks. site/kuviot/ulkomaantalous.qmd):
# nykyinen alkaa vuodesta 2025, pidempi historia on rinnakkaisessa.
kysely <- list(
  `SITC Products` = "=ALL", `Time period` = "=ALL", Flow = c(1, 2),
  `Continents and groups` = "00", Indicators = "V1"
)
tulli <- bind_rows(
  visu_get_uljas("/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC",
                 classifiers = kysely),
  visu_get_uljas("/DATABASE/01 ULKOMAANKAUPPATILASTOT/02 SITC/ULJAS_SITC2",
                 classifiers = kysely)
) |>
  distinct(time, Flow, .keep_all = TRUE) |>
  transmute(time, suunta = if_else(Flow == "2", "vienti", "tuonti"), values) |>
  inner_join(hinnat, by = c("time", "suunta")) |>
  transmute(time, suunta, sarja = "tulli", values = values / hinta)

liukuva_summa <- function(x) as.numeric(stats::filter(x, rep(1, 12), sides = 1))

# Vain kuukaudet, joilta on kaikki sarjat, jotta viivat päättyvät samaan.
# Joulukuun 2021 liukuva summa on vuoden 2021 summa, joten jakaminen sillä
# antaa indeksin 2021 = 100.
d <- bind_rows(meri, tulli) |>
  group_by(time, suunta) |>
  filter(n() == 2) |>
  group_by(suunta, sarja) |>
  arrange(time, .by_group = TRUE) |>
  mutate(values = liukuva_summa(values),
         values = 100 * values / values[time == as.Date("2021-12-01")]) |>
  ungroup() |>
  filter(!is.na(values))

t <- list(
  vienti = "Tavaravienti, volyymi-indeksi 2021 = 100",
  tuonti = "Tavaratuonti, volyymi-indeksi 2021 = 100",
  selite = "12 kuukauden liukuva summa",
  lahde = paste0("Lähde: Tilastokeskus, ulkomaan meriliikenne ja tuottajahintaindeksit; ",
                 "Tulli, tavaroiden ulkomaankauppa"),
  sarjat = c(tulli = "Tulli, arvo deflatoituna", meri = "Meriliikenne, tonnit")
)

leveys <- 6.28
korkeus <- 4.49
for (s in c("vienti", "tuonti")) {
  ds <- d |>
    filter(suunta == s) |>
    mutate(sarja = factor(sarja, levels = names(t$sarjat), labels = t$sarjat))
  p <- visu_plot(ds, colour = "sarja", title = t[[s]], subtitle = t$selite,
                 caption = t$lahde, lang = "fi")
  p <- visu_restyle(p, legend = "auto", scale = 1.15, width = leveys,
                    linewidth = 1.5, height = korkeus)
  tiedosto <- sprintf("esitys/kuviot/meriliikenne-tulli-%s.png", s)
  ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
  cat("Tallennettu:", tiedosto, "\n")
}
