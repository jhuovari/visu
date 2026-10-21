# Inflaatio, pohjainflaatio ja energiainflaatio Suomessa ja euroalueella,
# brittienglanniksi. Kolme kuviota rinnakkain samalle dialle: kunkin leveys on
# kolmannes pohjan sisaltoalueesta (11.89 tuumaa).
#
# Euroalue ja Suomi ovat Eurostatin yhdenmukaistettua indeksia, josta
# vuosimuutos lasketaan kuten sivuston inflaatiosivulla. Ensimmaisessa
# kuviossa on lisaksi Tilastokeskuksen kansallisen kuluttajahintaindeksin
# vuosimuutos.
#
# Ajo: Rscript esitys/kuviot/inflaatio-vertailu-en.R [kansio]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
kansio <- if (length(args)) args[[1]] else "esitys/kuviot"

hicp <- visu_get_eurostat(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/prc_hicp_minr/M.I25.TOTAL+TOT_X_NRG_FOOD+NRG.FI+EA20"
) |>
  group_by(coicop18, geo) |>
  mutate(values = visu_change(values, time)) |>
  ungroup() |>
  filter(!is.na(values)) |>
  transmute(time, coicop18 = as.character(coicop18), sarja = as.character(geo), values)

khi <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/khi/15b5.px/",
  query = list(
    "timeperiod_m" = "*",
    "coicop_46_20231201" = "SSS",
    "contentscode" = "vm_khi"
  )
)

# Suomi ensin, jolloin se saa paletin ensimmaisen varin kuten sivustolla.
sarjat <- c(FI = "Finland", EA20 = "Euro area", CPI = "CPI, Finland")

kuviot <- list(
  inflation = list(
    data = bind_rows(
      filter(hicp, coicop18 == "TOTAL"),
      transmute(filter(khi, !is.na(values)), time, sarja = "CPI", values)
    ),
    title = "Inflation, %",
    subtitle = "Annual change, HICP and CPI",
    caption = "Source: Eurostat, Statistics Finland"
  ),
  core = list(
    data = filter(hicp, coicop18 == "TOT_X_NRG_FOOD"),
    title = "Core inflation, %",
    # Eurostatin luokituksessa elintarvikkeisiin kuuluvat alkoholi ja tupakka.
    subtitle = "HICP excl. energy and food",
    caption = "Source: Eurostat"
  ),
  energy = list(
    data = filter(hicp, coicop18 == "NRG"),
    title = "Energy inflation, %",
    subtitle = "Annual change, HICP energy",
    caption = "Source: Eurostat"
  )
)

leveys <- 11.89 / 3
korkeus <- 4.6

for (nimi in names(kuviot)) {
  k <- kuviot[[nimi]]
  d <- mutate(k$data, sarja = factor(sarja, levels = intersect(names(sarjat), sarja),
                                     labels = sarjat[intersect(names(sarjat), sarja)]))
  p <- visu_plot(d, colour = "sarja", title = k$title, subtitle = k$subtitle,
                 caption = k$caption, lang = "en", zeroline = TRUE,
                 start = "2020-01-01")
  p <- visu_restyle(p, legend = "bottom", scale = 1.15, width = leveys,
                    linewidth = 1.5, height = korkeus)
  tiedosto <- file.path(kansio, paste0("inflaatio-", nimi, "-en.png"))
  ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
  cat("Tallennettu:", tiedosto, "\n")
}
