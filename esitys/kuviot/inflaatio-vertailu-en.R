# Inflaatio, pohjainflaatio ja energiainflaatio Suomessa, euroalueella ja
# muissa Pohjoismaissa, brittienglanniksi. Kolme kuviota rinnakkain samalle
# dialle: kunkin leveys on kolmannes pohjan sisaltoalueesta (11.89 tuumaa).
#
# Kaikki sarjat ovat Eurostatin yhdenmukaistettua indeksia, josta vuosimuutos
# lasketaan kuten sivuston inflaatiosivulla. Muut Pohjoismaat on Ruotsin,
# Tanskan, Norjan ja Islannin vuosimuutosten yksinkertainen keskiarvo.
#
# Ajo: Rscript esitys/kuviot/inflaatio-vertailu-en.R [kansio]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
kansio <- if (length(args)) args[[1]] else "esitys/kuviot"

pohjoismaat <- c("SE", "DK", "NO", "IS")

hicp <- visu_get_eurostat(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/prc_hicp_minr/M.I25.TOTAL+TOT_X_NRG_FOOD+NRG.FI+EA20+SE+DK+NO+IS"
) |>
  group_by(coicop18, geo) |>
  mutate(values = visu_change(values, time)) |>
  ungroup() |>
  filter(!is.na(values)) |>
  transmute(time, coicop18 = as.character(coicop18), sarja = as.character(geo), values)

# Keskiarvo vain kuukausilta, joilta kaikkien neljan maan luku on, jotta sen
# koostumus ei vaihtele.
nordic <- hicp |>
  filter(sarja %in% pohjoismaat) |>
  group_by(time, coicop18) |>
  filter(n() == length(pohjoismaat)) |>
  summarise(values = mean(values), .groups = "drop") |>
  mutate(sarja = "NORD")

# Suomi ensin, jolloin se saa paletin ensimmaisen varin kuten sivustolla.
sarjat <- c(FI = "Finland", EA20 = "Euro area", NORD = "Other Nordics")
d_kaikki <- bind_rows(filter(hicp, sarja %in% c("FI", "EA20")), nordic) |>
  mutate(sarja = factor(sarja, levels = names(sarjat), labels = sarjat))

lahde <- "Source: Eurostat. Other Nordics: simple average\nof Sweden, Denmark, Norway and Iceland"

kuviot <- list(
  inflation = list(
    coicop = "TOTAL",
    title = "Inflation, %",
    subtitle = "Annual change, HICP"
  ),
  core = list(
    coicop = "TOT_X_NRG_FOOD",
    title = "Core inflation, %",
    # Eurostatin luokituksessa elintarvikkeisiin kuuluvat alkoholi ja tupakka.
    subtitle = "HICP excl. energy and food"
  ),
  energy = list(
    coicop = "NRG",
    title = "Energy inflation, %",
    subtitle = "Annual change, HICP energy"
  )
)

leveys <- 11.89 / 3
korkeus <- 4.6

for (nimi in names(kuviot)) {
  k <- kuviot[[nimi]]
  p <- visu_plot(filter(d_kaikki, coicop18 == k$coicop), colour = "sarja",
                 title = k$title, subtitle = k$subtitle, caption = lahde,
                 lang = "en", zeroline = TRUE, start = "2020-01-01")
  p <- visu_restyle(p, legend = "bottom", scale = 1.15, width = leveys,
                    linewidth = 1.5, height = korkeus) +
    # Selite koko kuvion vasemmasta reunasta eika piirtoalueen: kapeassa
    # kuviossa y-akselin levyinen tila ratkaisee, mahtuuko kolmas sarja.
    ggplot2::theme(legend.location = "plot")
  tiedosto <- file.path(kansio, paste0("inflaatio-", nimi, "-en.png"))
  ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
  cat("Tallennettu:", tiedosto, "\n")
}
