# Inflaatio, pohjainflaatio ja energiainflaatio Suomessa ja euroalueella,
# brittienglanniksi. Kolme kuviota rinnakkain samalle dialle: kunkin leveys on
# kolmannes pohjan sisaltoalueesta (11.89 tuumaa).
#
# Euroalue ja Suomi ovat Eurostatin yhdenmukaistettua indeksia, josta
# vuosimuutos lasketaan kuten sivuston inflaatiosivulla. Ensimmaisessa
# kuviossa on lisaksi Tilastokeskuksen kansallisen kuluttajahintaindeksin
# vuosimuutos. Energiainflaation kuviossa on vertailuna muiden Pohjoismaiden
# (Ruotsi, Tanska, Norja, Islanti) yksinkertainen keskiarvo ohuemmalla
# viivalla.
#
# Ajo: Rscript esitys/kuviot/inflaatio-vertailu-en.R [kansio]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
kansio <- if (length(args)) args[[1]] else "esitys/kuviot"

hicp <- visu_get_eurostat(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/prc_hicp_minr/M.I25.TOTAL+TOT_X_NRG_FOOD+NRG.FI+EA20+SE+DK+NO+IS"
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

# Muiden Pohjoismaiden energiainflaatio yksinkertaisena keskiarvona. Vain
# kuukaudet, joilta kaikkien nelja maan luku on, jotta keskiarvon koostumus
# ei vaihtele.
pohjoismaat <- c("SE", "DK", "NO", "IS")
nordic <- hicp |>
  filter(coicop18 == "NRG", sarja %in% pohjoismaat) |>
  group_by(time, coicop18) |>
  filter(n() == length(pohjoismaat)) |>
  summarise(values = mean(values), .groups = "drop") |>
  mutate(sarja = "NORD")

# Suomi ensin, jolloin se saa paletin ensimmaisen varin kuten sivustolla.
sarjat <- c(FI = "Finland", EA20 = "Euro area", CPI = "CPI, Finland",
            NORD = "Other Nordics")
paa <- c("FI", "EA20")

# Viivan paksuus ennen esitysskaalausta: visu_restyle() kertoo sen niin, etta
# perusviiva 0.8 on esityksessa 1.5. Vertailusarja on ohuempi.
paksuus <- c(FI = 0.8, EA20 = 0.8, NORD = 0.45)

# VM-paletin varit kiinnitettyina sarjoihin, koska piirtojarjestys (alla)
# poikkeaa selitteen jarjestyksesta.
varit <- c(FI = "#006475", EA20 = "#c48903", NORD = "#365ABD")

kuviot <- list(
  inflation = list(
    data = bind_rows(
      filter(hicp, coicop18 == "TOTAL", sarja %in% paa),
      transmute(filter(khi, !is.na(values)), time, sarja = "CPI", values)
    ),
    title = "Inflation, %",
    subtitle = "Annual change, HICP and CPI",
    caption = "Source: Eurostat, Statistics Finland"
  ),
  core = list(
    data = filter(hicp, coicop18 == "TOT_X_NRG_FOOD", sarja %in% paa),
    title = "Core inflation, %",
    # Eurostatin luokituksessa elintarvikkeisiin kuuluvat alkoholi ja tupakka.
    subtitle = "HICP excl. energy and food",
    caption = "Source: Eurostat"
  ),
  energy = list(
    data = bind_rows(filter(hicp, coicop18 == "NRG", sarja %in% paa), nordic),
    title = "Energy inflation, %",
    subtitle = "Annual change, HICP energy",
    caption = "Source: Eurostat. Other Nordics: simple average\nof Sweden, Denmark, Norway and Iceland",
    ohuet = TRUE
  )
)

leveys <- 11.89 / 3
korkeus <- 4.6

for (nimi in names(kuviot)) {
  k <- kuviot[[nimi]]
  d <- mutate(k$data, sarja = factor(sarja, levels = intersect(names(sarjat), sarja),
                                     labels = sarjat[intersect(names(sarjat), sarja)]))
  ohuet <- isTRUE(k$ohuet)
  if (ohuet) {
    # ggplot piirtaa ryhmat tasojen jarjestyksessa, joten ohuet ensin ja
    # korostetut niiden paalle. Selitteen jarjestys annetaan erikseen.
    jarjestys <- c(setdiff(names(paksuus), paa), paa)
    d <- mutate(k$data, sarja = factor(sarja, levels = jarjestys,
                                       labels = sarjat[jarjestys]))
  }
  p <- visu_plot(d, colour = "sarja", title = k$title, subtitle = k$subtitle,
                 caption = k$caption, lang = "en", zeroline = TRUE,
                 start = "2020-01-01", linewidth = if (ohuet) "sarja" else NULL)
  if (ohuet) {
    # Sama sarake varina ja paksuutena, joten selite yhdistyy yhdeksi.
    nimet <- unname(sarjat[names(paksuus)])
    p <- suppressMessages(
      p +
        ggplot2::scale_linewidth_manual(values = stats::setNames(paksuus, nimet),
                                        breaks = nimet) +
        ggplot2::scale_colour_manual(values = stats::setNames(varit[names(paksuus)], nimet),
                                     breaks = nimet)
    )
  }
  p <- visu_restyle(p, legend = "bottom", scale = 1.15, width = leveys,
                    linewidth = 1.5, height = korkeus)
  # Selite koko kuvion vasemmasta reunasta eika piirtoalueen: kapeassa
  # kuviossa y-akselin levyinen tila ratkaisee, mahtuuko kolmas sarja.
  if (ohuet) p <- p + ggplot2::theme(legend.location = "plot")
  tiedosto <- file.path(kansio, paste0("inflaatio-", nimi, "-en.png"))
  ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
  cat("Tallennettu:", tiedosto, "\n")
}
