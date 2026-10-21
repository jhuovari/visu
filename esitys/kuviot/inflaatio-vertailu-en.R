# Inflaatio, pohjainflaatio ja energiainflaatio Suomessa ja euroalueella,
# brittienglanniksi. Kolme kuviota rinnakkain samalle dialle: kunkin leveys on
# kolmannes pohjan sisaltoalueesta (11.89 tuumaa).
#
# Euroalue ja Suomi ovat Eurostatin yhdenmukaistettua indeksia, josta
# vuosimuutos lasketaan kuten sivuston inflaatiosivulla. Ensimmaisessa
# kuviossa on lisaksi Tilastokeskuksen kansallisen kuluttajahintaindeksin
# vuosimuutos. Energiainflaation kuviossa ovat vertailuna myos Ruotsi,
# Tanska, Norja ja Islanti ohuemmalla viivalla.
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

# Suomi ensin, jolloin se saa paletin ensimmaisen varin kuten sivustolla.
sarjat <- c(FI = "Finland", EA20 = "Euro area", CPI = "CPI, Finland",
            SE = "Sweden", DK = "Denmark", NO = "Norway", IS = "Iceland")
paa <- c("FI", "EA20")

# Viivan paksuus ennen esitysskaalausta: visu_restyle() kertoo sen niin, etta
# perusviiva 0.8 on esityksessa 1.5. Pohjoismaat ovat taustalla ohuina.
paksuus <- c(FI = 0.8, EA20 = 0.8, SE = 0.35, DK = 0.35, NO = 0.35, IS = 0.35)

# VM-paletin varit kiinnitettyina maihin. Islannille ei anneta paletin
# kuudetta turkoosia, joka on lahes Suomen vari.
varit <- c(FI = "#006475", EA20 = "#c48903", SE = "#365ABD", DK = "#0098e8",
           NO = "#1B396D", IS = "#4E8E7C")

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
    data = filter(hicp, coicop18 == "NRG"),
    title = "Energy inflation, %",
    subtitle = "Annual change, HICP energy",
    caption = "Source: Eurostat",
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
    # Sama sarake varina ja paksuutena, joten selite yhdistyy yhdeksi; kuusi
    # sarjaa ei mahdu kapean kuvion alareunaan yhdelle riville.
    nimet <- unname(sarjat[names(paksuus)])
    p <- suppressMessages(
      p +
        ggplot2::scale_linewidth_manual(values = stats::setNames(paksuus, nimet),
                                        breaks = nimet) +
        ggplot2::scale_colour_manual(values = stats::setNames(varit[names(paksuus)], nimet),
                                     breaks = nimet) +
        # Norjan huippu venyttaa asteikon, ja oletuksena akselille jaisi vain
        # 0 ja 50.
        ggplot2::scale_y_continuous(breaks = seq(-50, 150, 25))
    ) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2),
                      linewidth = ggplot2::guide_legend(nrow = 2))
  }
  p <- visu_restyle(p, legend = "bottom", scale = 1.15, width = leveys,
                    linewidth = 1.5, height = korkeus)
  tiedosto <- file.path(kansio, paste0("inflaatio-", nimi, "-en.png"))
  ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
  cat("Tallennettu:", tiedosto, "\n")
}
