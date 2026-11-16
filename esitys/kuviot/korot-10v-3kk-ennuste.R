# Suomen valtion 10 vuoden viitelainan korko ja 3 kuukauden euribor seka
# VM:n ennuste samoille koroille, esityskoossa.
#
# Toteutuneet korot ovat Suomen Pankin kuukausikeskiarvoja. Euribor tulee
# sivuston kerryttamasta paivasarjasta (site/data/euribor.csv), viitelainan
# korko raporttipalvelusta skriptilla esitys/kuviot/hae-viitelaina10v.js.
#
# Ennuste on Taloudellisen katsauksen (syksy 2026, VM 2026:35) taulukosta 5
# Taustaoletukset (s. 28). Luvut ovat vuosikeskiarvoja, joten ne piirretaan
# vaakaviivana kunkin ennustevuoden kohdalle. Paivita luvut kun uusi katsaus
# ilmestyy.
#
# Ulkoasu kuten visu_chart_png():lla: ggcustomin VM-teema, tekstit 1.15-
# kertaisina ja viivan paksuus VM-maarittelyn 1.5.
#
# Ajo: Rscript esitys/kuviot/korot-10v-3kk-ennuste.R [tiedosto.png]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
tiedosto <- if (length(args)) args[[1]] else
  "esitys/kuviot/korot-10v-3kk-ennuste.png"

alku <- as.Date("2019-01-01")

viitelaina <- read.csv("esitys/kuviot/viitelaina10v.csv") |>
  transmute(time = as.Date(time), sarja = "10 v", values)

# Paivahavainnoista kuukausikeskiarvot kuten viitelainalla. Kesken oleva
# kuukausi jaa pois, koska viitelainan sarja paattyy viimeiseen taysiin kuuhun.
euribor <- read.csv("site/data/euribor.csv") |>
  filter(sarja == "euribor3kk") |>
  mutate(time = as.Date(format(as.Date(time), "%Y-%m-01"))) |>
  filter(time <= max(viitelaina$time)) |>
  group_by(time) |>
  summarise(values = mean(values), .groups = "drop") |>
  mutate(sarja = "3 kk")

sarjat <- c("10 v", "3 kk")
d <- bind_rows(viitelaina, euribor) |>
  filter(time >= alku) |>
  mutate(sarja = factor(sarja, levels = sarjat))

# Taloudellinen katsaus, syksy 2026, taulukko 5 (s. 28), ennustevuodet.
ennuste <- data.frame(
  vuosi = rep(2026:2028, times = 2),
  sarja = factor(rep(sarjat, each = 3), levels = sarjat),
  values = c(3.2, 3.0, 3.0,
             2.2, 2.6, 2.5)
) |>
  # Pieni rako vuosien valiin, jotta perakkaiset vuodet erottuvat.
  mutate(alku = as.Date(paste0(vuosi, "-01-12")),
         loppu = as.Date(paste0(vuosi, "-12-20")))

p <- visu_plot(d, colour = "sarja",
               title = "Korot ja VM:n ennuste, %",
               subtitle = "Kuukausikeskiarvo. Vaakaviivat: VM:n ennuste vuosikeskiarvolle, syksy 2026",
               caption = "Lähde: Suomen Pankki ja valtiovarainministeriö",
               lang = "fi")

# Samat mitat ja tyyli kuin visu_chart_png():n oletuksilla.
leveys <- 6.28
korkeus <- 4.49
p <- visu_restyle(p, legend = "auto", scale = 1.15, width = leveys,
                  linewidth = 1.5, height = korkeus) +
  # Ennuste lisataan restylen jalkeen, jotta se pysyy toteumaa ohuempana.
  # Vari tulee samasta asteikosta kuin sarjalla eika selitteeseen tule
  # omaa rivia.
  ggplot2::geom_segment(
    data = ennuste,
    ggplot2::aes(x = alku, xend = loppu, y = values, yend = values,
                 colour = sarja),
    linewidth = 1, lineend = "butt", inherit.aes = FALSE, show.legend = FALSE
  )
ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
cat("Tallennettu:", tiedosto, "\n")
