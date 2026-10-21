# Tyoikaiset, tyovoima ja tyolliset trendisarjoista, esityskoossa ja
# englanniksi. Sama kuvio kuin sivuston tyovoiman-tarjonta
# (site/kuviot/tyomarkkinat.qmd), mutta kausitasoitettujen sarjojen sijaan
# Tilastokeskuksen trendisarjat.
#
# Tyovoima on tyollisten ja tyottomien trendien summa. Trendi ei ole
# tasmalleen additiivinen, mutta ero summan omaan trendiin on pieni, ja nain
# tyovoima ja sen osat ovat keskenaan johdonmukaiset. Tyoikainen vaesto on
# sellaisenaan: se on kannan suure eika kaipaa tasoitusta.
#
# Ajo: Rscript esitys/kuviot/tyovoiman-tarjonta-trendi-en.R [tiedosto.png]

library(visu)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
tiedosto <- if (length(args)) args[[1]] else
  "esitys/kuviot/tyovoiman-tarjonta-trendi-en.png"

tunnusluvut <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/135z.px/",
  query = list(
    "timeperiod_m" = "*",
    "contentscode" = c("tyolliset_trendi", "tyottomat_trendi")
  )
)

vaesto <- visu_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/135y.px/",
  query = list(
    "timeperiod_m" = "*",
    "sukupuoli_9_20180101" = "SSS",
    "ikaryhma_19_20190101" = "15-64",
    "contentscode" = "tyti-Vaesto"
  )
) |>
  transmute(time, sarja = "vaesto", values)

sarja <- function(koodi) {
  tunnusluvut |> filter(contentscode == koodi) |> select(time, values)
}

# Liitos varmistaa, etta tyolliset ja tyottomat ovat samalta kuukaudelta.
trendi <- inner_join(sarja("tyolliset_trendi"), sarja("tyottomat_trendi"),
                     by = "time", suffix = c("_tyolliset", "_tyottomat"))

sarjat <- c(vaesto = "Working-age population 15-64",
            tyovoima = "Labour force 15-74", tyolliset = "Employed 15-74",
            tyottomat = "Unemployed")

# Muutos tammikuusta 2019 kuten sivustolla, jotta eri kokoiset kannat ovat
# samassa kuviossa vertailukelpoisia.
tarjonta <- bind_rows(
  vaesto,
  transmute(trendi, time, sarja = "tyovoima",
            values = values_tyolliset + values_tyottomat),
  transmute(trendi, time, sarja = "tyolliset", values = values_tyolliset),
  transmute(trendi, time, sarja = "tyottomat", values = values_tyottomat)
) |>
  filter(!is.na(values), time >= as.Date("2019-01-01")) |>
  group_by(sarja) |>
  arrange(time, .by_group = TRUE) |>
  mutate(values = values - first(values)) |>
  ungroup() |>
  mutate(erittely = factor(sarja, levels = names(sarjat), labels = sarjat))

p <- visu_plot(tarjonta, colour = "erittely",
               title = "Change from January 2019, 1,000 persons",
               subtitle = "Trend",
               caption = "Source: Statistics Finland, Labour Force Survey",
               lang = "en", zeroline = TRUE) +
  # Pitkat nimet veisivat oikeassa reunassa puolet piirtoalasta; alareunaan
  # ne mahtuvat kahteen sarakkeeseen.
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2))

# Samat mitat ja tyyli kuin visu_chart_png():n oletuksilla.
leveys <- 6.28
korkeus <- 4.49
p <- visu_restyle(p, legend = "bottom", scale = 1.15, width = leveys,
                  linewidth = 1.5, height = korkeus, top_first = TRUE)
ggplot2::ggsave(tiedosto, p, width = leveys, height = korkeus, dpi = 300, bg = "white")
cat("Tallennettu:", tiedosto, "\n")
