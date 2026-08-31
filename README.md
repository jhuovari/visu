# visu

Rakentaa StatFin-datasta interaktiivisia kuvioita ja julkaisee ne
Quarto-sivustona osoitteessa <https://jhuovari.github.io/visu/>.

Sivusto päivittyy joka aamu klo 8:01 Suomen aikaa, mutta vain siltä osin kuin
on tarpeen: ajo tarkistaa PxWeb-rajapinnasta kunkin kuvion lähdetaulun
päivitysajan ja renderöi uudelleen ainoastaan ne kuviot, joiden data on
muuttunut. Kun mikään taulu ei ole päivittynyt, ajo ei renderöi mitään eikä
tee committia.

## Uuden kuvion lisääminen

Lisää yksi tiedosto `site/kuviot/<tunnus>.qmd`. Tiedostonimi on kuvion tunnus,
etulehden `visu.table_url` ohjaa tuoreustarkistusta ja koodilohko hakee datan
ja piirtää kuvion:

````
---
title: "Palkansaajat"
visu:
  table_url: "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/11pk.px/"
---

```{r}
library(visu)

dat <- pxwebtools::pxw_get_data(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/11pk.px/",
  query = list(
    "timeperiod_y" = "*",
    "sukupuoli_9_20180101" = "SSS",
    "contentscode" = "Palkansaajat_yht"
  )
)

dat |>
  visu_plot(title = "Palkansaajat, 1 000 henkeä") |>
  visu_interactive()
```
````

Jokaisella kuviolla on oma datahakunsa, ja koodilohko näkyy sivulla
kopiointinapin kanssa. Kopioidun pätkän saa ajettua sellaisenaan omassa
R-istunnossa.

Sarakkeiden nimet tulevat suoraan `pxwebtools::pxw_get_data()`:n paluuarvosta.
`visu_plot()` käyttää oletuksena ensimmäistä saraketta x-akselina ja saraketta
`values` y-akselina. Useamman sarjan kuviossa anna luokittelusarake
argumentilla `colour = "<sarakkeen nimi>"`; jos nimi on väärin, virheilmoitus
luettelee datan sarakkeet.

Etulehden ja koodilohkon URL:n pitää olla sama. `visu_check_charts()` tarkistaa
tämän, ja `visu_update_site()` keskeytyy jos ne ovat päässeet eroamaan — muuten
kuvio voisi jäädä päivittymättä huomaamatta.

## Sivuston päivittäminen käsin

```r
# Mitä rakennettaisiin uudelleen, rakentamatta mitään
visu::visu_update_site(dry_run = TRUE)

# Rakenna muuttuneet kuviot
visu::visu_update_site()

# Pakota yksi kuvio
visu::visu_update_site(force = "palkansaajat")

# Koko sivusto: tarvitaan kun _quarto.yml tai sivupohja muuttuu, koska
# yksittäisen sivun renderöinti ei päivitä muiden sivujen navigaatiota
visu::visu_update_site(full = TRUE)
```

Ajot tehdään repositorion juuresta. Ne tarvitsevat
[Quarton](https://quarto.org/docs/get-started/) polusta.

## Miten päivitys pysyy kevyenä

1. `site/kuviot/*.qmd` -tiedostojen etulehdistä muodostetaan rekisteri.
2. Kunkin taulun päivitysaika luetaan PxWeb-kansiolistauksesta. Yksi pyyntö
   kattaa kaikki saman kansion taulut, eikä varsinaista dataa haeta lainkaan.
3. Aikaleimaa verrataan tiedostoon `site/_visu_state.json`. Kuvio on
   vanhentunut jos sen data, koodi tai lähdetaulu on muuttunut.
4. Vanhentuneilta kuvioilta puretaan Quarton freeze-välimuisti ja ne
   renderöidään yksitellen. Muiden sivujen HTML ei avaudu lainkaan.
5. Etusivu renderöidään, tila kirjoitetaan ja poistuneiden kuvioiden sivut
   siivotaan.

Jos yhden kuvion renderöinti kaatuu, muut rakennetaan silti loppuun ja
julkaistaan. Kaatunut kuvio jää ilman tilamerkintää, joten se yritetään
uudelleen seuraavalla ajolla, ja ajo päättyy virheeseen — Actionsissa näkyy
punainen ajo, mutta sivusto pysyy muilta osin ajan tasalla.

Jos aikaleimaa ei saada rajapinnasta, kuvio rakennetaan varmuuden vuoksi
uudelleen ja ajo varoittaa. Silloin sivusto pysyy oikeana, mutta päivitys ei
ole enää kevyt — tarkista PxWeb-kansiolistauksen `updated`-kenttä.

## Julkaisu

GitHub Pages tarjoillaan päähaaran `docs/`-hakemistosta (Settings → Pages →
Deploy from a branch → `main` / `docs`). Sivusto rakentuu paikalleen, joten
git-diffistä näkee täsmälleen mitkä kuviot muuttuivat.

Ajastus on tiedostossa `.github/workflows/update-site.yml`. GitHub Actionsin
cron on UTC:ssä, joten työ ajastetaan sekä kello 5:01 että 6:01 UTC ja
vahtityö päästää läpi vain sen ajon, joka osuu Suomen aikaan kello 8.

## Asennus

```r
# install.packages("pak")
pak::pak("jhuovari/visu")
```
