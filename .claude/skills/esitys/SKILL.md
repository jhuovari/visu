---
name: esitys
description: Kokoa PowerPoint-esitys visu-sivuston kuvioista VM:n esityspohjaan. Käytä kun pyydetään esitystä, diasarjaa, kalvoja tai kuvioita esitykseen, kun viitataan sivuston kuvioon ("inflaatiosivun vuosimuutoskuvio", "työttömyysaste"), tai kun kuvioita halutaan liittää olemassa olevaan esitykseen. Also use for building a slide deck from the visu site charts.
---

# Esitys visun kuvioista

Sivuston 58 kuviota ovat valmiina käytettäväksi esityksessä. Kuvio renderöidään
uudelleen dian paikkamerkin mittoihin, joten se täyttää paikkansa eikä sitä
venytetä.

## Työnkulku

1. **Etsi kuviot.** Lue `site/_visu_charts.json`. Siinä on jokaisen kuvion
   tunniste, sivu, otsikko kolmella kielellä ja lähde. Täsmää käyttäjän
   pyyntö otsikkoon tai tunnisteeseen. Kysy vain jos osumia on monta eikä
   valinta ratkea asiayhteydestä.
2. **Kirjoita määrittely** `esitys/<nimi>.yml`. Malli: `esitys/suhdanne.yml`.
3. **Kokoa:** `python3 esitys/kokoa.py esitys/<nimi>.yml`
4. **Katso tulos.** Muunna kuviksi ja tarkista jokainen dia:
   ```
   soffice --headless --norestore --convert-to pdf esitys/<nimi>.pptx
   pdftoppm -jpeg -r 110 <nimi>.pdf slide
   ```
   Etsi erityisesti: leikkautunut teksti, päällekkäiset elementit, tyhjäksi
   jäänyt paikkamerkki.
5. Kerro käyttäjälle tiedostopolku ja mitä dioilla on.

## Määrittelyn muoto

```yaml
kieli: fi                    # oletuskieli kaikille kuvioille
liita: polku/olemassa.pptx   # valinnainen: lisää diat tämän perään

diat:
  - kansi: "Esityksen otsikko"
    alaotsikko: "Nimi\nOsasto\n2.10.2026"

  - kuvio: tyottomyysaste-taso   # tunniste luettelosta
    otsikko: "Työttömyys on kääntynyt"
    teksti:                      # jos puuttuu, kuvio saa koko leveyden
      - "Ensimmäinen luetelmakohta."
      - "Toinen."
    alkaen: 2019-01-01           # valinnainen aikarajaus
    asti: 2026-06-30
    kieli: en                    # valinnainen, ohittaa oletuksen
    otsikko_kuviossa: false      # poistaa kuvion oman otsikon
    muistiinpanot: "Puhujan muistiinpanot."

  - valiotsikko: "Julkinen talous"
  - lopetus: "Kiitos"
```

## Kun valmista kuviota ei ole

Jos luettelosta ei löydy sopivaa kuviota, piirrä se samalla tavalla kuin
sivuston kuviot:

1. Etsi taulu StatFinin PxWeb-rajapinnasta.
2. Hae data `visu_get_data()`:lla, kausitasoita tarvittaessa
   `visu_seasonal()`:lla, piirrä `visu_plot()`:lla.
3. Otsikko kertoo mitä mitataan ja yksikön ("Työttömyysaste, 15-74-vuotiaat, %"),
   caption alkaa sanalla "Lähde:".
4. Tallenna skripti kansioon `esitys/kuviot/` ja renderöi PNG samoilla mitoilla
   ja tyylillä kuin `visu_chart_png()` (katso oletusarvot). Malli:
   `esitys/kuviot/ostovoima-erat-reaalinen-trendi.R`.

**Tarjoa aina lopuksi kuvion viemistä sivustolle.** Kerta­luonteinen
esityskuvio kannattaa lisätä `site/kuviot/<sivu>.qmd`:hen kolmikielisine
teksteineen, jolloin se on seuraavalla kerralla valmiina luettelossa — ja
päivittyy itsestään.

## Hyvä tietää

- **Kuvion haku kestää.** Ensimmäinen kuvio sivulta vie ~15 s, koska sivun
  kaikkien kuvioiden data haetaan rajapinnasta. Saman sivun seuraavat kuviot
  ovat ilmaisia samassa ajossa, joten `kokoa.py` renderöi kaikki kuviot
  yhdellä R-ajolla.
- **Selite** menee automaattisesti alas, jos sen rivi mahtuu kuvion
  leveyteen, muuten oikeaan reunaan. Pitkät nimet vievät oikeassa reunassa
  helposti puolet piirtoalasta; silloin `legend = "bottom"` ja
  `guides(colour = guide_legend(nrow = 2))` ladovat ne alas kahdelle
  riville. Alareunan selite on esityksessä tiiviimpi kuin sivustolla: väli
  x-akseliin on kolmanneksen pienempi ja sarjojen välit kapeammat.
- **Ulkoasu on ggcustomin VM-teema** (`theme_vm()`), kuten sivustolla, mutta
  tekstit ovat 1.15-kertaisia ja viivat VM-määrittelyn paksuisia (1.5).
  Sivustolla viiva on ohuempi (0.8), eikä sitä muuteta. `visu_chart_png()`
  tekee tämän itse; omassa kuviossa käytä samoja asetuksia:
  `visu_restyle(p, legend = "auto", scale = 1.15, linewidth = 1.5, height = 4.49, top_first = TRUE)`.
- **Selitteen ensimmäinen sarja on päällimmäisenä.** Viivakuviossa sarjat
  piirretään käänteisessä järjestyksessä, joten tärkein sarja (yleensä Suomi,
  selitteessä ensimmäisenä) ei jää muiden alle. Värit ja selitteen järjestys
  eivät muutu. Pinottuihin pylväisiin ja alueisiin tämä ei vaikuta.
  `visu_chart_png()` tekee tämän itse; sivustolla järjestys on toisin päin.
- **Pitkät tekstit sovitetaan** kuvion mittoihin, kun `visu_restyle()` saa
  korkeuden (`visu_chart_png()` antaa sen aina): liian pitkä otsikko jaetaan
  riveille, ja lähde, joka leikkautuisi oikean reunan selitteen takia,
  tasataan kuvion oikeaan reunaan. Sivuston kuvioihin tämä ei vaikuta.
  Älä muuta teeman muita asetuksia (otsikon paikka, fonttikoot) ilman syytä.
- **Lähde on kuvion sisällä** (ggplotin caption), joten dialle ei tarvita
  erillistä lähderiviä.
- **Pohja** on `esitys/pohja.pptx`. Asettelut luetaan siitä, joten mitat eivät
  ole koodissa kovakoodattuina. Muu pohja: `pohja:`-avain määrittelyssä.
- **Älä muokkaa `site/_visu_charts.json`:ia käsin.** Se syntyy sivuston
  rakentamisen yhteydessä; kerralla sen saa ajamalla `visu_catalog_build()`.
