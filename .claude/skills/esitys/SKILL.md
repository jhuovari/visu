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
   kuin `visu_chart_png()` (katso oletusarvot).

**Tarjoa aina lopuksi kuvion viemistä sivustolle.** Kerta­luonteinen
esityskuvio kannattaa lisätä `site/kuviot/<sivu>.qmd`:hen kolmikielisine
teksteineen, jolloin se on seuraavalla kerralla valmiina luettelossa — ja
päivittyy itsestään.

## Hyvä tietää

- **Kuvion haku kestää.** Ensimmäinen kuvio sivulta vie ~15 s, koska sivun
  kaikkien kuvioiden data haetaan rajapinnasta. Saman sivun seuraavat kuviot
  ovat ilmaisia samassa ajossa, joten `kokoa.py` renderöi kaikki kuviot
  yhdellä R-ajolla.
- **Selite** menee automaattisesti alas kun sarjoja on enintään neljä, muuten
  oikeaan reunaan. Alareunassa monen sarjan selite veisi liikaa korkeutta.
- **Lähde on kuvion sisällä** (ggplotin caption), joten dialle ei tarvita
  erillistä lähderiviä.
- **Pohja** on `esitys/pohja.pptx`. Asettelut luetaan siitä, joten mitat eivät
  ole koodissa kovakoodattuina. Muu pohja: `pohja:`-avain määrittelyssä.
- **Älä muokkaa `site/_visu_charts.json`:ia käsin.** Se syntyy sivuston
  rakentamisen yhteydessä; kerralla sen saa ajamalla `visu_catalog_build()`.
