# Esitykset visun kuvioista

Sivuston kuviot PowerPoint-esitykseen VM:n pohjaan. Kuvio renderöidään
uudelleen dian paikkamerkin mittoihin, joten se täyttää paikkansa eikä sitä
venytetä jälkikäteen.

```
python3 esitys/kokoa.py esitys/suhdanne.yml
```

## Tiedostot

| Tiedosto | Mitä |
|---|---|
| `pohja.pptx` | VM:n esityspohja. Asettelut ja mitat luetaan tästä. |
| `kokoa.py` | Kokoaa esityksen määrittelytiedostosta. |
| `suhdanne.yml` | Esimerkkimäärittely. |
| `kuvat/` | Renderöidyt kuviot. Ei versionhallinnassa. |

## Asennus

Pilvi-istunnossa (Claude Code webissä) riippuvuudet asentuvat itsestään:
`.claude/hooks/session-start.sh` hoitaa R:n, visun ja Python-kirjastot
istunnon alkaessa. Mitään ei tarvitse tehdä.

Omalla koneella asennus on kerran käsin:

```
R CMD INSTALL .                                   # visu riippuvuuksineen
pip install -r esitys/requirements.txt            # python-pptx, PyYAML
```

Esityksen tarkistamiseen kuvina tarvitaan lisäksi `libreoffice-impress` ja
`poppler-utils`.

## Määrittely

```yaml
kieli: fi                    # oletuskieli
pohja: esitys/pohja.pptx     # valinnainen, oletus tämä
liita: polku/olemassa.pptx   # valinnainen: diat lisätään tämän perään

diat:
  - kansi: "Talouden tilannekuva"
    alaotsikko: "Etunimi Sukunimi\nOsasto\n2.10.2026"

  - kuvio: tyottomyysaste-taso
    otsikko: "Työttömyys on kääntynyt"
    teksti:
      - "Luetelmakohta."
    alkaen: 2019-01-01
    asti: 2026-06-30
    kieli: en
    otsikko_kuviossa: false
    muistiinpanot: "Puhujan muistiinpanot."

  - valiotsikko: "Julkinen talous"
  - lopetus: "Kiitos"
    alaotsikko: "jhuovari.github.io/visu"
```

Kuvion tunnisteet löytyvät luettelosta `site/_visu_charts.json`, jossa on myös
kunkin kuvion otsikko kolmella kielellä.

Ilman `teksti`-kenttää kuvio saa dialla molempien palstojen leveyden.

## Mitat

Pohjan asettelu "Kaksi palstaa tai sisältö ja graafi" antaa kuviolle
6,28 × 4,49 tuumaa; ilman tekstipalstaa 11,89 × 4,49. Kuvio renderöidään
näihin mittoihin 300 dpi:llä.

## Liittäminen olemassa olevaan esitykseen

`liita`-avain ottaa pohjaksi annetun tiedoston, säilyttää sen diat ja lisää
uudet perään. Asettelut tulevat silloin siitä tiedostosta, joten sen pitää
olla VM:n pohjaan perustuva.

## Kuviot ilman valmista kuviota

Jos sivustolla ei ole sopivaa kuviota, piirrä se `visu_plot()`:lla samoilla
konventioilla ja harkitse sen lisäämistä sivustolle — silloin se päivittyy
itsestään ja on ensi kerralla valmiina.
