#!/bin/bash
# Asentaa visun riippuvuudet pilvi-istuntoon.
#
# Istunnon kontti on tyhja: R, paketit ja Python-kirjastot pitaa asentaa
# ennen kuin kuvioita voi piirtaa tai esityksia koota. Kontin tila
# tallentuu valimuistiin taman jalkeen, joten hinta maksetaan kerran.
#
# Omalla koneella tata ei ajeta: siella R-ymparisto on kayttajan oma eika
# apt-asennuksia tehda hanen puolestaan.
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  echo "Paikallinen istunto, ohitetaan riippuvuuksien asennus."
  exit 0
fi

JUURI="${CLAUDE_PROJECT_DIR:-$(cd "$(dirname "$0")/../.." && pwd)}"
cd "$JUURI"

# Kuvioiden otsikot ovat suomeksi. Ilman UTF-8-localea R kirjoittaa aakkoset
# koodattuina, kuten CI-ajossakin erikseen asetetaan.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  {
    echo 'export LANG=C.UTF-8'
    echo 'export LC_ALL=C.UTF-8'
  } >> "$CLAUDE_ENV_FILE"
fi
export LANG=C.UTF-8 LC_ALL=C.UTF-8

puuttuu() { ! command -v "$1" > /dev/null 2>&1; }

# --- Jarjestelmapaketit ---------------------------------------------------
# soffice ja pdftoppm ovat esityksen tarkistusta varten: ilman niita diaa ei
# voi katsoa kuvana, jolloin leikkautunut teksti jaisi huomaamatta.
TARVE=()
puuttuu R         && TARVE+=(r-base-core r-base-dev)
puuttuu soffice   && TARVE+=(libreoffice-impress)
puuttuu pdftoppm  && TARVE+=(poppler-utils)

if [ ${#TARVE[@]} -gt 0 ]; then
  echo "Asennetaan: ${TARVE[*]}"
  export DEBIAN_FRONTEND=noninteractive
  apt-get update -qq
  # R-pakettien kaannos tarvitsee naita, vaikka suurin osa tulee binaarina.
  apt-get install -y -qq --no-install-recommends \
    "${TARVE[@]}" gfortran libxml2-dev libuv1-dev libcurl4-openssl-dev \
    libssl-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
fi

# --- R-paketit ------------------------------------------------------------
# Posit Package Manager tarjoaa Ubuntulle valmiiksi kaannetyt binaarit.
# Lahdekoodista kaannettyna sama joukko - ggplot2, plotly, seasonal ja
# x13binary riippuvuuksineen - veisi moninkertaisen ajan.
export R_REPOS="https://packagemanager.posit.co/cran/__linux__/noble/latest"

asenna_github() {
  # ggcustom ja pxwebtools tulevat GitHubista. Niita ei haeta
  # remotes::install_github():lla, koska se kysyy GitHubin rajapinnalta - ja
  # rajapinta on istunnon valityspalvelimen takana usein suljettu (403),
  # vaikka git clone samaan repoon toimii. Kloonaus on siis luotettavampi.
  local nimi="$1"
  if Rscript -e "quit(status = !requireNamespace('$nimi', quietly = TRUE))" 2>/dev/null; then
    return 0
  fi
  local hakemisto
  hakemisto="$(mktemp -d)"
  echo "Kloonataan $nimi..."
  git clone --depth 1 -q "https://github.com/jhuovari/$nimi.git" "$hakemisto/$nimi"
  Rscript -e "
    options(repos = c(CRAN = Sys.getenv('R_REPOS')), Ncpus = max(1L, parallel::detectCores()))
    remotes::install_deps('$hakemisto/$nimi', dependencies = TRUE, upgrade = 'never')
  "
  R CMD INSTALL --no-docs "$hakemisto/$nimi"
  rm -rf "$hakemisto"
}

# Pelkan visun tarkistus ei riita: visu kayttaa riippuvuuksiaan ggcustom::
# -tyylisilla kutsuilla, joten sen nimiavaruus latautuu vaikka ggcustom
# puuttuisi. Vika nakyisi vasta kuviota piirrettaessa.
TARPEET='c("visu", "ggcustom", "pxwebtools", "dplyr")'
if ! Rscript -e "quit(status = !all(vapply($TARPEET, requireNamespace, logical(1), quietly = TRUE)))" 2>/dev/null; then
  echo "Asennetaan R-riippuvuudet..."
  Rscript -e '
    options(repos = c(CRAN = Sys.getenv("R_REPOS")), Ncpus = max(1L, parallel::detectCores()))
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  '
  # Jarjestys on tassa olennainen: kun nama kaksi ovat jo paikallaan,
  # visun oma riippuvuusasennus ei yrita hakea niita GitHubista.
  asenna_github ggcustom
  asenna_github pxwebtools
  # Loput riippuvuudet luetaan DESCRIPTIONista ja asennetaan CRANista.
  # remotes::install_deps() ei kay tahan: se selvittaa Remotes-kentan
  # paketit GitHubin rajapinnasta myos silloin kun ne ovat jo asennettuina,
  # ja rajapinta vastaa 403.
  Rscript -e '
    options(repos = c(CRAN = Sys.getenv("R_REPOS")), Ncpus = max(1L, parallel::detectCores()))
    d <- read.dcf("DESCRIPTION")
    kentat <- intersect(c("Imports", "Suggests"), colnames(d))
    nimet <- unlist(strsplit(paste(d[1, kentat], collapse = ","), ","))
    nimet <- trimws(sub("\\(.*", "", nimet))
    nimet <- nimet[nzchar(nimet)]
    # Omat GitHub-paketit on jo asennettu, ja base-paketit tulevat R:n mukana.
    base <- rownames(installed.packages(priority = "base"))
    nimet <- setdiff(nimet, c("ggcustom", "pxwebtools", base))
    # dplyr on kuviosivujen eika paketin riippuvuus, joten se on vain
    # Suggests-kentassa; roxygen2 tarvitaan dokumentaation ajamiseen.
    nimet <- unique(c(nimet, "dplyr", "roxygen2"))
    puuttuvat <- nimet[!vapply(nimet, requireNamespace, logical(1), quietly = TRUE)]
    if (length(puuttuvat) > 0) {
      message("Asennetaan: ", paste(puuttuvat, collapse = ", "))
      install.packages(puuttuvat)
    }
  '
fi

# Paketti asennetaan aina, jotta se vastaa tyohakemiston koodia.
echo "Asennetaan visu..."
if ! R CMD INSTALL --no-docs . > /tmp/visu-install.log 2>&1; then
  echo "visun asennus epaonnistui:" >&2
  tail -20 /tmp/visu-install.log >&2
  exit 1
fi

# --- Python ---------------------------------------------------------------
# Esityksen kokoaminen: python-pptx ja PyYAML.
echo "Asennetaan Python-riippuvuudet..."
pip install --quiet --break-system-packages --root-user-action=ignore \
  -r esitys/requirements.txt

echo "Valmis. R, visu ja esitystyokalu ovat kaytettavissa."
