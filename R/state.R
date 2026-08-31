#' Sivuston lähdehakemisto
#'
#' @param site_dir Hakemisto, tai `NULL` jolloin käytetään asetusta
#'   `visu.site_dir` ja sen puuttuessa työhakemiston alla olevaa `site`-kansiota.
#' @return Hakemistopolku merkkijonona.
#' @export
visu_site_dir <- function(site_dir = NULL) {
  dir <- site_dir %||% getOption("visu.site_dir", "site")
  if (!dir.exists(dir)) {
    stop("Sivuston hakemistoa '", dir, "' ei l\u00f6ydy. Aja repositorion juuresta ",
         "tai aseta options(visu.site_dir = ...).", call. = FALSE)
  }
  dir
}

# Kuvioiden .qmd-tiedostot ovat aina site_dir/kuviot.
visu_charts_dir <- function(site_dir = NULL) {
  file.path(visu_site_dir(site_dir), "kuviot")
}

visu_state_path <- function(site_dir = NULL) {
  file.path(visu_site_dir(site_dir), "_visu_state.json")
}

#' Lue kuvioiden tilatiedosto
#'
#' @param site_dir Sivuston hakemisto, ks. [visu_site_dir()].
#' @return Nimetty lista, jonka avaimina ovat kuvioiden tunnisteet. Tyhjä lista
#'   jos tiedostoa ei vielä ole.
#' @export
visu_state_read <- function(site_dir = NULL) {
  path <- visu_state_path(site_dir)
  if (!file.exists(path)) return(list())
  state <- jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  if (!is.list(state)) list() else state
}

#' Kirjoita kuvioiden tilatiedosto
#'
#' Avaimet järjestetään aakkosjärjestykseen, jotta git-diff näyttää vain
#' oikeasti muuttuneet kuviot.
#'
#' @param state Nimetty lista, ks. [visu_state_read()].
#' @param site_dir Sivuston hakemisto.
#' @return Tiedostopolku näkymättömänä.
#' @export
visu_state_write <- function(state, site_dir = NULL) {
  path <- visu_state_path(site_dir)
  state <- state[order(names(state))]
  jsonlite::write_json(state, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

#' Kuvioiden rekisteri
#'
#' Lukee jokaisen `site/kuviot/*.qmd`-tiedoston etulehden ja muodostaa niistä
#' rekisterin. Etulehden `visu.table_url` on tuoreustarkistuksen syöte:
#'
#' ```
#' ---
#' title: "Palkansaajat"
#' visu:
#'   table_url: "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/11pk.px/"
#' ---
#' ```
#'
#' Kuvio voi lukea useaa taulua. Silloin `table_url` on lista, ja kuvio on
#' vanhentunut heti kun mikä tahansa sen tauluista on päivittynyt:
#'
#' ```
#' visu:
#'   table_url:
#'     - "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/khi/15b5.px/"
#'     - "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/khi/15b7.px/"
#' ```
#'
#' @param site_dir Sivuston hakemisto.
#' @return Data frame sarakkeilla `id`, `title`, `path`, `code_hash` ja
#'   listasarake `table_url`, jonka alkiona on kuvion taulujen osoitteet.
#' @export
visu_chart_registry <- function(site_dir = NULL) {
  dir <- visu_charts_dir(site_dir)
  files <- sort(list.files(dir, pattern = "\\.qmd$", full.names = TRUE))

  charts <- lapply(files, function(path) {
    id <- sub("\\.qmd$", "", basename(path))
    fm <- visu_front_matter(path)
    list(
      id = id,
      title = enc2utf8(as.character(fm$title %||% id)),
      table_url = visu_table_urls(fm$visu$table_url, id),
      path = path,
      code_hash = visu_file_hash(path)
    )
  })

  field <- function(name) vapply(charts, function(x) x[[name]], character(1))

  registry <- data.frame(
    id = field("id"),
    title = field("title"),
    path = field("path"),
    code_hash = field("code_hash"),
    stringsAsFactors = FALSE
  )
  # Yhdella kuviolla voi olla useita tauluja, joten osoitteet eivat mahdu
  # tavalliseen sarakkeeseen. Lue alkio aina muodossa table_url[[i]].
  registry$table_url <- lapply(charts, function(x) x$table_url)
  registry
}

# Etulehden table_url yhtena tai useampana osoitteena. Jarjestys sailyy, jotta
# etulehden ja koodilohkon voi lukea rinnakkain.
visu_table_urls <- function(x, id) {
  urls <- unlist(x, use.names = FALSE)
  if (length(urls) == 0L || !is.character(urls) || anyNA(urls) || !all(nzchar(urls))) {
    stop("Kuviosta '", id, "' puuttuu etulehden kentt\u00e4 visu.table_url, tai ",
         "se ei ole osoite eik\u00e4 lista osoitteita.", call. = FALSE)
  }
  unique(urls)
}

#' Tarkista kuvioiden eheys
#'
#' Etulehden `table_url` ohjaa tuoreustarkistusta ja koodilohkon URL hakee
#' varsinaisen datan. Jos ne eroavat, sivu voi jäädä päivittymättä
#' huomaamatta. Tarkistus kulkee molempiin suuntiin: jokaisen etulehdessä
#' luetellun taulun pitää esiintyä koodissa, ja jokaisen koodissa haetun taulun
#' pitää olla lueteltu etulehdessä. Päättävä kenoviiva ei erota tauluja.
#'
#' @param site_dir Sivuston hakemisto.
#' @return Merkkijonovektori ongelmista, korkeintaan yksi kuviota kohti; tyhjä
#'   vektori kun kaikki on kunnossa.
#' @export
visu_check_charts <- function(site_dir = NULL) {
  registry <- visu_chart_registry(site_dir)
  problems <- character()

  for (i in seq_len(nrow(registry))) {
    body <- visu_body(registry$path[i])
    declared <- registry$table_url[[i]]
    used <- visu_body_table_urls(body)

    missing <- declared[!visu_url_key(declared) %in% visu_url_key(used)]
    extra <- used[!visu_url_key(used) %in% visu_url_key(declared)]

    details <- character()
    if (length(missing) > 0L) {
      details <- c(details, paste0(
        "etulehden taulua ei haeta koodissa: ", paste(missing, collapse = ", ")
      ))
    }
    if (length(extra) > 0L) {
      details <- c(details, paste0(
        "koodi hakee taulun, jota ei ole etulehdess\u00e4: ",
        paste(extra, collapse = ", ")
      ))
    }
    if (length(details) > 0L) {
      problems <- c(problems, paste0(
        "Kuvion '", registry$id[i], "' etulehti ja koodilohko osoittavat eri ",
        "tauluihin, joten tuoreustarkistus ja datahaku voivat erkaantua: ",
        paste(details, collapse = "; "), "."
      ))
    }
  }

  problems
}

# Koodilohkoissa esiintyvat PxWeb-taulujen osoitteet. Taulun tunnistaa
# .px-paatteesta, joten tilaston selauslinkit eivat osu tarkistukseen.
visu_body_table_urls <- function(body) {
  hits <- regmatches(body, gregexpr("https?://[^\"'[:space:])]+\\.px/?", body))[[1]]
  unique(hits)
}

# Vertailukelpoinen muoto: paattava kenoviiva ei vaihda taulua.
visu_url_key <- function(urls) {
  sub("/+$", "", as.character(urls))
}

# Etulehden rajat: ensimmäinen '---'-rivi ja sitä seuraava '---' tai '...'.
visu_front_matter_range <- function(lines) {
  start <- which(grepl("^---\\s*$", lines))
  if (length(start) == 0L || start[1] != 1L) return(NULL)
  end <- which(grepl("^(---|\\.\\.\\.)\\s*$", lines))
  end <- end[end > 1L]
  if (length(end) == 0L) return(NULL)
  c(2L, end[1] - 1L)
}

visu_front_matter <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  range <- visu_front_matter_range(lines)
  if (is.null(range)) return(list())
  if (range[2] < range[1]) return(list())
  # Etulehti on aina UTF-8, mutta ajoympariston locale ei valttamatta ole.
  yaml::yaml.load(enc2utf8(paste(lines[range[1]:range[2]], collapse = "\n"))) %||% list()
}

# Tiedoston sisältö ilman etulehteä.
visu_body <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  range <- visu_front_matter_range(lines)
  if (!is.null(range)) lines <- lines[-seq_len(range[2] + 1L)]
  paste(lines, collapse = "\n")
}

# Rivinvaihdoista riippumaton tiedoston tiiviste.
visu_file_hash <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  digest::digest(paste(lines, collapse = "\n"), algo = "sha256", serialize = FALSE)
}
