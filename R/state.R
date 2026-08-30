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
#' @param site_dir Sivuston hakemisto.
#' @return Data frame sarakkeilla `id`, `title`, `table_url`, `path`, `code_hash`.
#' @export
visu_chart_registry <- function(site_dir = NULL) {
  dir <- visu_charts_dir(site_dir)
  files <- sort(list.files(dir, pattern = "\\.qmd$", full.names = TRUE))

  rows <- lapply(files, function(path) {
    id <- sub("\\.qmd$", "", basename(path))
    fm <- visu_front_matter(path)
    url <- fm$visu$table_url
    if (is.null(url) || !is.character(url) || length(url) != 1L) {
      stop("Kuviosta '", id, "' puuttuu etulehden kentt\u00e4 visu.table_url.", call. = FALSE)
    }
    data.frame(
      id = id,
      title = enc2utf8(as.character(fm$title %||% id)),
      table_url = url,
      path = path,
      code_hash = visu_file_hash(path),
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0L) {
    return(data.frame(id = character(), title = character(), table_url = character(),
                      path = character(), code_hash = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' Tarkista kuvioiden eheys
#'
#' Etulehden `table_url` ohjaa tuoreustarkistusta ja koodilohkon URL hakee
#' varsinaisen datan. Jos ne eroavat, sivu voi jäädä päivittymättä
#' huomaamatta. Tämä tarkistus estää sellaisen ajautumisen.
#'
#' @param site_dir Sivuston hakemisto.
#' @return Merkkijonovektori ongelmista; tyhjä vektori kun kaikki on kunnossa.
#' @export
visu_check_charts <- function(site_dir = NULL) {
  registry <- visu_chart_registry(site_dir)
  problems <- character()

  for (i in seq_len(nrow(registry))) {
    body <- visu_body(registry$path[i])
    if (!grepl(registry$table_url[i], body, fixed = TRUE)) {
      problems <- c(problems, paste0(
        "Kuvion '", registry$id[i], "' etulehden table_url ei esiinny sen ",
        "koodilohkossa. Tuoreustarkistus ja datahaku osoittavat eri tauluun."
      ))
    }
  }

  problems
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
