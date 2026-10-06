#' Päätä mitkä kuviot pitää rakentaa uudelleen
#'
#' Puhdas funktio: ei verkkoa eikä levyä, joten koko päivityslogiikka on
#' testattavissa ilman StatFin-rajapintaa.
#'
#' @param registry [visu_chart_registry()]:n tulos.
#' @param state [visu_state_read()]:n tulos.
#' @param updated Kuvion tunnuksella nimetty lista tai vektori, jonka alkiona
#'   ovat kuvion taulujen aikaleimat. Usean taulun kuviossa alkio on taulun
#'   osoitteella nimetty vektori, yhden taulun kuviossa riittää pelkkä
#'   aikaleima. `NA_character_` tarkoittaa, ettei aikaleimaa saatu.
#' @param force `TRUE` (kaikki), tunnisteiden vektori, tai `NULL`.
#' @return Data frame sarakkeilla `id`, `stale`, `reason`.
#' @export
visu_stale_charts <- function(registry, state, updated, force = NULL) {
  ids <- registry$id
  forced <- if (isTRUE(force)) ids else as.character(force %||% character())

  reason <- vapply(seq_along(ids), function(i) {
    id <- ids[i]
    prev <- state[[id]]
    urls <- visu_url_key(registry$table_url[[i]])
    prev_urls <- visu_url_key(unlist(prev$table_url) %||% character())
    stamps <- visu_source_stamps(if (id %in% names(updated)) updated[[id]] else NULL, urls)
    prev_stamps <- visu_source_stamps(prev$source_updated, prev_urls)

    if (id %in% forced) {
      "pakotettu"
    } else if (is.null(prev) || is.null(prev$built_at)) {
      "uusi kuvio"
    } else if (!visu_code_unchanged(id, registry, state)) {
      "koodi muuttunut"
    } else if (!identical(sort(prev_urls), sort(urls))) {
      "l\u00e4hdetaulu vaihtunut"
    } else if (length(stamps) != length(urls) || anyNA(stamps)) {
      "aikaleima tuntematon"
    } else if (!identical(stamps, prev_stamps)) {
      "data p\u00e4ivittynyt"
    } else {
      "ajan tasalla"
    }
  }, character(1))

  data.frame(
    id = ids,
    stale = reason != "ajan tasalla",
    reason = reason,
    stringsAsFactors = FALSE
  )
}

# Onko sivun lahdekoodi sama kuin viimeisimmassa onnistuneessa rakennuksessa.
# Sama vertailu ratkaisee kaksi asiaa: kuvion vanhentumisen ja sen, kelpaako
# talteen otettu freeze-valimuisti Quartolle.
visu_code_unchanged <- function(id, registry, state) {
  prev <- state[[id]]
  identical(as.character(prev$code_hash %||% NA_character_),
            registry$code_hash[match(id, registry$id)])
}

# Kuvion taulujen aikaleimat vertailukelpoisessa muodossa: osoitteella nimetty
# ja nimen mukaan jarjestetty vektori, tai NULL kun leimoja ei ole. Vanha
# tilatiedosto kirjasi yhden taulun leiman ilman osoitetta, joten nimeton leima
# nimetaan kuvion osoitteilla.
visu_source_stamps <- function(stamps, urls) {
  stamps <- unlist(stamps, use.names = TRUE)
  if (length(stamps) == 0L) return(NULL)
  keys <- names(stamps)
  if (is.null(keys) || !all(nzchar(keys))) {
    if (length(stamps) != length(urls)) return(NULL)
    keys <- urls
  }
  stamps <- stats::setNames(as.character(stamps), visu_url_key(keys))
  stamps[order(names(stamps))]
}

#' Päivitä sivusto inkrementaalisesti
#'
#' Tarkistaa jokaisen kuvion lähdetaulun päivitysajan ja renderöi uudelleen
#' vain ne kuviot, joiden data on muuttunut. Kun mikään ei ole muuttunut, ajo
#' ei tee yhtään Quarto-renderöintiä eikä kirjoita yhtään tiedostoa, jolloin
#' GitHub Actions -ajo päättyy ilman committia.
#'
#' Jokainen kuvio rakennetaan omana renderöintinään, jotta yhden lähteen katko
#' ei vie muita mukanaan. Myös `full = TRUE` tekee näin ensin: koko sivuston
#' renderöinti on yksi Quarto-kutsu, joka kaatuisi kokonaan yhteen virheeseen.
#' Epäonnistunut kuvio palautetaan edelliseen versioonsa freeze-välimuistista,
#' jolloin sivustorenderöinti ei aja sitä uudelleen. Jos kuviosta ei ole
#' edellistä versiota tai sen qmd on muuttunut, sivustorenderöinti jätetään
#' väliin ja vain etusivu päivitetään — rakennetut sivut jäävät voimaan.
#' Ajo päättyy silti virheeseen, jotta hajonnut kuvio huomataan.
#'
#' @param site_dir Sivuston hakemisto, ks. [visu_site_dir()].
#' @param force `TRUE` pakottaa kaikki kuviot, tunnisteiden vektori vain osan.
#' @param dry_run Jos `TRUE`, tulostaa päätöstaulukon renderöimättä mitään.
#' @param full Jos `TRUE`, renderöi koko sivuston. Tarvitaan kun `_quarto.yml`
#'   tai sivupohja muuttuu, koska yksittäisen sivun renderöinti ei päivitä
#'   muiden sivujen navigaatiota.
#' @param quiet Vaimentaa Quarton tulosteen.
#' @return Data frame `id` / `stale` / `reason` / `status` näkymättömänä.
#'   `status` on `"rakennettu"`, `"ajan tasalla"` tai `"virhe"`.
#' @export
visu_update_site <- function(site_dir = NULL,
                             force = NULL,
                             dry_run = FALSE,
                             full = FALSE,
                             quiet = FALSE) {
  site_dir <- visu_site_dir(site_dir)
  visu_clear_cache()

  problems <- visu_check_charts(site_dir)
  if (length(problems) > 0L) {
    stop("Kuvioiden eheystarkistus ep\u00e4onnistui:\n- ",
         paste(problems, collapse = "\n- "), call. = FALSE)
  }

  registry <- visu_chart_registry(site_dir)
  if (nrow(registry) == 0L) {
    message("Hakemistossa ", visu_charts_dir(site_dir), " ei ole kuvioita.")
    empty <- visu_stale_charts(registry, list(), list())
    empty$status <- character()
    return(invisible(empty))
  }

  state <- visu_state_read(site_dir)
  # Yksi kuvio voi lukea useaa taulua, joten aikaleimat kerataan taulukohtaisesti.
  # Saman kansion taulut maksavat silti vain yhden pyynnon, ks. visu_table_updated().
  updated <- stats::setNames(
    lapply(registry$table_url, function(urls) {
      stats::setNames(vapply(urls, visu_table_updated, character(1), USE.NAMES = FALSE), urls)
    }),
    registry$id
  )
  if (all(is.na(unlist(updated)))) {
    warning("Yhdenk\u00e4\u00e4n taulun p\u00e4ivitysaikaa ei saatu selville, joten kaikki ",
            "kuviot rakennetaan uudelleen. Tarkista PxWeb-rajapinnan ",
            "kansiolistaus ja sen updated-kentt\u00e4.", call. = FALSE)
  }

  decisions <- visu_stale_charts(registry, state, updated, force)
  decisions$status <- ifelse(decisions$stale, "rakennetaan", "ajan tasalla")
  visu_report(decisions)

  if (dry_run) return(invisible(decisions))

  stale <- decisions$id[decisions$stale]
  if (length(stale) == 0L && !full) {
    message("Ei muutoksia, sivustoa ei rakennettu uudelleen.")
    return(invisible(decisions))
  }

  quarto <- visu_quarto_bin()

  # Freeze-valimuisti pitaisi kuvion vanhassa datassa, joten se puretaan
  # nimenomaan niilta kuvioilta, joiden data halutaan hakea uudelleen. Vanha
  # valimuisti otetaan talteen, jotta epaonnistunut kuvio voidaan palauttaa
  # edelliseen versioonsa.
  stash <- visu_freeze_stash(site_dir, stale)
  on.exit(unlink(stash, recursive = TRUE), add = TRUE)
  unlink(file.path(visu_freeze_dir(site_dir), stale), recursive = TRUE)

  # Yksi hajonnut kuvio ei saa pysayttaa koko paivittaista ajoa, joten virheet
  # kerataan talteen ja silmukkaa jatketaan. Nain on myos taydessa ajossa:
  # koko sivuston renderointi on yksi Quarto-kutsu, joka kaatuisi kokonaan
  # yhden lahteen katkoon, joten lohkot ajetaan ensin sivu kerrallaan.
  failed <- character()
  for (id in stale) {
    ok <- tryCatch({
      visu_quarto_render(quarto, registry$path[match(id, registry$id)], quiet)
      TRUE
    }, error = function(e) {
      message("Kuvion '", id, "' render\u00f6inti ep\u00e4onnistui: ", conditionMessage(e))
      FALSE
    })
    if (!ok) failed <- c(failed, id)
  }

  # Epaonnistuneen kuvion valimuisti palautetaan, jotta koko sivuston
  # renderointi kayttaa kuvion edellista versiota eika aja rikkinaista lohkoa
  # uudelleen. Palautus ei auta, jos kuviota ei ole kertaakaan rakennettu tai
  # jos sen qmd on muuttunut: Quarto ajaa muuttuneen sivun joka tapauksessa.
  estavat <- union(
    visu_freeze_restore(site_dir, stash, failed),
    failed[!vapply(failed, visu_code_unchanged, logical(1), registry, state)]
  )

  # Epaonnistuneet kuviot eivat saa tilamerkintaa, jotta ne yritetaan
  # uudelleen seuraavalla ajolla. Tila kirjoitetaan ennen etusivua, koska
  # etusivu lukee paivitysajat siita.
  # Kuvioluettelo taydentyy renderoinnin aikana kuvio kerrallaan; tassa siita
  # poistetaan kuviot, joita ei enaa ole.
  visu_catalog_prune(site_dir)

  visu_state_write(visu_new_state(registry, updated, state, decisions, failed), site_dir)

  # Taysi renderointi kayttaa freeze-valimuistia, joten se ei aja lohkoja
  # uudelleen. Se jaa kuitenkin valiin, jos jokin kuvio kaatuisi siina taas:
  # silloin jo rakennetut sivut jaavat voimaan ja vain navigaatio jaa
  # paivittamatta.
  taysi <- full && length(estavat) == 0L
  if (full && !taysi) {
    message("Koko sivustoa ei render\u00f6ity, koska kuvio ",
            paste(estavat, collapse = ", "),
            " ajettaisiin uudelleen ja kaatuisi samaan virheeseen.")
  }
  visu_quarto_render(quarto, if (taysi) site_dir else file.path(site_dir, "index.qmd"), quiet)

  visu_prune_output(registry, site_dir)

  decisions$status <- ifelse(
    decisions$id %in% failed, "virhe",
    ifelse(decisions$stale, "rakennettu", "ajan tasalla")
  )

  if (length(failed) > 0L) {
    stop(length(failed), "/", length(stale), " kuvion render\u00f6inti ep\u00e4onnistui: ",
         paste(failed, collapse = ", "),
         ". Muut kuviot rakennettiin ja ovat committoitavissa.", call. = FALSE)
  }

  invisible(decisions)
}

# Uusi tila: rakennetuille kuvioille tuore leima, muille entinen säilytetään.
# Rekisteristä poistuneet kuviot putoavat tilasta pois.
visu_new_state <- function(registry, updated, state, decisions, failed = character()) {
  built_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  new <- lapply(seq_len(nrow(registry)), function(i) {
    id <- registry$id[i]
    unchanged <- !decisions$stale[match(id, decisions$id)]
    # Epaonnistunut kuvio pitaa entisen tilansa, ja epaonnistunut uusi kuvio
    # jaa kokonaan ilman merkintaa -- kummassakin tapauksessa se on seuraavalla
    # ajolla taas vanhentunut.
    if ((unchanged || id %in% failed) && !is.null(state[[id]])) {
      return(state[[id]])
    }
    if (id %in% failed) return(NULL)
    # Tuntemattomat leimat jaavat pois, jotta ne haetaan uudelleen seuraavalla
    # ajolla. Leimat kirjataan taulun osoitteella, jotta usean taulun kuviossa
    # tiedetaan mika taulu paivittyi.
    stamps <- visu_source_stamps(updated[[id]], visu_url_key(registry$table_url[[i]]))
    stamps <- stamps[!is.na(stamps)]
    list(
      table_url = registry$table_url[[i]],
      title = registry$title[i],
      source_updated = if (length(stamps) == 0L) NULL else as.list(stamps),
      code_hash = registry$code_hash[i],
      built_at = built_at
    )
  })

  names(new) <- registry$id
  new[!vapply(new, is.null, logical(1))]
}

visu_freeze_dir <- function(site_dir) file.path(site_dir, "_freeze", "kuviot")

# Kopioi kuvioiden freeze-valimuistin tilapaishakemistoon ja palauttaa sen
# polun, jotta epaonnistunut kuvio voidaan palauttaa entiselleen.
visu_freeze_stash <- function(site_dir, ids) {
  stash <- tempfile("visu-freeze-")
  dir.create(stash, recursive = TRUE)
  for (id in ids) {
    from <- file.path(visu_freeze_dir(site_dir), id)
    if (dir.exists(from)) file.copy(from, stash, recursive = TRUE)
  }
  stash
}

# Palauttaa talteen otetun valimuistin ja kertoo ne tunnukset, joilta sita ei
# ollut.
visu_freeze_restore <- function(site_dir, stash, ids) {
  if (length(ids) == 0L) return(character())
  dir.create(visu_freeze_dir(site_dir), showWarnings = FALSE, recursive = TRUE)
  puuttuu <- character()
  for (id in ids) {
    from <- file.path(stash, id)
    if (!dir.exists(from)) {
      puuttuu <- c(puuttuu, id)
      next
    }
    unlink(file.path(visu_freeze_dir(site_dir), id), recursive = TRUE)
    file.copy(from, visu_freeze_dir(site_dir), recursive = TRUE)
  }
  puuttuu
}

# Poistaa poistuneiden kuvioiden jaljet: sivun, sen resurssihakemiston ja
# freeze-valimuistin. Muuten docs/ kerryttaisi kuolleita sivuja.
visu_prune_output <- function(registry, site_dir) {
  out_charts <- file.path(visu_output_dir(site_dir), "kuviot")
  if (!dir.exists(out_charts)) return(invisible(NULL))

  pages <- list.files(out_charts, pattern = "\\.html$")
  orphans <- setdiff(sub("\\.html$", "", pages), registry$id)
  if (length(orphans) == 0L) return(invisible(NULL))

  message("Poistetaan poistuneet kuviot: ", paste(orphans, collapse = ", "))
  unlink(c(
    file.path(out_charts, paste0(orphans, ".html")),
    file.path(out_charts, paste0(orphans, "_files")),
    file.path(site_dir, "_freeze", "kuviot", orphans)
  ), recursive = TRUE)

  invisible(NULL)
}

# Quarton output-dir luetaan _quarto.yml:stä, jotta polku on yhdessä paikassa.
visu_output_dir <- function(site_dir) {
  config <- file.path(site_dir, "_quarto.yml")
  out <- if (file.exists(config)) yaml::yaml.load_file(config)$project$`output-dir` else NULL
  normalizePath(file.path(site_dir, out %||% "_site"), mustWork = FALSE)
}

visu_quarto_bin <- function() {
  bin <- Sys.which("quarto")
  if (!nzchar(bin)) {
    stop("Quartoa ei l\u00f6ydy polusta. Asenna Quarto: https://quarto.org/docs/get-started/",
         call. = FALSE)
  }
  unname(bin)
}

visu_quarto_render <- function(quarto, target, quiet) {
  args <- c("render", target)
  if (quiet) args <- c(args, "--quiet")
  status <- system2(quarto, args)
  if (!identical(status, 0L)) {
    stop("Quarto-render\u00f6inti ep\u00e4onnistui kohteelle '", target, "' (status ", status, ").",
         call. = FALSE)
  }
  invisible(TRUE)
}

visu_report <- function(decisions) {
  if (nrow(decisions) == 0L) return(invisible(NULL))
  message(paste0("  ", format(decisions$id), "  ", decisions$reason, collapse = "\n"))
  invisible(NULL)
}
