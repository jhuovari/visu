#' Päätä mitkä kuviot pitää rakentaa uudelleen
#'
#' Puhdas funktio: ei verkkoa eikä levyä, joten koko päivityslogiikka on
#' testattavissa ilman StatFin-rajapintaa.
#'
#' @param registry [visu_chart_registry()]:n tulos.
#' @param state [visu_state_read()]:n tulos.
#' @param updated Nimetty merkkijonovektori, kuvion tunnus -> lähdetaulun
#'   aikaleima, tai `NA_character_` jos aikaleimaa ei saatu.
#' @param force `TRUE` (kaikki), tunnisteiden vektori, tai `NULL`.
#' @return Data frame sarakkeilla `id`, `stale`, `reason`.
#' @export
visu_stale_charts <- function(registry, state, updated, force = NULL) {
  ids <- registry$id
  forced <- if (isTRUE(force)) ids else as.character(force %||% character())

  reason <- vapply(seq_along(ids), function(i) {
    id <- ids[i]
    prev <- state[[id]]
    upd <- if (id %in% names(updated)) as.character(updated[[id]]) else NA_character_

    if (id %in% forced) {
      "pakotettu"
    } else if (is.null(prev) || is.null(prev$built_at)) {
      "uusi kuvio"
    } else if (!identical(as.character(prev$code_hash %||% NA_character_), registry$code_hash[i])) {
      "koodi muuttunut"
    } else if (!identical(as.character(prev$table_url %||% NA_character_), registry$table_url[i])) {
      "l\u00e4hdetaulu vaihtunut"
    } else if (is.na(upd)) {
      "aikaleima tuntematon"
    } else if (!identical(as.character(prev$source_updated %||% NA_character_), upd)) {
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

#' Päivitä sivusto inkrementaalisesti
#'
#' Tarkistaa jokaisen kuvion lähdetaulun päivitysajan ja renderöi uudelleen
#' vain ne kuviot, joiden data on muuttunut. Kun mikään ei ole muuttunut, ajo
#' ei tee yhtään Quarto-renderöintiä eikä kirjoita yhtään tiedostoa, jolloin
#' GitHub Actions -ajo päättyy ilman committia.
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
    empty <- visu_stale_charts(registry, list(), character())
    empty$status <- character()
    return(invisible(empty))
  }

  state <- visu_state_read(site_dir)
  updated <- stats::setNames(
    vapply(registry$table_url, visu_table_updated, character(1), USE.NAMES = FALSE),
    registry$id
  )
  if (all(is.na(updated))) {
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
  # nimenomaan niilta kuvioilta, joiden data halutaan hakea uudelleen.
  unlink(file.path(site_dir, "_freeze", "kuviot", stale), recursive = TRUE)

  # Yksi hajonnut kuvio ei saa pysayttaa koko paivittaista ajoa, joten virheet
  # kerataan talteen ja silmukkaa jatketaan.
  failed <- character()
  if (!full) {
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
  }

  # Epaonnistuneet kuviot eivat saa tilamerkintaa, jotta ne yritetaan
  # uudelleen seuraavalla ajolla. Tila kirjoitetaan ennen etusivua, koska
  # etusivu lukee paivitysajat siita.
  visu_state_write(visu_new_state(registry, updated, state, decisions, failed), site_dir)

  if (full) {
    visu_quarto_render(quarto, site_dir, quiet)
  } else {
    visu_quarto_render(quarto, file.path(site_dir, "index.qmd"), quiet)
  }

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
    list(
      table_url = registry$table_url[i],
      title = registry$title[i],
      source_updated = if (is.na(updated[[id]])) NULL else unname(updated[[id]]),
      code_hash = registry$code_hash[i],
      built_at = built_at
    )
  })

  names(new) <- registry$id
  new[!vapply(new, is.null, logical(1))]
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
