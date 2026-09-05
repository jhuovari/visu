# Ajon sisäinen välimuisti kansiolistauksille, jotta N kuviota samasta
# tietokannan kansiosta maksaa yhden HTTP-pyynnön eikä N:ää.
the <- new.env(parent = emptyenv())
the$folder_listing <- list()
the$table_meta <- list()

#' Tyhjennä kansiolistausten välimuisti
#'
#' @return `NULL`, kutsutaan sivuvaikutuksen vuoksi.
#' @export
visu_clear_cache <- function() {
  the$folder_listing <- list()
  the$table_meta <- list()
  invisible(NULL)
}

#' Milloin StatFin-taulu on viimeksi päivitetty
#'
#' Lukee taulun yläkansion listauksen PxWeb-rajapinnasta ja poimii siitä
#' taulun `updated`-aikaleiman. Tämä on koko sivuston päivityslogiikan halpa
#' ensimmäinen taso: aikaleiman haku ei lataa varsinaista dataa lainkaan, ja
#' yksi kansiopyyntö kattaa kaikki saman kansion taulut.
#'
#' @param url Taulun API-osoite, esim.
#'   `"https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/tyti/11pk.px/"`.
#' @return Aikaleima merkkijonona, tai `NA_character_` jos sitä ei saada
#'   selville. `NA` ei ole virhe vaan kehotus tarkistaa tuoreus datan
#'   tiivisteestä (`visu_data_hash()`).
#' @export
visu_table_updated <- function(url) {
  parts <- visu_split_table_url(url)
  if (is.null(parts)) return(NA_character_)

  listing <- visu_folder_listing(parts$folder)
  if (is.null(listing) || !all(c("id", "updated") %in% names(listing))) {
    return(NA_character_)
  }

  hit <- listing$updated[listing$id == parts$table]
  if (length(hit) != 1L || is.na(hit) || !nzchar(hit)) NA_character_ else as.character(hit)
}

# Pilkkoo taulun URL:n kansio-osoitteeksi ja taulun tunnisteeksi.
visu_split_table_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) return(NULL)
  clean <- sub("[?#].*$", "", sub("/+$", "", url))
  parts <- strsplit(clean, "/", fixed = TRUE)[[1]]
  if (length(parts) < 2L) return(NULL)
  list(
    folder = paste(utils::head(parts, -1L), collapse = "/"),
    table = utils::tail(parts, 1L)
  )
}

# Hakee kansiolistauksen kerran ajoa kohti. Kaikki virheet (verkko, JSON,
# odottamaton rakenne) päätyvät NULLiin, jolloin kutsuja siirtyy tiivisteeseen.
visu_folder_listing <- function(folder) {
  cached <- the$folder_listing[[folder]]
  if (!is.null(cached)) {
    return(if (identical(cached, NA)) NULL else cached)
  }

  listing <- tryCatch(
    jsonlite::fromJSON(folder, simplifyDataFrame = TRUE),
    error = function(e) {
      warning("Kansiolistausta ei saatu osoitteesta ", folder, ": ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (!is.data.frame(listing)) listing <- NULL

  the$folder_listing[[folder]] <- listing %||% NA
  listing
}
