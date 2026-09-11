#' Tulosta sivun päivitystieto
#'
#' Kirjoittaa sivun alkuun yhden rivin siitä, milloin sivun lähdetiedot ovat
#' viimeksi päivittyneet. Aikaleima on tuorein sivun tauluista, koska se on se
#' hetki, jolloin sivun luvut viimeksi muuttuivat.
#'
#' Taulut luetaan oletuksena renderöitävän tiedoston etulehdestä, joten
#' osoitteita ei tarvitse toistaa. `visu_check_charts()` pitää huolen siitä,
#' että etulehti ja koodilohkot hakevat samat taulut.
#'
#' Tarkoitettu koodilohkoon, jonka asetuksina ovat `#| output: asis` ja
#' `#| echo: false`.
#'
#' @param url Taulujen API-osoitteet. Oletuksena `NULL`, jolloin ne luetaan
#'   renderöitävän tiedoston etulehden `visu.table_url`-kentästä.
#' @param path Luettavan `.qmd`-tiedoston polku. Oletuksena `NULL`, jolloin
#'   käytetään knitrin renderöimää tiedostoa.
#' @return Rivi merkkijonona näkymättömänä; kutsutaan tulosteen vuoksi. Jos
#'   aikaleimaa ei saada, ei tulosteta mitään.
#' @examples
#' \dontrun{
#' visu_updated_note()
#' }
#' @export
visu_updated_note <- function(url = NULL, path = NULL) {
  urls <- url %||% visu_current_table_urls(path)
  if (length(urls) == 0L) return(invisible(""))

  leimat <- vapply(urls, visu_table_updated, character(1), USE.NAMES = FALSE)
  leimat <- leimat[!is.na(leimat) & nzchar(leimat)]
  if (length(leimat) == 0L) return(invisible(""))

  # Tuorein taulu kertoo, milloin sivun luvut viimeksi muuttuivat.
  ulos <- paste0("*Tiedot päivitetty ",
                 visu_format_stamp(max(leimat)), ".*\n\n")
  cat(ulos)
  invisible(ulos)
}

# Renderoitavan tiedoston taulut. Ilman knitria tai tiedoston ulkopuolella
# palautetaan tyhja, jolloin kutsuja jattaa rivin pois eika kaada renderointia.
visu_current_table_urls <- function(path = NULL) {
  if (is.null(path) && requireNamespace("knitr", quietly = TRUE)) {
    path <- knitr::current_input(dir = TRUE)
  }
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(character())

  fm <- visu_front_matter(path)
  urls <- fm$visu$table_url
  if (is.null(urls)) character() else as.character(unlist(urls, use.names = FALSE))
}
