#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

# NULL-yhdistäjä: palauttaa y:n jos x on NULL.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Huomautus seka varoituksena etta suoraan stderriin: knitr nielaisee
# varoitukset (warning: false), joten ilman stderria katkos jaisi ajon
# lokissa nakymatta juuri silloin kun se pitaa huomata.
visu_note <- function(...) {
  viesti <- paste0(...)
  cat(viesti, "\n", sep = "", file = stderr())
  warning(viesti, call. = FALSE)
  invisible(viesti)
}
