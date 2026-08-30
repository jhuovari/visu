#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

# NULL-yhdistäjä: palauttaa y:n jos x on NULL.
`%||%` <- function(x, y) if (is.null(x)) y else x
