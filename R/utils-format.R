#' Helpers de formatage (pour l'UI)
#' @keywords internal
#' @noRd


# Pourcent FR (XX %)
#' @keywords internal
french_percent <- function(x) {
  scales::label_number(scale = 100, suffix = " %")(x)
}


# Opérateur coalesce léger
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


# Parse une chaîne "+10", "-0,25 %" → numérique
#' @keywords internal
#' @noRd
.parse_signed_numeric <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NA_real_)
  x <- gsub("%", "", x)
  x <- gsub("\\s+", "", x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}


# Format préfixé du signe, vectorisé
#' @keywords internal
#' @noRd
.format_with_sign <- function(num, digits = 2) {
  num <- as.numeric(num)
  out <- character(length(num))
  nas <- is.na(num)
  zeros <- !nas & abs(num) < .Machine$double.eps
  keep <- !nas & !zeros
  out[nas] <- ""
  out[zeros] <- "0"
  if (any(keep)) {
    fmt <- paste0("%+.", digits, "f")
    out[keep] <- sprintf(fmt, num[keep])
  }
  out
}


# Format numérique FR (espaces milliers)
#' @keywords internal
fmt_num <- function(x, digits = NULL) {
  if (!is.null(digits)) x <- round(x, digits)
  x <- unname(x)
  base::format(x, big.mark = " ", trim = TRUE, scientific = FALSE)
}
