#==============================================================================|
#                          ---- Variable parsers ----
#==============================================================================|

#' @keywords internal
parse_string <- function(res, var) {
  res[[var]] %||% NA_character_
}

#' @keywords internal
parse_bool <- function(res, var) {
  res[[var]] %||% NA
}

#' @keywords internal
parse_datetime <- function(res, var) {
  as.POSIXct(res[[var]] %||% NA)
}

#' @keywords internal
parse_date <- function(res, var) {
  as.Date(res[[var]] %||% NA)
}

