#' Get the list of available fields for sorting in the entities
#'
#' @inheritParams oag_query
#'
#' @returns A character vector with the fields on which query results for the
#' given entity can be sorted on.
#' @export
#'
#' @examples
#' get_sorting_fields("projects")

get_sorting_fields <- function(entity) {
  rlang::arg_match(entity, oag_entities())
  # Fields on which the "research-products" entity can be sorted by
  rp <- c(
    "relevance",
    "publicationDate",
    "dateOfCollection",
    "influence",
    "popularity",
    "citationCount",
    "impulse"
  )
  # Fields on which the "organizations" entity can be sorted by
  org <- "relevance"
  # Fields on which the "datasources" entity can be sorted by
  ds <- "relevance"
  # Fields on which the "projects" entity can be sorted by
  proj <- c("relevance", "startDate", "endDate")
  # Fields on which the "persons" entity can be sorted by
  pers <- "relevance"

  fields <- switch(
    entity,
    "research-products" = rp,
    organizations = org,
    datasources = ds,
    projects = proj,
    persons = pers
  )

  return(fields)
}

#' Compose an error message for sorting option issues with pre-formatted help
#'
#' @param msg A string with the message to add to the pre-formatted error
#' message.
#' @param call The caller environment passed to `cli::cli_abort()`.
#'
#' @returns Nothin, use for side-effect only.
#' @keywords internal

sorting_error_msg <- function(msg, call) {
  cli::cli_abort(
    c(
      msg,
      i = paste0(
        "Sorting can be controlled by passing a named character vector to the ",
        "{.arg sortBy} argument in {.fn oag_set_options}."
      ),
      i = paste0(
        "The vector must be named using the format ",
        "{.code fieldname = \"sortDirection\"}, where {.arg sortDirection} ",
        "is \"ASC\" (ascending) or \"DESC\" (descending)."
      ),
      i = paste0(
        "Here is valid example: ",
        "{.code oag_set_options(\"datasources\", sortBy=c(relevance=\"ASC\"))}"
      )
    ),
    call = call
  )
}
