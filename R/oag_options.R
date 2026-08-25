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
