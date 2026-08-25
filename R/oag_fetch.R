#' Get the list of entities available in the OpenAIRE Graph API
#'
#' The entities available are:
#' \itemize{
#'   \item Research products (publications, datasets, software, and other
#'   research products)
#'   \item Organizations (universities, research organizations, funders, and
#'   companies)
#'   \item Data sources (repositories, journals, aggregators, and funder
#'   databases whose content is available in the OpenAIRE Graph)
#'   \item Projects (funded research project grants)
#'   \item Persons (authors and contributors involved in research products)
#' }
#' The list of entities exposed in the API is available at
#' \url{https://graph.openaire.eu/docs/apis/graph-api/overview}.
#'
#' @returns A character vector with the OpenAIRE graph entities.
#' @export
#'
#' @examples
#' oag_entities()

oag_entities <- function() {
  c("research-products", "organizations", "datasources", "projects", "persons")
}
