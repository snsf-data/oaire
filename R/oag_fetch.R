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


#' Get the OpenAIRE Graph API url for a given entity
#'
#' @param entity A string with the entity to query. It must match an entity
#' returned by `oag_entities()`.
#'
#' @returns A string with the URL to the API for the given entity.
#' @export
#'
#' @examples
#' get_oag_api_url("datasources")

get_oag_api_url <- function(entity) {
  # Only entities returned by `oag_entities()` are accepted
  entity <- rlang::arg_match(entity, oag_entities())
  # Base OpenAIRE Graph API URL
  api_url <- "https://api.openaire.eu/graph/v3"

  # Building and returning the entity API URL
  file.path(api_url, entity)
}

#' Compose a query to the OpenAIRE Graph API
#'
#' The OpenAIRE Graph API works with filters and options (see
#' \url{https://graph.openaire.eu/docs/apis/graph-api/}). The function
#' interprets additional (named) argument(s) passed to the `...` argument as
#' filter(s). The filter(s) must be named with the name of the field to filter,
#' as described in the OpenAIRE Graph API. Options can be specified with
#' `opa_options()`.
#'
#' @inheritParams get_oag_api_url
#' @param ... Filter(s) to use when composing the query. The filter(s) must be
#' named and their value must be a string. The name of the filter must be a
#' valid filter referenced in the OpenAIRE Graph API documentation. For example,
#' `publicationYear = "2020"` can be used to filter publication from 2020 when
#' working with "research-products" entity.
#' @param options A set of options (created with `opa_options()`) used to
#' compose the query. Options allow to control sorting, paging, etc. See
#' `?opa_options()` for more details.
#'
#' @returns A string with the query URL.
#' @export
#'
#' @examples
#' oag_query(
#'   "research-products",
#'   publicationYear = "2020",
#'   options = oag_options(sortBy = c(relevance = "ASC"))
#' )

oag_query <- function(entity, ..., options = NULL) {
  # Only entities returned by `oag_entities()` are accepted
  entity <- rlang::arg_match(entity, oag_entities())
  # Extract the filters passed as additional arguments
  filters <- rlang::dots_list(...)

  if (!is.null(options)) {
    if (!inherits(options, "oag_options")) {
      # Check the options were set with `oag_set_options()` to make sure they
      # have the right format.
      cli::cli_abort(
        c(
          "Options passed to a query must be of class {.var oag_options}.",
          i = "Use {.fn oag_options} to create a valid `options` object."
        )
      )
    } else {
      options <- oag_set_options(
        entity = entity,
        sortBy = options[["sortBy"]],
        includeStats = options[["includeStats"]],
        pageSize = options[["pageSize"]],
        page = options[["page"]],
        cursor = options[["cursor"]]
      )
    }
  }

  # If the number of filters is greater than zero, they are checked and
  # formatted before adding them to the query.
  if (length(filters) > 0) {
    # Is each filter named?
    is_filter_named <- nzchar(names(filters))
    # Filters passed via `...` must be named since the name of the argument
    # should be the actual name of the filter.
    if (!all(is_filter_named)) {
      cli::cli_abort(
        c(
          "All filters must be named following the scheme `field = value`.",
          x = "{.var {filters[!is_filter_named]}} {?is/are} not named."
        )
      )
    }

    # Are values in filters a string
    is_filter_char <- mapply(rlang::is_bare_string, filters)
    is_filter_oag_str <- mapply(\(x) inherits(x, "oag_str"), filters)

    # Check that values in filters are either a string or of class `oag_str`
    if (!all(is_filter_char | is_filter_oag_str)) {
      cli::cli_abort(
        c(
          "Values assigned to filters must be a string.",
          x = paste0(
            "{.var {names(filters[!is_filter_char & !is_filter_oag_str])}} ",
            "{?is/are} not a string"
          ),
          i = paste0(
            "Multiple values combined with AND/OR/NOT logical operators can ",
            "be constructed with the {.fn concat_*} functions."
          )
        )
      )
    }

    # Create the query filters by adding together the filtered fields and their
    # filtering values (format "field=value").
    query_filters <- paste0(names(filters), "=", unlist(filters))
    # Combine all filters into a single string using the ampersand as separator
    collapsed_filters <- paste0(query_filters, collapse = "&")
  } else {
    # If not filter was passed to the function, `collapsed_filters` must be NULL
    collapsed_filters <- NULL
  }

  # Get the API URL of the entity
  query_entity <- get_oag_api_url(entity)
  # Combine the filters and options
  query_params <- paste0(c(collapsed_filters, options), collapse = "&")
  # In case `query_params` is empty, the final query is `query_entity`. If not,
  # then we concatenate `query_entity` and `query_params`, separated by a
  # question mark.
  if (!nzchar(query_params)) {
    query <- query_entity
  } else {
    query <- paste0(query_entity, "?", query_params)
  }

  query
}
