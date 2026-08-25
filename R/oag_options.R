#' Check and format sorting option passed to `oag_set_options()`
#'
#' @inheritParams oag_query
#' @param sortBy A named character vector with the fields on which to sort the
#' results from the queried entity. Names must be valid sorting fields and
#' values can only be "ASC" (ascending) or "DESC" (descending).
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to control sorting. If `sortBy` is NULL at the input,
#' the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_sorting <- function(sortBy, entity) {
  call <- rlang::caller_env()

  if (!is.null(sortBy)) {
    if (!rlang::is_bare_character(sortBy) || !rlang::is_vector(sortBy)) {
      sorting_error_msg(
        "The object passed to {.arg sortBy} must be a character vector.",
        call
      )
    }
    if (!rlang::is_named(sortBy)) {
      sorting_error_msg(
        "The character vector passed to {.arg sortBy} must be named.",
        call
      )
    }
    if (!all(sortBy %in% c("ASC", "DESC"))) {
      sorting_error_msg(
        paste0(
          "The character vector passed to {.arg sortBy} can only contains ",
          "\"ASC\" (ascending) or \"DESC\" (descending) as values."
        ),
        call
      )
    }

    # Value that can be used to sort
    sorting_fields <- get_sorting_fields(entity)

    if (!all(names(sortBy) %in% sorting_fields)) {
      cli::cli_abort(
        paste0(
          "When working with the \"{entity}\" entity, only the following ",
          "fields can be used to sort the results: {.var {sorting_fields}}."
        )
      )
    }

    # Glue together sorting fields and direction (e.g. "relevance ASC")
    sorting_formatted <- paste0(names(sortBy), " ", sortBy)
    # Combine the sorting options together using a coma separator (if only one
    # sorting option, nothing will be collpase and will stay as is).
    sorting_formatted <- paste0(sorting_formatted, collapse = ", ")

    return(sorting_formatted)
  } else {
    return(NULL)
  }
}

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
