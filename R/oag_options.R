#' Collect the options to use in a query to the OpenAIRE Graph API
#'
#' This function collects the options requirements to use when building a query.
#' The validity of the options are collected without any checks. The checks will
#' take place later, after being passed to `oag_query()`.
#'
#' @param sortBy A named character vector with the field(s) on which to sort the
#' results. Names must be valid sorting fields, only accepting the values "ASC"
#' (ascending) and "DESC" (descending) as input. Note that the fields on which
#' results can be sorted vary across entities. To get the list of sorting fields
#' for a given entity use `get_sorting_fields()`.
#' @param includeStats A scalar logical indicating whether statistics about the
#' query should be included (`TRUE`) or not (`FALSE`) in the results.
#' @param pageSize A positive integer indicating the number of records per page
#' a query should return. The OpenAIRE Graph API has a limit of 100 records per
#' page.
#' @param page A positive integer indicating the page the query should return.
#' Using this argument trigger an offset-based paging query. Note that the
#' `page` (offset-based paging) and `cursor` (cursor-based paging) arguments
#' cannot be used simultaneously.
#' @param cursor A logical indicating whether to use cursor-based paging when
#' querying the OpenAIRE Graph API (`TRUE`) or not (`FALSE`). Note that the
#' `cursor` (cursor-based paging) and `page` (offset-based paging) arguments
#' cannot be used simultaneously.
#'
#' @returns A list of class `oag_options` with the options requirements to pass
#' to `oa_query()`. Note that at this stage, the validity of the options has not
#' been checked yet.
#' @export
#'
#' @examples
#' # Example of query options with offset-based paging with sorting
#' oag_options(
#'   sortBy = c(prevalence = "ASC"),
#'   includeStats = TRUE,
#'   pageSize = 50,
#'   page = 1
#' )
#'
#' # Example of query options with cursor-based paging and sorting
#' oag_options(
#'   sortBy = c(prevalence = "DESC"),
#'   includeStats = FALSE,
#'   pageSize = 100,
#'   cursor = TRUE
#' )

oag_options <- function(
    sortBy = NULL,
    includeStats = NULL,
    pageSize = NULL,
    page = NULL,
    cursor = NULL
) {
  options <- list(
    sortBy = sortBy,
    includeStats = includeStats,
    pageSize = pageSize,
    page = page,
    cursor = cursor
  )

  structure(options, class = c("oag_options", "character"))
}

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

fmt_opt_sorting <- function(sortBy, entity, call) {
  if (!is.null(sortBy)) {
    if (!rlang::is_bare_character(sortBy) || !rlang::is_vector(sortBy)) {
      sorting_error_msg(
        paste0(
          "In the list of options, the object passed to {.arg sortBy} must be ",
          "a character vector or NULL to use OpenAIRE Graph API's default."
        ),
        call
      )
    }
    if (!rlang::is_named(sortBy)) {
      sorting_error_msg(
        paste0(
          "In the list of options, the character vector passed to ",
          "{.arg sortBy} must be named."
        ),
        call
      )
    }
    if (!all(sortBy %in% c("ASC", "DESC"))) {
      sorting_error_msg(
        paste0(
          "In the list of options, character vector passed to {.arg sortBy} ",
          "can only contains \"ASC\" (ascending) or \"DESC\" (descending) ",
          "as input."
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
          "fields can be passed to the list of options to sort the results: ",
          "{.var {sorting_fields}}."
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

#' Check and format the "includeStats" option passed to `oag_set_options()`
#'
#' @inheritParams oag_query
#' @param includeStats A scalar logical indicating whether statistics about the
#' query should included (`TRUE`) or not (`FALSE`).
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to control the inclusion of query statistics. If
#' `includeStats` is NULL at the input, the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_stats <- function(includeStats, call) {
  if (!is.null(includeStats)) {
    if (!rlang::is_scalar_logical(includeStats)) {
      cli::cli_abort(
        paste0(
          "In the list of options, {.var includeStats} must be a logical ",
          "scalar or NULL to use OpenAIRE Graph API's default."
        ),
        call = call
      )
    } else if (includeStats) {
      includeStats <- "includeStats=true"
    } else {
      includeStats <- "includeStats=false"
    }
  } else {
    includeStats <- NULL
  }

  return(includeStats)
}


#' Check and format the "pageSize" option passed to `oag_set_options()`
#'
#' @inheritParams oag_query
#' @param pageSize A positive integer indicating the page size of a query. Page
#' size corresponds to the number of records returned.
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to specify the page size of a query. If `pageSize` is
#' NULL at the input, the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_page_size <- function(pageSize, call) {
  if (!is.null(pageSize)) {
    if (!rlang::is_scalar_integerish(pageSize) || pageSize < 1) {
      cli::cli_abort(
        paste0(
          "In the list of options, {.var pageSize} must be a scalar positive ",
          "integer or NULL to use OpenAIRE Graph API's default."
        ),
        call = call
      )
    } else if (pageSize > 100) {
      cli::cli_abort("{.var pageSize} cannot exceed 100.", call = call)
    } else {
      pageSize <- paste0("pageSize=", pageSize)
    }
  } else {
    pageSize <- NULL
  }

  return(pageSize)
}


#' Check and format the "page" option passed to `oag_set_options()`
#'
#' @inheritParams oag_query
#' @param page A positive integer indicating the page to return from a query.
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to specify the page to return from a query. If `page` is
#' NULL at the input, the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_page <- function(page, call) {
  if (!is.null(page)) {
    if (!rlang::is_scalar_integerish(page) || page < 1) {
      cli::cli_abort(
        paste0(
          "In the list of options, {.var page} must be a scalar positive ",
          "integer or NULL to use OpenAIRE Graph API's default."
        ),
        call = call
      )
    } else {
      # Avoid R coercing large number to scientific notation when pasting them
      page <- paste0("page=", format(page, scientific = FALSE))
    }
  } else {
    page <- NULL
  }

  return(page)
}


#' Check and format the "cursor" option passed to `oag_set_options()`
#'
#' @inheritParams oag_query
#' @param cursor A scalar logical indicating whether to use cursor-based paging
#' when querying the OpenAIRE Graph API (`TRUE`) or not (`FALSE`).
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to specify that cursor-based paging should be used. If
#' `cursor` is NULL at the input, the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_cursor <- function(cursor, call) {
  if (!is.null(cursor)) {
    if (!rlang::is_scalar_logical(cursor)) {
      cli::cli_abort(
        paste0(
          "In the list of options, {.var cursor} must be a logical scalar or ",
          "NULL or NULL to use OpenAIRE Graph API's default."
        ),
        call = call
      )
    } else if (cursor) {
      cursor <- "cursor=*"
    } else {
      cursor <- NULL
    }
  }

  return(cursor)
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
        "{.arg sortBy} argument in {.fn oag_options}."
      ),
      i = paste0(
        "The vector must be named using the format ",
        "{.code fieldname = \"sortDirection\"}, where {.arg sortDirection} ",
        "is \"ASC\" (ascending) or \"DESC\" (descending)."
      ),
      i = paste0(
        "Here is valid example: ",
        "{.code oag_options(sortBy=c(relevance=\"ASC\"))}"
      )
    ),
    call = call
  )
}
