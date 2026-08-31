#' Collect the options to use in a query to the OpenAIRE Graph API
#'
#' This function collects the options requirements to use when building a query.
#' The options are collected without any checks regarding their validity. The
#' checks will take place later, after being passed to `oag_query()`.
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
#' @param page A positive integer indicating the specific page the query should
#' return. When `page` is kept NULL, this triggers an offset-based paging query.
#' Note that the `page` (offset-based paging) and `cursor` (cursor-based paging)
#' arguments cannot be used simultaneously.
#' @param cursor A logical indicating whether to use cursor-based paging when
#' querying the OpenAIRE Graph API (`TRUE`) or not (`FALSE`). Note that the
#' `cursor` (cursor-based paging) and `page` (offset-based paging) arguments
#' cannot be used simultaneously.
#' @param is_cursor_next **Do not use this argument**. It is used internally by
#' `oag_fetch()` to indicate whether the `cursor` argument must be interpreted
#' as the method to use ("cursor-based paging") or as the next cursor.
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
  cursor = NULL,
  is_cursor_next = FALSE
) {
  options <- list(
    sortBy = sortBy,
    includeStats = includeStats,
    pageSize = pageSize,
    page = page,
    cursor = cursor,
    is_cursor_next = is_cursor_next
  )
  if (all(mapply(is.null, options))) {
    invisible()
  } else {
    structure(options, class = c("oag_options", "list"))
  }
}

#' Check and format all options from an object of class `oag_options`
#'
#' @inheritParams oag_options
#' @inheritParams oag_api_url
#' @param call The caller environment passed to `cli::cli_abort()`.
#'
#' @returns A string with options requirements formatted according to the
#' OpenAIRE Graph API's documentation.
#' @keywords internal

oag_set_options <- function(
  entity,
  sortBy = NULL,
  includeStats = NULL,
  pageSize = NULL,
  page = NULL,
  cursor = NULL,
  is_cursor_next = FALSE,
  call = rlang::caller_env()
) {
  # Get the environment of the calling function to allow associating errors
  # with the calling function.
  # Only entities returned by `oag_entities()` are accepted
  entity <- rlang::arg_match(entity, oag_entities())

  # Check and format all the options included in the function
  sortBy_str <- fmt_opt_sorting(sortBy, entity, call)
  includeStats_str <- fmt_opt_stats(includeStats, call)
  pageSize_str <- fmt_opt_page_size(pageSize, call)
  page_str <- fmt_opt_page(page, call)
  cursor_str <- fmt_opt_cursor(cursor, is_cursor_next, call)

  # Check that offset- and cursor-based paging having not been both set
  if (!is.null(page) && (!is.null(cursor) && cursor)) {
    cli::cli_abort(
      c(
        "Offset- and cursor-based paging cannot be used simultaneously.",
        i = paste0(
          "To query with offset-based paging, set {.code page = <n>} in ",
          "{.fn oag_options} (where `<n>` is the page number) and keep the ",
          "the {.arg page} argument as NULL (default)."
        ),
        i = paste0(
          "To query with cursor-based paging, set {.code cursor = TRUE} in ",
          "{.fn oag_options} and keep the {.arg page} argument as NULL ",
          "(default)."
        )
      ),
      call = call
    )
  }

  # Check before sending the query if the number of records requested is not
  # to big when using offset-based paging.
  if (!is.null(page) && !is.null(pageSize) && (page * pageSize > 10000)) {
    cli::cli_abort(
      c(
        paste0(
          "Offset-based paging can be used to retrieve up to 10,000 records ",
          "only."
        ),
        i = "Consider using cursor-based paging or reducing the query size.",
        i = paste0(
          "Your query contains ",
          "{prettyNum(page*pageSize, scientific = FALSE, big.mark = ',')} ",
          "records ({.arg page} * {.arg pageSize})."
        )
      ),
      call = call
    )
  }

  # Combine all the options together in a single string using ampersand as
  # separator.
  option_str <- paste0(
    c(sortBy_str, includeStats_str, pageSize_str, page_str, cursor_str),
    collapse = "&"
  )

  option_str
}

#' Check and format sorting option passed to `oag_set_options()`
#'
#' @inheritParams oag_options
#' @inheritParams sorting_error_msg
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to control sorting. If `sortBy` is NULL at the input,
#' the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_sorting <- function(sortBy, entity, call) {
  sorting_formatted <- NULL

  if (!is.null(sortBy)) {
    if (!rlang::is_bare_character(sortBy) || !rlang::is_vector(sortBy)) {
      sorting_error_msg(
        paste0(
          "In the list of options, the object passed to {.arg sortBy} must be ",
          "a character vector or NULL to use OpenAIRE Graph API's default."
        ),
        call = call
      )
    }
    if (!rlang::is_named(sortBy)) {
      sorting_error_msg(
        paste0(
          "In the list of options, the character vector passed to ",
          "{.arg sortBy} must be named."
        ),
        call = call
      )
    }
    if (!all(sortBy %in% c("ASC", "DESC"))) {
      sorting_error_msg(
        paste0(
          "In the list of options, character vector passed to {.arg sortBy} ",
          "can only contain \"ASC\" (ascending) or \"DESC\" (descending) ",
          "as input."
        ),
        call = call
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
    sorting_formatted <- paste0("sortBy=", sorting_formatted)
  }

  sorting_formatted
}

#' Check and format the "includeStats" option passed to `oag_set_options()`
#'
#' @inheritParams oag_options
#' @inheritParams sorting_error_msg
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to control the inclusion of query statistics. If
#' `includeStats` is NULL at the input, the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_stats <- function(includeStats, call) {
  if (!is.null(includeStats)) {
    if (!rlang::is_scalar_logical(includeStats) || anyNA(includeStats)) {
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
  }

  includeStats
}


#' Check and format the "pageSize" option passed to `oag_set_options()`
#'
#' @inheritParams oag_options
#' @inheritParams sorting_error_msg
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
  }

  pageSize
}


#' Check and format the "page" option passed to `oag_set_options()`
#'
#' @inheritParams oag_options
#' @inheritParams sorting_error_msg
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
  }

  page
}


#' Check and format the "cursor" option passed to `oag_set_options()`
#'
#' @inheritParams oag_options
#' @inheritParams sorting_error_msg
#'
#' @returns A string following OpenAIRE Graph API documentation that can be
#' added to an API call to specify that cursor-based paging should be used. If
#' `cursor` is NULL at the input, the result returned will also be NULL.
#'
#' @keywords internal

fmt_opt_cursor <- function(cursor, is_cursor_next = FALSE, call) {
  # Coerce `is_cursor_next` to FALSE if NULL
  if (is.null(is_cursor_next)) {
    is_cursor_next <- FALSE
  }

  # Check that `is_cursor_next` is a logical
  if (
    !is.null(is_cursor_next) &&
      (!rlang::is_scalar_logical(is_cursor_next) || anyNA(is_cursor_next))
  ) {
    cli::cli_abort(
      "In the list of options, {.var is_cursor_next} must be a logical scalar.",
      call = call
    )
  }

  # `is_cursor_next` cannot be used without using `cursor`
  if ((is.null(cursor) || rlang::is_scalar_logical(cursor)) && is_cursor_next) {
    cli::cli_abort(
      paste0(
        "In the list of options, {.var is_cursor_next} cannot be set to ",
        "`TRUE` without passing a string to {.arg cursor}."
      ),
      call = call
    )
  }
  if (!is.null(cursor)) {
    if (
      (!rlang::is_scalar_logical(cursor) || anyNA(cursor)) && !is_cursor_next
    ) {
      cli::cli_abort(
        paste0(
          "In the list of options, {.var cursor} must be a logical scalar or ",
          "NULL to use OpenAIRE Graph API's default."
        ),
        call = call
      )
    } else if (
      (!rlang::is_scalar_character(cursor) || anyNA(cursor)) && is_cursor_next
    ) {
      cli::cli_abort(
        paste0(
          "In the list of options, {.var cursor} must be a bare string when ",
          "{.var is_cursor_next} is set to `TRUE`."
        ),
        call = call
      )
    } else if (is_cursor_next) {
      cursor <- paste0("cursor=", cursor)
    } else if (cursor) {
      cursor <- "cursor=*"
    }
  }

  cursor
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

  switch(
    entity,
    "research-products" = rp,
    organizations = org,
    datasources = ds,
    projects = proj,
    persons = pers
  )
}

#' Compose an error message for sorting option issues with pre-formatted help
#'
#' @param msg A string with the message to add to the pre-formatted error
#' message.
#' @inheritParams oag_set_options
#'
#' @returns Nothing, use for side-effect only.
#' @export

sorting_error_msg <- function(msg, call = rlang::current_env()) {
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
