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
#' oag_api_url("datasources")

oag_api_url <- function(entity) {
  # Only entities returned by `oag_entities()` are accepted
  entity <- rlang::arg_match(entity, oag_entities())
  # Base OpenAIRE Graph API URL
  api_url <- "https://api.openaire.eu/graph/v3"

  # Building and returning the entity API URL
  file.path(api_url, entity)
}

#' Compose a query and fetch the data from the OpenAIRE Graph API
#'
#' @description
#' The OpenAIRE Graph API works with filters and options (see
#' \url{https://graph.openaire.eu/docs/apis/graph-api/}). The functions
#' interpret additional (named) argument(s) passed to the `...` argument as
#' filter(s). The filter(s) must be named with the name of the field to filter,
#' as described in the OpenAIRE Graph API. Options can be specified with
#' `oag_options()`.
#'
#' Note that `oag_fetch()` is based on `oag_request()`, which use throttling
#' to make sure that the performed requests never exceed the rate limit (60 per
#' hour for unauthenticated requests, compared to 7200 for authenticated ones).
#'
#' @inheritParams oag_api_url
#' @param ... Filter(s) to use when composing the query. The filter(s) must be
#' named and be a valid filter as referenced in the OpenAIRE Graph API
#' documentation. For example, `publicationYear = "2020"` can be used to
#' filter publication from 2020 when working with the "research-products"
#' entity.
#' @param options A set of options (created with `oag_options()`) used to
#' compose the query. Options allow to control sorting, paging, etc. See
#' `?oag_options()` for more details.
#'
#' @returns A string with the query URL for `oag_query()` and he results from
#' the performed query in a JSON format for `oag_fetch()`.
#' @export
#'
#' @examples
#' # Create a query
#' oag_query(
#'   "research-products",
#'   publicationYear = "2020",
#'   options = oag_options(sortBy = c(relevance = "ASC"))
#' )

oag_query <- function(entity, ..., options = NULL) {
  if (is.null(rlang::caller_call())) {
    call <- rlang::current_env()
  } else {
    call <- rlang::caller_env()
  }

  # Only entities returned by `oag_entities()` are accepted
  entity <- rlang::arg_match(entity, oag_entities(), error_call = call)
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
        cursor = options[["cursor"]],
        is_cursor_next = options[["is_cursor_next"]],
        call = call
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
        ),
        call = call
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
        ),
        call = call
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
  query_entity <- oag_api_url(entity)
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

  structure(query, class = c("oag_query", "character"))
}

#' @param token A valid regular or refresh token to authenticate to the
#' OpenAIRE Graph API, granting the user of 7'200 requests per hour. If no
#' token is passed, the request will be unauthenticated, with a rate limit of
#' 60 requests per hour.
#'
#' @export
#' @rdname oag_query
#' @examples
#' \dontrun{
#' # Create a query then pipe it to `oag_fetch()` to get the results
#' rp_data <- oag_query(
#'   "research-products",
#'   publicationYear = "2020",
#'   options = oag_options(sortBy = c(relevance = "ASC"))
#' ) |>
#'   oag_request()
#' }

oag_fetch <- function(
  entity,
  ...,
  options = NULL,
  token = oag_api_token()
) {
  # Dry run to check that entity, filters, and options are correctly formatted
  invisible(oag_query(
    entity,
    ...,
    options = options
  ))
  # If page is not NULL we simply fetch the data for that single page
  if (!is.null(options[["page"]])) {
    res <- oag_request(oag_query(entity, ..., options = options), token = token)
  } else {
    # Here we will use paging. To know how many records (and thus pages) have to
    # be accessed, we make a dry run with the filters passed by the user but
    # for a single page of size 1 so we can retrieve the numbers of records.
    query_1_n <- oag_query(
      entity = entity,
      ...,
      options = oag_options(
        page = 1,
        pageSize = 1
      )
    )

    # Access the number of records and compute, given the page size, the
    # required number of pages to query to get all the records.
    res_query_1_n <- oag_request(query_1_n, token = token)
    n_records <- res_query_1_n[["header"]][["numFound"]]
    p_size <- options[["pageSize"]]
    n_query <- ceiling(n_records / p_size)

    # Get the number of available tokens according to API terms of use
    n_tokens <- available_oag_tokens()

    # Inform user when the number of tokens available is smaller than required
    # to fetch all the data.
    if (n_query > n_tokens) {
      cli::cli_warn(
        c(
          paste0(
            "The number of requests ({n_query}) is greater than the number of ",
            "available requests ({n_tokens})."
          ),
          i = paste0(
            "It is likely that the data fetching will pause at some point to ",
            "comply with the API terms of use."
          )
        )
      )
    }

    res <- list()
    # Since we will use paging, the page options must be NULL. It is irrelevant
    # for cursor-based paging. For offset-based paging, we will just go along
    # the number of pages in `n_query`.
    options[["page"]] <- NULL
    # If `cursor` is not NULL, `use_cursor` is by default set to FALSE (ensuring
    # that the first page is always accessed with "cursor=*" when using
    # cursor-based paging).
    if (is.null(options[["cursor"]])) {
      use_cursor <- FALSE
    } else {
      use_cursor <- options[["cursor"]]
    }

    cli::cli_progress_bar("Fetching OpenAIRE data", total = n_query)
    # Loop across the pages
    for (i in seq_len(n_query)) {
      # Make the request to the data from the current page
      res[[i]] <- oag_request(
        oag_query(entity, ..., options = options),
        token = token
      )
      # If cursor-based paging is used, set `is_cursor_next` to TRUE and pass
      # the next cursor to `cursor`.
      if (use_cursor && i < n_query) {
        options[["cursor"]] <- res[[i]][["header"]][["nextCursor"]]
        options[["is_cursor_next"]] <- TRUE
      }
      cli::cli_progress_update()
    }
    cli::cli_progress_done()

    # Flatten the results from all pages (but it is still a difficult JSON
    # object to handle though).
    res <- unlist(lapply(res, \(x) x[["results"]]), recursive = FALSE)
  }

  res
}

#' Perform the request to the OpenAIRE Graph API
#'
#' @param url An `oag_query` object or a string with a valid URL to make a call
#' to the API.
#' @param token A string with a valid token to call the API. `oag_api_token()`
#' automatically handle retrieving the user's token when saved as environment
#' variable. See `?oag_api_token()` for more details.
#'
#' @returns The response from the API as a JSON object.
#' @export
#'
#' @examples
#' \dontrun{
#' oag_query(
#'   "research-products",
#'   publicationYear = "2020",
#'   options = oag_options(sortBy = c(relevance = "ASC"))
#' ) |>
#'   oag_request()
#' }

oag_request <- function(url, token = oag_api_token()) {
  if (!inherits(url, "oag_query") && !rlang::is_scalar_character(url)) {
    cli::cli_abort(
      "The object passed to {.arg url} must be a string of class `oag_query`.",
    )
  }

  # Prepare the request specifying that we will read the results a JSON
  req <- httr2::request(utils::URLencode(url)) |>
    httr2::req_headers(Accept = "application/json")

  # Set rate limit for requests depending on whether the user has a token or not
  if (nzchar(token)) {
    req <- httr2::req_auth_bearer_token(req, token) |>
      httr2::req_throttle(capacity = 7200, fill_time_s = 3600)
  } else {
    req <- httr2::req_throttle(req, capacity = 60, fill_time_s = 3600)
  }

  # Perform the request will catching the error message in case the request did
  # not perform as expected.
  res <- req |>
    httr2::req_error(body = \(x) {
      c(
        httr2::resp_body_json(x)$error,
        httr2::resp_body_json(x)$message
      )
    }) |>
    httr2::req_perform(error_call = rlang::current_env()) |>
    httr2::resp_body_json()

  res
}

#' Return how many request tokens are available to query the OpenAIRE Graph API
#'
#' @param realm A string used to identify the "realm"
#'
#' @returns The number of request tokens available to query the OpenAIRE Graph
#' API.
#' @keywords internal

available_oag_tokens <- function(realm = "openaire") {
  throttle_status <- httr2::throttle_status()
  oag_realm <- grepl("openaire", throttle_status[["realm"]])
  throttle_status[["tokens"]][oag_realm]
}
