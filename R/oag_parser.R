#==============================================================================|
#                           ---- Object parsers ----
#==============================================================================|

#' Parse the research products objects
#'
#' @param object An OpenAIRE Graph object as returned by `oag_fetch()` or
#' `oag_request()`.
#' @param type A string with the type of research product contained in `object`
#' @param selection A character vector with the variables to select in the
#' OpenAIRE Graph object passed to `object`.
#'
#' @returns A tibble with the parsed object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch some "research products" data from the OpenAIRE Graph
#' res_prod <-  oag_fetch(
#'   "research-products",
#'   type = "publication",
#'   relProjectFundingShortName = "SNSF",
#'   fromPublicationDate = "2023-01-01",
#'   toPublicationDate = "2023-01-31",
#'   options = oag_options(pageSize = 100, cursor = TRUE)
#' )
#'
#' # Parsing the returned research products object
#' res_prod_df <- parse_research_products(res_prod, type = "publication")
#'
#' # Fetch some "organizations" data from the OpenAIRE Graph
#' res_org <- oag_fetch(
#'   "organizations",
#'   countryCode = "CH",
#'   options = oag_options(pageSize = 100, cursor = TRUE)
#' )
#'
#' # Parsing the returned organizations object
#' res_org_df <- parse_entity_organizations(res_org)
#'
#' # Fetch some "projects" data from the OpenAIRE Graph
#' res_proj <- oag_fetch(
#'   "projects",
#'   fundingShortName = "SNSF",
#'   startYear = "2025",
#'   options = oag_options(pageSize = 100, cursor = TRUE)
#' )
#'
#' # Parsing the returned projects object
#' res_proj_df <- parse_entity_projects(res_proj)
#' }

parse_research_products <- function(
  object,
  type = c("publication", "dataset", "software", "other"),
  selection = NULL
) {
  type <- rlang::arg_match(
    type,
    c("publication", "dataset", "software", "other")
  )
  # Initiate an empty table with the variable of the research product type
  # already set.
  res_prod_df <- init_res_prod_df(type, selection)

  if (is.null(selection)) {
    # Only keep the variable to parse that are in "selection"
    selection <- colnames(res_prod_df)
  }

  # Named vector where the names are the research product variables and the
  # value their corresponding parser.
  vars_with_fn <- c(
    # Common variables
    id = "parse_string",
    type = "parse_string",
    originalIds = "parse_list",
    mainTitle = "parse_string",
    subTitle = "parse_string",
    authors = "parse_authors",
    bestAccessRight = "parse_best_access_right",
    contributors = "parse_list",
    countries = "parse_countries",
    coverages = "parse_list",
    dateOfCollection = "parse_datetime",
    descriptions = "parse_list",
    embargoEndDate = "parse_date",
    indicators = "parse_indicators",
    instances = "parse_instances",
    language = "parse_language",
    lastUpdateTimeStamp = "parse_string",
    pids = "parse_pids",
    publicationDate = "parse_date",
    publisher = "parse_string",
    sources = "parse_list",
    formats = "parse_list",
    subjects = "parse_subjects",
    isGreen = "parse_bool",
    openAccessColor = "parse_string",
    isInDiamondJournal = "parse_bool",
    publiclyFunded = "parse_bool",
    projects = "parse_projects",
    organizations = "parse_organizations",
    communities = "parse_communities",
    collectedFrom = "parse_collected_from",
    # Specific to publications
    container = "parse_container",
    # Specific to data sources
    size = "parse_string",
    version = "parse_string",
    geoLocations = "parse_geo_locations",
    # Specific to software
    documentationUrls = "parse_list",
    codeRepositoryUrl = "parse_string",
    programmingLanguage = "parse_string",
    # Specific to other
    contactPeople = "parse_list",
    contactGroups = "parse_list",
    tools = "parse_list"
  )

  # Only keep the variable to parse that are in "selection"
  vars_with_fn <- vars_with_fn[names(vars_with_fn) %in% selection]

  # Initiate a progress bar for the data parsing process
  cli::cli_progress_bar(
    total = length(object),
    type = "custom",
    format = paste0(
      "{cli::pb_spin} Parsed {cli::pb_current} work(?s) out of  ",
      "{length(object)}... {cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}]"
    )
  )

  # Loop over each element in `object`
  for (i in seq_along(object)) {
    cli::cli_progress_update(set = i)

    # Go over all variables to parse and extract structured data from the raw
    # data.
    parsed_vars <- lapply(names(vars_with_fn), \(x) {
      do.call(vars_with_fn[[x]], list(res = object[[i]], var = x))
    }) |>
      # Make sure that set to list any data not being a scalar
      lapply(
        \(x) {
          if (rlang::is_scalar_atomic(x)) {
            x
          } else {
            list(x)
          }
        }
      )

    names(parsed_vars) <- names(vars_with_fn)

    # Turn the list of parsed data into a tibble and bind it to the tibble with
    # the already parsed data.
    vars_df <- tibble::as_tibble(parsed_vars)
    res_prod_df <- rbind(res_prod_df, vars_df)
  }
  cli::cli_progress_done()

  res_prod_df
}

#' @rdname parse_research_products
#' @export

parse_entity_organizations <- function(object, selection = NULL) {
  # Initiate an empty table with the variable of the organizations type already
  # set.
  res_org_df <- init_orgs_df(selection)

  if (is.null(selection)) {
    # Only keep the variable to parse that are in "selection"
    selection <- colnames(res_org_df)
  }

  # Named vector where the names are the organizations variables and the value
  # their corresponding parser.
  vars_with_fn <- c(
    id = "parse_string",
    legalShortName = "parse_string",
    legalName = "parse_string",
    alternativeNames = "parse_list",
    websiteUrl = "parse_string",
    country = "parse_country",
    pids = "parse_pids",
    originalIds = "parse_list",
    fundings = "parse_org_fundings",
    collectedFrom = "parse_collected_from"
  )

  # Only keep the variable to parse that are in "selection"
  vars_with_fn <- vars_with_fn[names(vars_with_fn) %in% selection]

  # Initiate a progress bar for the data parsing process
  cli::cli_progress_bar(
    total = length(object),
    type = "custom",
    format = paste0(
      "{cli::pb_spin} Parsed {cli::pb_current} organization{?s} out of ",
      "{length(object)}... {cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}]"
    )
  )

  # Loop over each element in `object`
  for (i in seq_along(object)) {
    cli::cli_progress_update(set = i)

    # Go over all variables to parse and extract structured data from the raw
    # data.
    parsed_vars <- lapply(names(vars_with_fn), \(x) {
      do.call(vars_with_fn[[x]], list(res = object[[i]], var = x))
    }) |>
      # Make sure that set to list any data not being a scalar
      lapply(
        \(x) {
          if (rlang::is_scalar_atomic(x)) {
            x
          } else {
            list(x)
          }
        }
      )

    names(parsed_vars) <- names(vars_with_fn)

    # Turn the list of parsed data into a tibble and bind it to the tibble with
    # the already parsed data.
    vars_df <- tibble::as_tibble(parsed_vars)
    res_org_df <- rbind(res_org_df, vars_df)
  }
  cli::cli_progress_done()

  res_org_df
}

#' @rdname parse_research_products
#' @export

parse_entity_projects <- function(object, selection = NULL) {
  # Initiate an empty table with the variable of the organizations type already
  # set.
  res_proj_df <- init_proj_df(selection)

  if (is.null(selection)) {
    # Only keep the variable to parse that are in "selection"
    selection <- colnames(res_proj_df)
  }

  # Named vector where the names are the organizations variables and the value
  # their corresponding parser.
  vars_with_fn <- c(
    id = "parse_string",
    code = "parse_string",
    acronym = "parse_string",
    title = "parse_string",
    callIdentifier = "parse_string",
    fundings = "parse_proj_fundings",
    granted = "parse_granted",
    h2020Programmes = "parse_h2020",
    funding = "parse_proj_funding",
    keywords = "parse_string",
    openAccessMandateForDataset = "parse_bool",
    openAccessMandateForPublications = "parse_bool",
    startDate = "parse_date",
    endDate = "parse_date",
    subjects = "parse_list",
    summary = "parse_string",
    websiteUrl = "parse_string"
  )

  # Only keep the variable to parse that are in "selection"
  vars_with_fn <- vars_with_fn[names(vars_with_fn) %in% selection]

  # Initiate a progress bar for the data parsing process
  cli::cli_progress_bar(
    total = length(object),
    type = "custom",
    format = paste0(
      "{cli::pb_spin} Parsed {cli::pb_current} project{?s} out of ",
      "{length(object)}... {cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}]"
    )
  )

  # Loop over each element in `object`
  for (i in seq_along(object)) {
    cli::cli_progress_update(set = i)

    # Go over all variables to parse and extract structured data from the raw
    # data.
    parsed_vars <- lapply(names(vars_with_fn), \(x) {
      do.call(vars_with_fn[[x]], list(res = object[[i]], var = x))
    }) |>
      # Make sure that set to list any data not being a scalar
      lapply(
        \(x) {
          if (rlang::is_scalar_atomic(x)) {
            x
          } else {
            list(x)
          }
        }
      )

    names(parsed_vars) <- names(vars_with_fn)

    # Turn the list of parsed data into a tibble and bind it to the tibble with
    # the already parsed data.
    vars_df <- tibble::as_tibble(parsed_vars)
    res_proj_df <- rbind(res_proj_df, vars_df)
  }
  cli::cli_progress_done()

  res_proj_df
}

parse_entity_persons <- function(object, selection = NULL) {
  # Initiate an empty table with the variable of the organizations type already
  # set.
  res_prsn_df <- init_prsn_df(selection)

  if (is.null(selection)) {
    # Only keep the variable to parse that are in "selection"
    selection <- colnames(res_prsn_df)
  }

  # Named vector where the names are the organizations variables and the value
  # their corresponding parser.
  vars_with_fn <- c(
    id = "parse_string",
    originalId = "parse_list",
    givenName = "parse_string",
    familyName = "parse_string",
    alternativeNames = "parse_list",
    biography = "parse_string",
    subject = "parse_list",
    indicator = "parse_indicator",
    context = "parse_context",
    consent = "parse_bool",
    coAuthors = "parse_list"
  )

  # Only keep the variable to parse that are in "selection"
  vars_with_fn <- vars_with_fn[names(vars_with_fn) %in% selection]

  # Initiate a progress bar for the data parsing process
  cli::cli_progress_bar(
    total = length(object),
    type = "custom",
    format = paste0(
      "{cli::pb_spin} Parsed {cli::pb_current} person{?s} out of ",
      "{length(object)}... {cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}]"
    )
  )

  # Loop over each element in `object`
  for (i in seq_along(object)) {
    cli::cli_progress_update(set = i)

    # Go over all variables to parse and extract structured data from the raw
    # data.
    parsed_vars <- lapply(names(vars_with_fn), \(x) {
      do.call(vars_with_fn[[x]], list(res = object[[i]], var = x))
    }) |>
      # Make sure that set to list any data not being a scalar
      lapply(
        \(x) {
          if (rlang::is_scalar_atomic(x)) {
            x
          } else {
            list(x)
          }
        }
      )

    names(parsed_vars) <- names(vars_with_fn)

    # Turn the list of parsed data into a tibble and bind it to the tibble with
    # the already parsed data.
    vars_df <- tibble::as_tibble(parsed_vars)
    res_prsn_df <- rbind(res_prsn_df, vars_df)
  }
  cli::cli_progress_done()

  res_prsn_df
}

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

#' @keywords internal
parse_list <- function(res, var) {
  if (is.null(res[[var]])) {
    NULL
  } else {
    unlist(null_to_na(res[[var]]))
  }
}

#' @keywords internal
parse_pids <- function(res, var = "pids") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          scheme = x[["scheme"]] %||% NA_character_,
          value = x[["value"]] %||% NA_character_,
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_geo_locations <- function(res, var = "geoLocations") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          box = x[["box"]] %||% NA_character_,
          place = x[["place"]] %||% NA_character_,
          point = x[["point"]] %||% NA_character_,
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_language <- function(res, var = "language") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      code = res[[var]][["code"]] %||% NA_character_,
      label = res[[var]][["label"]] %||% NA_character_
    )
  }
}

#' @keywords internal
parse_instances <- function(res, var = "instances") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          accessRight_code = x[["accessRight"]][["code"]] %||% NA_character_,
          accessRight_label = x[["accessRight"]][["label"]] %||% NA_character_,
          accessRight_openAccessRoute = x[["accessRight"]][[
            "openAccessRoute"
          ]] %||%
            NA_character_,
          accessRight_scheme = x[["accessRight"]][["scheme"]] %||%
            NA_character_,
          alternateIdentifiers = list(
            lapply(
              x[["alternateIdentifiers"]],
              \(x) tibble::tibble(scheme = x[["scheme"]], value = x[["value"]])
            ) |>
              Reduce(x = _, "rbind")
          ),
          articleProcessingCharge_amount = x[["articleProcessingCharge"]][[
            "amount"
          ]] %||%
            NA_character_,
          articleProcessingCharge_currency = x[["articleProcessingCharge"]][[
            "currency"
          ]] %||%
            NA_character_,
          license = x[["license"]] %||% NA_character_,
          pids = list(
            lapply(
              x[["pids"]],
              \(x) tibble::tibble(scheme = x[["scheme"]], value = x[["value"]])
            ) |>
              Reduce(x = _, "rbind")
          ),
          publicationDate = x[["publicationDate"]] %||% NA_character_,
          refereed = x[["refereed"]] %||% NA_character_,
          hostedBy_key = x[["hostedBy"]][["key"]] %||% NA_character_,
          hostedBy_value = x[["hostedBy"]][["value"]] %||% NA_character_,
          collectedFrom_key = x[["collectedFrom"]][["key"]] %||% NA_character_,
          collectedFrom_value = x[["collectedFrom"]][["value"]] %||%
            NA_character_,
          type = x[["type"]] %||% NA_character_,
          urls = list(unlist(null_to_na(x[["urls"]])))
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_best_access_right <- function(res, var = "bestAccessRight") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      code = res[[var]][["code"]] %||% NA_character_,
      label = res[[var]][["label"]] %||% NA_character_,
      scheme = res[[var]][["scheme"]] %||% NA_character_
    )
  }
}

#' @keywords internal
parse_projects <- function(res, var = "projects") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tmp_res <- tibble::tibble(
          id = x[["id"]] %||% NA_character_,
          code = x[["code"]] %||% NA_character_,
          acronym = x[["acronym"]] %||% NA_character_,
          title = x[["title"]] %||% NA_character_,
          provenance = x[["provenance"]][["provenance"]] %||% NA_character_,
          provenance_trust = x[["provenance"]][["trust"]] %||% NA_real_,
          validationDate = x[["validated"]][["validationDate"]] %||%
            NA_character_,
          validatedByFunder = x[["validated"]][["validatedByFunder"]] %||%
            NA_real_
        )
        if (length(x[["funder"]]) == 1) {
          tibble::add_column(
            tmp_res,
            funder = x[["funder"]] %||% NA_character_,
            .after = "title"
          )
        } else if (length(x[["funder"]]) > 1) {
          tibble::add_column(
            tmp_res,
            funder_shortName = x[["funder"]][["shortName"]] %||% NA_character_,
            funder_name = x[["funder"]][["name"]] %||% NA_character_,
            funder_jurisdiction = x[["funder"]][["jurisdiction"]] %||%
              NA_character_,
            funder_funding_stream = x[["funder"]][["fundingStream"]] %||%
              NA_character_,
            .after = "title"
          )
        }
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_countries <- function(res, var = "countries") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          code = x[["code"]] %||% NA_character_,
          label = x[["label"]] %||% NA_character_,
          provenance = x[["provenance"]][["provenance"]] %||% NA_character_,
          provenance_trust = x[["provenance"]][["trust"]] %||% NA_real_
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_country <- function(res, var = "country") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      code = res[[var]][["code"]] %||% NA_character_,
      label = res[[var]][["label"]] %||% NA_character_,
      provenance = res[[var]][["provenance"]][["provenance"]] %||%
        NA_character_,
      provenance_trust = res[[var]][["provenance"]][["trust"]] %||% NA_real_
    )
  }
}

#' @keywords internal
parse_proj_fundings <- function(res, var = "fundings") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          fundingStream_description = x[["fundingStream"]][["description"]] %||%
            NA_character_,
          fundingStream_id = x[["fundingStream"]][["id"]] %||% NA_character_,
          jurisdiction = x[["jurisdiction"]] %||% NA_character_,
          name = x[["name"]] %||% NA_character_,
          shortName = x[["shortName"]] %||% NA_real_
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_proj_funding <- function(res, var = "funding") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      funder_id = res[[var]][["funder"]][["id"]] %||% NA_character_,
      funder_shortname = res[[var]][["funder"]][["shortname"]] %||% NA_character_,
      funder_name = res[[var]][["funder"]][["name"]] %||% NA_character_,
      funder_jurisdiction_code = res[[var]][["funder"]][["jurisdiction"]][[
        "code"
      ]] %||%
        NA_character_,
      funder_jurisdiction_label = res[[var]][["funder"]][["jurisdiction"]][[
        "label"
      ]] %||%
        NA_character_,
      funder_pid = res[[var]][["funder"]][["pid"]] %||% NA_character_,
      level0 = list(
        tibble::tibble(
          id = res[[var]][["level0"]][["id"]] %||% NA_character_,
          description = res[[var]][["level0"]][["description"]] %||% NA_character_,
          name = res[[var]][["level0"]][["name"]] %||% NA_character_
        )
      ),
      level1 = list(
        tibble::tibble(
          id = res[[var]][["level1"]][["id"]] %||% NA_character_,
          description = res[[var]][["level1"]][["description"]] %||% NA_character_,
          name = res[[var]][["level1"]][["name"]] %||% NA_character_
        )
      ),
      level2 = list(
        tibble::tibble(
          id = res[[var]][["level2"]][["id"]] %||% NA_character_,
          description = res[[var]][["level2"]][["description"]] %||% NA_character_,
          name = res[[var]][["level2"]][["name"]] %||% NA_character_
        )
      )
    )
  }
}

#' @keywords internal
parse_org_fundings <- function(res, var = "fundings") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          funder_id = x[["funder"]][["id"]] %||% NA_character_,
          funder_shortname = x[["funder"]][["shortname"]] %||% NA_character_,
          funder_name = x[["funder"]][["name"]] %||% NA_character_,
          funder_jurisdiction_code = x[["funder"]][["jurisdiction"]][[
            "code"
          ]] %||%
            NA_character_,
          funder_jurisdiction_label = x[["funder"]][["jurisdiction"]][[
            "label"
          ]] %||%
            NA_character_,
          funder_pid = x[["funder"]][["pid"]] %||% NA_character_,
          level0 = list(
            tibble::tibble(
              id = x[["level0"]][["id"]] %||% NA_character_,
              description = x[["level0"]][["description"]] %||% NA_character_,
              name = x[["level0"]][["name"]] %||% NA_character_
            )
          ),
          level1 = list(
            tibble::tibble(
              id = x[["level1"]][["id"]] %||% NA_character_,
              description = x[["level1"]][["description"]] %||% NA_character_,
              name = x[["level1"]][["name"]] %||% NA_character_
            )
          ),
          level2 = list(
            tibble::tibble(
              id = x[["level2"]][["id"]] %||% NA_character_,
              description = x[["level2"]][["description"]] %||% NA_character_,
              name = x[["level2"]][["name"]] %||% NA_character_
            )
          )
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_subjects <- function(res, var = "subjects") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          subject_scheme = x[["subject"]][["scheme"]] %||% NA,
          subject_value = x[["subject"]][["value"]] %||% NA,
          provenance = x[["provenance"]][["provenance"]] %||% NA_character_,
          provenance_trust = x[["provenance"]][["trust"]] %||% NA_real_
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_collected_from <- function(res, var = "collectedFrom") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          key = x[["key"]] %||% NA_character_,
          value = x[["value"]] %||% NA_character_
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_authors <- function(res, var = "authors") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          fullName = x[["fullName"]] %||% NA_character_,
          rank = x[["rank"]] %||% NA_integer_,
          name = x[["name"]] %||% NA_character_,
          surname = x[["surname"]] %||% NA_character_,
          pid_scheme = x[["pid"]][["id"]][["scheme"]] %||% NA_character_,
          pid_value = x[["pid"]][["id"]][["value"]] %||% NA_character_,
          pid_provenance_ = x[["pid"]][["provenance"]][["provenance"]] %||%
            NA_character_,
          pid_provenance_trust = x[["pid"]][["provenance"]][["trust"]] %||%
            NA_real_,
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}


#' @keywords internal
parse_granted <- function(res, var = "granted") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      currency = res[[var]][["currency"]] %||% NA_integer_,
      fundedAmount = res[[var]][["fundedAmount"]] %||% NA_integer_,
      totalCost = res[[var]][["totalCost"]] %||% NA_integer_
    )
  }
}

#' @keywords internal
parse_h2020 <- function(res, var = "h2020Programmes") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          code = x[["code"]] %||% NA_character_,
          description = x[["description"]] %||% NA_integer_
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}


#' @keywords internal
parse_context <- function(res, var = "context") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      affiliation = res[[var]][["affiliation"]] %||% NA_character_,
      department = res[[var]][["department"]] %||% NA_character_,
      country = res[[var]][["country"]] %||% NA_character_
    )
  }
}

#' @keywords internal
parse_indicator <- function(res, var = "indicator") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      citationCount = res[[var]][["citationCount"]] %||% NA_integer_,
      downloads = res[[var]][["downloads"]] %||% NA_integer_
    )
  }
}

#' @keywords internal
parse_indicators <- function(res, var = "indicators") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      influence = res[[var]][["citationImpact"]][["influence"]] %||%
        NA_integer_,
      influenceClass = res[[var]][["citationImpact"]][["influenceClass"]] %||%
        NA_character_,
      citationCount = res[[var]][["citationImpact"]][["citationCount"]] %||%
        NA_integer_,
      citationClass = res[[var]][["citationImpact"]][["citationClass"]] %||%
        NA_character_,
      popularity = res[[var]][["citationImpact"]][["popularity"]] %||%
        NA_integer_,
      popularityClass = res[[var]][["citationImpact"]][["popularityClass"]] %||%
        NA_character_,
      impulse = res[[var]][["citationImpact"]][["impulse"]] %||%
        NA_integer_,
      impulseClass = res[[var]][["citationImpact"]][["impulseClass"]] %||%
        NA_character_,
      usage_counts_downloads = res[[var]][["usageCounts"]][["downloads"]] %||%
        NA_integer_,
      usage_counts_views = res[[var]][["usageCounts"]][["views"]] %||%
        NA_integer_,
    )
  }
}

#' @keywords internal
parse_organizations <- function(res, var = "organizations") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          legalName = x[["legalName"]] %||% NA_character_,
          acronym = x[["acronym"]] %||% NA_character_,
          id = x[["id"]] %||% NA_character_,
          pids_scheme = x[["pids"]][["scheme"]] %||% NA_character_,
          pids_value = x[["pids"]][["value"]] %||% NA_character_
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_communities <- function(res, var = "communities") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    lapply(
      res[[var]],
      \(x) {
        tibble::tibble(
          code = x[["code"]] %||% NA_character_,
          label = x[["label"]] %||% NA_character_,
          provenance = x[["provenance"]][["provenance"]] %||% NA_character_,
          provenance_trust = x[["provenance"]][["trust"]] %||% NA_real_,
        )
      }
    ) |>
      Reduce(x = _, "rbind")
  }
}

#' @keywords internal
parse_container <- function(res, var = "container") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      edition = res[[var]][["edition"]] %||% NA_character_,
      iss = res[[var]][["iss"]] %||% NA_integer_,
      issnLinking = res[[var]][["issnLinking"]] %||% NA_character_,
      issnOnline = res[[var]][["issnOnline"]] %||% NA_character_,
      issnPrinted = res[[var]][["issnPrinted"]] %||% NA_character_,
      name = res[[var]][["name"]] %||% NA_character_,
      sp = res[[var]][["sp"]] %||% NA_integer_,
      ep = res[[var]][["ep"]] %||% NA_integer_,
      vol = res[[var]][["vol"]] %||% NA_integer_
    )
  }
}

#==============================================================================|
#                          ---- Tibble templates ----
#==============================================================================|

#' Initiate an empty tibble to used to store parsed entities objects
#'
#' The function generate an empty tibble that will be used to collect the parsed
#' data coming from a query to an OpenAIRE Graph entity.
#' Graph API.
#'
#' @param type A string with the type of research products to extract
#' @param selection A character vector with the variables of the research
#' products object that should be returned.
#'
#' @returns An empty tibble.
#' @keywords internal

init_res_prod_df <- function(
  type = c("publication", "dataset", "software", "other"),
  selection = NULL
) {
  type <- rlang::arg_match(
    type,
    c("publication", "dataset", "software", "other")
  )
  # fmt: skip
  # Empty tibble with the variables common to all research product types
  rp_df <- tibble::tribble(
    ~id, ~type, ~originalIds, ~mainTitle, ~subTitle, ~authors, ~bestAccessRight,
    ~contributors, ~countries, ~coverages, ~dateOfCollection, ~descriptions,
    ~embargoEndDate, ~indicators, ~instances, ~language, ~lastUpdateTimeStamp,
    ~pids, ~publicationDate, ~publisher, ~sources, ~formats, ~subjects,
    ~isGreen, ~openAccessColor, ~isInDiamondJournal, ~publiclyFunded, ~projects,
    ~organizations, ~communities, ~collectedFrom
  )

  # Depending on the research product type, additional variables are added
  if (type == "publication") {
    rp_pub_df <- tibble::tribble(~container)
    rp_df <- tibble::add_column(rp_df, rp_pub_df)
  } else if (type == "dataset") {
    rp_data_df <- tibble::tribble(~size, ~version, ~geoLocations)
    rp_df <- tibble::add_column(rp_df, rp_data_df)
  } else if (type == "software") {
    rp_soft_df <- tibble::tribble(
      ~documentationUrls,
      ~codeRepositoryUrl,
      ~programmingLanguage
    )
    rp_df <- tibble::add_column(rp_df, rp_soft_df)
  } else if (type == "other") {
    rp_other_df <- tibble::tribble(~contactPeople, ~contactGroups, ~tools)
    rp_df <- tibble::add_column(rp_df, rp_other_df)
  }

  # Only keep the variables passed to "selection" if not null
  if (!is.null(selection)) {
    rp_df <- rp_df[, colnames(rp_df) %in% selection]
  }

  rp_df
}

#' @rdname init_res_prod_df
#' @keywords internal

init_data_sources_df <- function(selection = NULL) {
  # fmt: skip
  # Empty tibble with the variables of the data sources entity
  ds_df <- tibble::tribble(
    ~id, ~originalIds, ~pids, ~type, ~openaireCompatibility, ~officialName,
    ~englishName, ~websiteUrl, ~logoUrl, ~dateOfValidation, ~description,
    ~subjects, ~languages, ~contentTypes, ~releaseStartDate, ~releaseEndDate,
    ~accessRights, ~uploadRights, ~databaseAccessRestriction,
    ~dataUploadRestriction, ~versioning, ~citationGuidelineUrl, ~pidSystems,
    ~certificates, ~policies, ~journal, ~missionStatementUrl
  )
  if (!is.null(selection)) {
    ds_df <- ds_df[, colnames(ds_df) %in% selection]
  }

  ds_df
}

#' @rdname init_res_prod_df
#' @keywords internal

init_orgs_df <- function(selection = NULL) {
  # fmt: skip
  # Empty tibble with the variables of the organizations entity
  orgs_df <- tibble::tribble(
    ~id, ~legalShortName, ~legalName, ~alternativeNames, ~websiteUrl, ~country,
    ~pids, ~originalIds, ~fundings, ~collectedFrom
  )
  if (!is.null(selection)) {
    orgs_df <- orgs_df[, colnames(orgs_df) %in% selection]
  }

  orgs_df
}

#' @rdname init_res_prod_df
#' @keywords internal

init_proj_df <- function(selection = NULL) {
  # fmt: skip
  # Empty tibble with the variables of the organizations entity
  proj_df <- tibble::tribble(
    ~id, ~code, ~acronym, ~title, ~callIdentifier, ~fundings, ~granted,
    ~h2020Programmes, ~funding, ~keywords, ~openAccessMandateForDataset,
    ~openAccessMandateForPublications, ~startDate, ~endDate, ~subjects,
    ~summary, ~websiteUrl
  )
  if (!is.null(selection)) {
    proj_df <- proj_df[, colnames(proj_df) %in% selection]
  }

  proj_df
}

#' @rdname init_res_prod_df
#' @keywords internal

init_prsn_df <- function(selection = NULL) {
  # fmt: skip
  # Empty tibble with the variables of the persons entity
  proj_df <- tibble::tribble(
    ~id, ~originalId, ~givenName, ~familyName, ~alternativeNames, ~biography,
    ~subject, ~indicator, ~context, ~consent, ~coAuthors
  )
  if (!is.null(selection)) {
    proj_df <- proj_df[, colnames(proj_df) %in% selection]
  }

  proj_df
}

#==============================================================================|
#                               ---- Helpers ----
#==============================================================================|

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @keywords internal
null_to_na <- function(x, na_type = NA_character_) {
  if (is.null(x)) {
    NULL
  } else {
    lapply(x, \(y) y %||% na_type)
  }
}
