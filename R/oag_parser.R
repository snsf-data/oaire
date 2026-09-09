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
#' res_prod_df <- oag_parse_object(
#'   res_prod,
#'   entity = "research-products",
#'   type = "publication"
#' )
#'
#' # Fetch some "organizations" data from the OpenAIRE Graph
#' res_org <- oag_fetch(
#'   "organizations",
#'   countryCode = "CH",
#'   options = oag_options(pageSize = 100, cursor = TRUE)
#' )
#'
#' # Parsing the returned organizations object
#' res_org_df <- oag_parse_object(res_org, entity = "organizations")
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
#' res_proj_df <- oag_parse_object(res_proj, entity = "projects")
#'
#' # Fetch some "persons" data from the OpenAIRE Graph
#' res_prsn <- oag_fetch(
#'   "persons",
#'   lastName = "Gorin",
#'   options = oag_options(pageSize = 100, cursor = TRUE)
#' )
#'
#' # Parsing the returned projects object
#' res_prsn_df <- oag_parse_object(res_prsn, entity = "persons")
#'
#' # Fetch some "persons" data from the OpenAIRE Graph
#' res_ds <- oag_fetch(
#'   "datasources",
#'   country = "CH",
#'   options = oag_options(pageSize = 100, cursor = TRUE)
#' )
#'
#' # Parsing the returned projects object
#' res_ds_df <- oag_parse_object(res_ds, entity = "datasources")
#' }

oag_parse_object <- function(
  object,
  entity,
  selection = NULL,
  type = c("publication", "dataset", "software", "other")
) {
  type <- rlang::arg_match(
    type,
    c("publication", "dataset", "software", "other")
  )

  res_df <- NULL

  # Named vector where names are the object variables and the value their
  # corresponding parser of the variable.
  vars_with_fn <- switch(
    entity,
    "research-products" = get_prod_vars(),
    "organizations" = get_org_vars(),
    "datasources" = get_ds_vars(),
    "projects" = get_proj_vars(),
    "persons" = get_prsn_vars()
  )

  # Only keep the variable to parse that are in "selection"
  if (is.null(selection)) {
    selection <- names(vars_with_fn)
  }

  # Only keep the variables to parse that are in "selection"
  vars_with_fn <- vars_with_fn[names(vars_with_fn) %in% selection]

  # Initiate a progress bar for the data parsing process
  cli::cli_progress_bar(
    total = length(object),
    type = "custom",
    format = paste0(
      "{cli::pb_spin} Parsed {cli::pb_current} out of {length(object)} ",
      "{entity} objects... {cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}]"
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
    res_df <- rbind(res_df, vars_df)
  }
  cli::cli_progress_done()

  res_df

}

#==============================================================================|
#                          ---- Variable parsers ----
#==============================================================================|

## Global variables parsers ----------------------------------------------------

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

## Research products variables parsers -----------------------------------------

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

## Organizations variables parsers ---------------------------------------------

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

## Projects variables parsers --------------------------------------------------

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
parse_proj_funding <- function(res, var = "funding") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      funder_id = res[[var]][["funder"]][["id"]] %||% NA_character_,
      funder_shortname = res[[var]][["funder"]][["shortname"]] %||%
        NA_character_,
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
          description = res[[var]][["level0"]][["description"]] %||%
            NA_character_,
          name = res[[var]][["level0"]][["name"]] %||% NA_character_
        )
      ),
      level1 = list(
        tibble::tibble(
          id = res[[var]][["level1"]][["id"]] %||% NA_character_,
          description = res[[var]][["level1"]][["description"]] %||%
            NA_character_,
          name = res[[var]][["level1"]][["name"]] %||% NA_character_
        )
      ),
      level2 = list(
        tibble::tibble(
          id = res[[var]][["level2"]][["id"]] %||% NA_character_,
          description = res[[var]][["level2"]][["description"]] %||%
            NA_character_,
          name = res[[var]][["level2"]][["name"]] %||% NA_character_
        )
      )
    )
  }
}

## Persons variables parsers ---------------------------------------------------

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

## Datasources variables parsers -----------------------------------------------

#' @keywords internal
parse_type <- function(res, var = "type") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      scheme = res[[var]][["scheme"]] %||% NA_character_,
      value = res[[var]][["value"]] %||% NA_character_
    )
  }
}

#' @keywords internal
parse_journal <- function(res, var = "journal") {
  if (is.null(res[[var]])) {
    NULL
  } else {
    tibble::tibble(
      edition = res[[var]][["edition"]] %||% NA_character_,
      iss = res[[var]][["iss"]] %||% NA_character_,
      issnLinking = res[[var]][["issnLinking"]] %||% NA_character_,
      issnOnline = res[[var]][["issnOnline"]] %||% NA_character_,
      issnPrinted = res[[var]][["issnPrinted"]] %||% NA_character_,
      name = res[[var]][["name"]] %||% NA_character_,
      sp = res[[var]][["sp"]] %||% NA_character_,
      ep = res[[var]][["ep"]] %||% NA_character_,
      vol = res[[var]][["vol"]] %||% NA_character_
    )
  }
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


  )
}

  )
}

  )
}

}

}
