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
    lapply(null_to_na(res[[var]]), unlist)
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
parse_geolocations <- function(res, var = "geolocations") {
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
  type = c("publication", "data", "software", "other"),
  selection = NULL
) {
  type <- rlang::arg_match(type, c("publication", "data", "software", "other"))
  # fmt: skip
  # Empty tibble with the variables common to all research product types
  rp_df <- tibble::tribble(
    ~id, ~type, ~originalIds, ~mainTitle, ~subTitle, ~authors, ~bestAccessRight,
    ~contributors, ~countries, ~coverages, ~dateOfCollection, ~descriptions,
    ~embargoEndDate, ~indicators, ~instances, ~language, ~lastUpdateTimeStamp,
    ~pids, ~publicationDate, ~publisher, ~sources, ~formats, ~subjects,
    ~isGreen, ~openAccessColor, ~isInDiamondJournal, ~publiclyFunded, ~projects,
    ~organizations, ~communities, ~collectedfromdfrom
  )

  # Depending on the research product type, additional variables are added
  if (type == "publication") {
    rp_pub_df <- tibble::tribble(~container)
    rp_df <- tibble::add_column(rp_df, rp_pub_df)
  } else if (type == "data") {
    rp_data_df <- tibble::tribble(~size, ~version, ~geolocations)
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
    ~id, ~legalName, ~alternativeNames, ~websiteUrl, ~country, ~pids
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
    ~h2020Programmes, ~keywords, ~openAccessMandateForDataset,
    ~openAccessMandateForPublications, ~startDate, ~endDate, ~subjects,
    ~summary, ~websiteUrl
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

