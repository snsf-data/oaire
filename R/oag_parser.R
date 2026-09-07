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
parse_collected_from <- function(res, var = "collectedfrom") {
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
