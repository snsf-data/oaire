#' Concatenate  string arguments with an "AND" or "OR" as separator
#'
#' In the OpenAIRE Graph API, logical operators can be used in filters. This
#' function allows concatenating series of strings with the logical operators
#' "AND"/"OR", following the API documentation.
#'
#' @inheritParams ... Strings to concatenate to a single string.
#' @param operator A string indicating the logical operator to use to
#' concatenate the arguments passed to `...`. Only "AND" and "OR" (case
#' sensitive) are accepted.
#'
#' @returns A string of class `oag_str`.
#' @keywords internal

concat_and_or <- function(..., operator) {
  args <- rlang::dots_list(...)
  operator <- rlang::arg_match(operator, c("AND", "OR"))
  operator <- paste0(" ", operator, " ")

  args_as_vec <- unlist(
    lapply(
      args,
      \(x) if (inherits(x, "oag_str")) x else paste0("\"", x, "\"")
    )
  )

  args_str <- paste0("(", paste0(args_as_vec, collapse = operator), ")")

  structure(args_str, class = c("oag_str", "character"))
}
