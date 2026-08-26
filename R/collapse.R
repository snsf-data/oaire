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
  call <- rlang::caller_env()
  args <- rlang::dots_list(...)
  operator <- rlang::arg_match(operator, c("AND", "OR"))
  operator <- paste0(" ", operator, " ")

  # Check that additional arguments are only string
  is_input_bare_string <- mapply(rlang::is_bare_string, args)
  is_input_oag_str <- mapply(\(x) inherits(x, "oag_str"), args)

  if (!all(is_input_bare_string | is_input_oag_str)) {
    cli::cli_abort(
      c(
        "Arguments must strings or of class `oag_str`.",
        x = "{.arg {args[!is_input_bare_string]}} {?is/are} not string{?s}."
      ),
      call = call
    )
  }

  # Check that in the input combined to a single character vector contains at
  # least two elements. An operator cannot be added to a single element.
  if (length(args) < 2) {
    rlang::abort(
      paste0(
        "At least two strings are required to concatenate them with a logical ",
        "operator."
      ),
      call = call
    )
  }

  args_as_vec <- unlist(
    lapply(
      args,
      \(x) if (inherits(x, "oag_str")) x else paste0("\"", x, "\"")
    )
  )

  args_str <- paste0("(", paste0(args_as_vec, collapse = operator), ")")

  structure(args_str, class = c("oag_str", "character"))
}
