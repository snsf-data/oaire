#' Concatenate strings with a logical operator (AND-OR-NOT)
#'
#' In the OpenAIRE Graph API, logical operators can be used in filters. These
#' functions allows concatenating series of strings with "AND"/"OR" operator, or
#' to negate a string with the "NOT" operator, following the API documentation.
#'
#' @param ... The string(s) to negate or to concatenate with "AND"/"OR". All
#' values passed to the `...` argument must be bare strings. In `concat_not()`,
#' only a single string can be passed to `...`.
#'
#' @returns A string of class `oag_str`.
#' @export
#'
#' @examples
#' concat_and("science", "history")
#' concat_or("biology", "humanities", "geology")
#' concat_not("model")
#'
#' # Combining different "concat" functions
#' concat_and("science", "history", concat_not("model"))
#' concat_and(concat_or("biology", "humanities"), concat_or("history", "geology"))
#'
#' # In a query context
#' oag_query("datasources", search = concat_or("biology", "humanities", "geology"))

concat_and <- function(...) {
  concat_and_or(..., operator = "AND")
}

#' @rdname concat_and
#' @export

concat_or <- function(...) {
  concat_and_or(..., operator = "OR")
}

#' @rdname concat_and
#' @export

concat_not <- function(...) {
  args <- rlang::dots_list(...)
  # Check that no more than one argument have been passed to the function
  if (length(args) > 1) {
    cli::cli_abort(
      c(
        "No more that 1 argument can be passed to the function.",
        i = "You passed {length(args)} argument{?s}."
      )
    )
  }

  # Now that arguments is of length one, take it out of the list
  str <- args[[1]]

  if (!rlang::is_bare_string(str) && !inherits(str, "oag_str")) {
    cli::cli_abort("The argument passed to the function must be a bare string.")
  }
  # Surround the string with double quotes if not an `oag_str` object
  if (!inherits(str, "oag_str")) {
    str <- paste0("\"", str, "\"")
  }

  # Negate the string
  str_not <- paste0("NOT ", str)

  # Set the class of the string and return it
  structure(str_not, class = c("oag_str", "character"))
}

#' Concatenate string arguments with an "AND" or "OR" as separator
#'
#' In the OpenAIRE Graph API, logical operators can be used in filters. This
#' function allows concatenating series of strings with the logical operators
#' "AND"/"OR", following the API documentation.
#'
#' @inheritParams concat_and
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
    cli::cli_abort(
      paste0(
        "At least two strings are required to concatenate them with a logical ",
        "operator."
      ),
      call = call
    )
  }

  # Surround bare strings (but not `oag_str` type of strings) with double quotes
  args_as_vec <- unlist(
    lapply(
      args,
      \(x) if (inherits(x, "oag_str")) x else paste0("\"", x, "\"")
    )
  )

  # Concatenate the input with the selected operator
  args_str <- paste0("(", paste0(args_as_vec, collapse = operator), ")")

  # Set the class of and return the concatenated string
  structure(args_str, class = c("oag_str", "character"))
}
