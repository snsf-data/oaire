#' @keywords internal
.oag_token_cache <- new.env(parent = emptyenv())

#' Get the token to connect to the OpenAIRE Graph API
#'
#' The OpenAIRE Graph API can be accessed via authenticated and non
#' authenticated requests. One method for authenticated access is to use a
#' personal access token (PAT). Two types of tokens exists:
#' \itemize{
#'   \item regular token: valid for one hour and must be regenerated at the end
#'   of the validity period.
#'   \item refresh token: valid for one month, it allows to programmatically
#'   refresh an access token for one hour during the whole validity period.
#' }
#'
#' By default, the function will check for the existence of a refresh token in
#' the environment variables. If there is one, it will use it to create an
#' access token, valid for one hour, based on the refresh token. After that, a
#' new access token will be refreshed when the 1-hour period is over.
#'
#' If no refresh token exist (or if the user requested not to use it), the
#' function will fall back to using the regular token stored in the environment
#' variables. In the end, if there are no tokens in the environment variables,
#' the function will return nothing.
#'
#' @param refresh_token A string with a refresh token. The default is to check
#' the environment variables for a variable called `oag_api_refresh_token`.
#' @param token A string with a regular token. The default is to check the
#' environment variables for a variable called `oag_api_token`.
#'
#' @returns A token to access the OpenAIRE Graph API or NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' # Default is to use the refresh token stored in the environment variable
#' # `oag_api_refresh_token`.
#' token <- oag_api_token()
#' # To force the function using the regular token
#' token <- oag_api_token(refresh_token = NULL)
#' }

oag_api_token <- function(
  refresh_token = Sys.getenv("oag_api_refresh_token"),
  token = Sys.getenv("oag_api_token")
) {
  # First check if there is a non-empty refresh token
  if (!is.null(refresh_token) && nzchar(refresh_token)) {
    # Check the special environment `.oag_token_cache` if there is already an
    # access token that has been generated and is still valid.
    if (
      !is.null(.oag_token_cache$access_token) &&
        !is.null(.oag_token_cache$valid_until) &&
        Sys.time() < .oag_token_cache$valid_until
    ) {
      # Use the existing access token if still valid
      token <- .oag_token_cache$access_token

      # If there was no valid access token in the environment, create a new one
    } else {
      # The URL to which the refresh token request must be sent
      url <- "https://services.openaire.eu/uoa-user-management/api/users/getAccessToken?refreshToken="
      # Appending the refresh token to the URL
      url_with_token <- paste0(url, refresh_token)

      # Making the request to refresh and getting the response
      res <- httr2::request(utils::URLencode(url_with_token)) |>
        httr2::req_headers(Accept = "application/json") |>
        # In case of error (bad request, unauthorized...), we grab the error and
        # message returned and pass them to the R error returned,
        httr2::req_error(body = \(x) {
          c(
            httr2::resp_body_json(x)$error,
            httr2::resp_body_json(x)$message
          )
        }) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      # Update the access token and its validity period in the
      # `.oag_token_cache` environment.
      .oag_token_cache$access_token <- res$access_token
      .oag_token_cache$valid_until <- Sys.time() + res$expires_in
      token <- .oag_token_cache$access_token
    }
  }

  token
}
