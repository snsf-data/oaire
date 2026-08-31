test_that("Check for error when fetching data using wrong tokens", {
  skip_on_cran()

  expect_error(
    {
      # Since different tests took place before, when need to nullify any cached
      # token first.
      .oag_token_cache$access_token <- NULL
      .oag_token_cache$valid_until <- NULL
      oag_api_token(refresh_token = "qqq")
    },
    "Bad.+Request.+Unauthorized"
  )

  skip_if_not(nzchar(Sys.getenv("oag_api_token_expired")))

  expect_error(
    oag_fetch(
      "research-products",
      type = "publication",
      options = oag_options(page = 1, pageSize = 1),
      token = oag_api_token(
        refresh_token = NULL,
        token = Sys.getenv("oag_api_token_expired")
      )
    ),
    "Token expired"
  )

  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  expect_error(
    {
      # Since different tests took place before, when need to nullify any cached
      # token first.
      .oag_token_cache$access_token <- NULL
      .oag_token_cache$valid_until <- NULL
      oag_fetch(
        "research-products",
        type = "publication",
        options = oag_options(page = 1, pageSize = 1),
        token = paste0(oag_api_token(), "1") # Alter the refresh token
      )
    },
    "Token invalid"
  )
})
