# Tests for `oag_parse_object()` -----------------------------------------------

oag_opts <- oag_options(pageSize = 100, page = 1)

## Success ----

test_that("Parsing functions work (research products", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  oag_pub <- suppressMessages(
    oag_fetch("research-products", type = "publication", options = oag_opts)
  )

  oag_ds <- suppressMessages(
    oag_fetch("research-products", type = "dataset", options = oag_opts)
  )

  oag_soft <- suppressMessages(
    oag_fetch("research-products", type = "software", options = oag_opts)
  )

  oag_other <- suppressMessages(
    oag_fetch("research-products", type = "other", options = oag_opts)
  )

  expect_no_error(
    parsed_pub <- suppressMessages(
      oag_parse_object(
        snsf_pub,
        entity = "research-products",
        type = "publication"
      )
    )
  )
  expect_equal(length(snsf_pub), nrow(parsed_pub))

  expect_no_error(
    parsed_ds <- suppressMessages(
      oag_parse_object(snsf_ds, entity = "research-products", type = "dataset")
    )
  )
  expect_equal(length(snsf_ds), nrow(parsed_ds))

  expect_no_error(
    parsed_soft <- suppressMessages(
      oag_parse_object(
        snsf_soft,
        entity = "research-products",
        type = "software"
      )
    )
  )
  expect_equal(length(snsf_soft), nrow(parsed_soft))

  expect_no_error(
    parsed_other <- suppressMessages(
      oag_parse_object(snsf_other, entity = "research-products", type = "other")
    )
  )
  expect_equal(length(snsf_other), nrow(parsed_other))
})

test_that("Parsing functions work (organizations)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_orgs <- suppressMessages(oag_fetch("organizations", options = oag_opts))

  expect_no_error(
    parsed_orgs <- suppressMessages(oag_parse_object(
      res_orgs[samp],
      entity = "organizations"
    ))
  )

  expect_equal(length(res_orgs[samp]), nrow(parsed_orgs))
})

test_that("Parsing functions work (projects)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_proj <- suppressMessages(oag_fetch("projects", options = oag_opts))

  expect_no_error(
    parsed_proj <- suppressMessages(oag_parse_object(
      res_proj[samp],
      entity = "projects"
    ))
  )

  expect_equal(length(res_proj[samp]), nrow(parsed_proj))
})

test_that("Parsing functions work (persons)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_prsn <- suppressMessages(oag_fetch("persons", options = oag_opts))

  expect_no_error(
    parsed_prsn <- suppressMessages(oag_parse_object(
      res_prsn,
      entity = "persons"
    ))
  )

  expect_equal(length(res_prsn), nrow(parsed_prsn))
})

test_that("Parsing functions work (datasources)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_ds <- suppressMessages(oag_fetch("datasources", options = oag_opts))

  expect_no_error(
    parsed_ds <- suppressMessages(oag_parse_object(
      res_ds,
      entity = "datasources"
    ))
  )

  expect_equal(length(res_ds), nrow(parsed_ds))
})
