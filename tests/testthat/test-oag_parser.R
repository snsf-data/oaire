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
      oag_parse_object(oag_pub, type = "publication")
    )
  )
  expect_equal(length(oag_pub), nrow(parsed_pub))

  expect_no_error(
    parsed_ds <- suppressMessages(oag_parse_object(oag_ds, type = "dataset"))
  )
  expect_equal(length(oag_ds), nrow(parsed_ds))

  expect_no_error(
    parsed_soft <- suppressMessages(
      oag_parse_object(oag_soft, type = "software")
    )
  )
  expect_equal(length(oag_soft), nrow(parsed_soft))

  expect_no_error(
    parsed_other <- suppressMessages(
      oag_parse_object(oag_other, type = "other")
    )
  )
  expect_equal(length(oag_other), nrow(parsed_other))
})

test_that("Parsing functions work (organizations)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_orgs <- suppressMessages(oag_fetch("organizations", options = oag_opts))

  expect_no_error(
    parsed_orgs <- suppressMessages(oag_parse_object(res_orgs))
  )

  expect_equal(length(res_orgs), nrow(parsed_orgs))
})

test_that("Parsing functions work (projects)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_proj <- suppressMessages(oag_fetch("projects", options = oag_opts))

  expect_no_error(
    parsed_proj <- suppressMessages(oag_parse_object(res_proj))
  )

  expect_equal(length(res_proj), nrow(parsed_proj))
})

test_that("Parsing functions work (persons)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_prsn <- suppressMessages(oag_fetch("persons", options = oag_opts))

  expect_no_error(
    parsed_prsn <- suppressMessages(oag_parse_object(res_prsn))
  )

  expect_equal(length(res_prsn), nrow(parsed_prsn))
})

test_that("Parsing functions work (datasources)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_ds <- suppressMessages(oag_fetch("datasources", options = oag_opts))

  expect_no_error(
    parsed_ds <- suppressMessages(oag_parse_object(res_ds))
  )

  expect_equal(length(res_ds), nrow(parsed_ds))
})

## Error ----

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
  oag_orgs <- suppressMessages(oag_fetch("organizations", options = oag_opts))
  oag_dat_s <- suppressMessages(oag_fetch("datasources", options = oag_opts))
  oag_proj <- suppressMessages(oag_fetch("projects", options = oag_opts))
  oag_prsn <- suppressMessages(oag_fetch("persons", options = oag_opts))

  # Manually remove the `oag_object` class so we can test errors
  class(oag_pub) <- "list"
  class(oag_ds) <- "list"
  class(oag_soft) <- "list"
  class(oag_other) <- "list"
  class(oag_orgs) <- "list"
  class(oag_dat_s) <- "list"
  class(oag_proj) <- "list"
  class(oag_prsn) <- "list"

  # Entity is missing
  expect_error(
    oag_parse_object(oag_pub),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )
  expect_error(
    oag_parse_object(oag_ds),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )
  expect_error(
    oag_parse_object(oag_soft),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )
  expect_error(
    oag_parse_object(oag_other),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )
  expect_error(
    oag_parse_object(oag_orgs),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )
  expect_error(
    oag_parse_object(oag_dat_s),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )
  expect_error(
    oag_parse_object(oag_proj),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )
  expect_error(
    oag_parse_object(oag_prsn),
    "`entity`.+cannot.+be.+NULL.+not.+an.+object.+of.+class.+oag_object"
  )

  # Entity is not a valid entity
  expect_error(
    oag_parse_object(oag_pub, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_ds, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_soft, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_other, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_orgs, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_dat_s, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_proj, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_prsn, entity = "YEAH"),
    "`entity`.+must.+be.+one.+of"
  )

  # `type` is not valid for "research-products
  expect_error(
    oag_parse_object(oag_other, entity = "research-products", type = 1),
    "`type`.+must.+be.+a.+character"
  )
  expect_error(
    oag_parse_object(oag_other, entity = "research-products", type = TRUE),
    "`type`.+must.+be.+a.+character"
  )
  expect_error(
    oag_parse_object(oag_other, entity = "research-products", type = list()),
    "`type`.+must.+be.+a.+character"
  )
  expect_error(
    oag_parse_object(oag_other, entity = "research-products", type = NA),
    "`type`.+must.+be.+a.+character"
  )
  expect_error(
    oag_parse_object(oag_other, entity = "research-products", type = NULL),
    "`type`.+must.+be.+a.+character"
  )
  expect_error(
    oag_parse_object(oag_other, entity = "research-products", type = "1"),
    "`type`.+must.+be.+one.+of"
  )
  expect_error(
    oag_parse_object(oag_other, entity = "research-products", type = "soft"),
    "`type`.+must.+be.+one.+of"
  )

  # `selection` must be a character vector
  expect_error(
    oag_parse_object(oag_prsn, entity = "persons", selection = NA),
    "`selection`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_parse_object(oag_prsn, entity = "persons", selection = 1),
    "`selection`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_parse_object(oag_prsn, entity = "persons", selection = TRUE),
    "`selection`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_parse_object(oag_prsn, entity = "persons", selection = list("a")),
    "`selection`.+must.+be.+a.+character.+vector"
  )

  # `selection` contains variables not valid for the object to pars
  expect_error(
    oag_parse_object(
      oag_pub,
      entity = "research-products",
      type = "publication",
      selection = names(get_org_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+research.+products.+object"
  )
  expect_error(
    oag_parse_object(
      oag_ds,
      entity = "research-products",
      type = "dataset",
      selection = names(get_org_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+research.+products.+object"
  )
  expect_error(
    oag_parse_object(
      oag_soft,
      entity = "research-products",
      type = "software",
      selection = names(get_org_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+research.+products.+object"
  )
  expect_error(
    oag_parse_object(
      oag_other,
      entity = "research-products",
      type = "other",
      selection = names(get_org_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+research.+products.+object"
  )
  expect_error(
    oag_parse_object(
      oag_orgs,
      entity = "organizations",
      selection = names(get_prod_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+organizations.+object"
  )
  expect_error(
    oag_parse_object(
      oag_dat_s,
      entity = "datasources",
      selection = names(get_prod_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+datasources.+object"
  )
  expect_error(
    oag_parse_object(
      oag_proj,
      entity = "projects",
      selection = names(get_prod_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+projects.+object"
  )
  expect_error(
    oag_parse_object(
      oag_prsn,
      entity = "persons",
      selection = names(get_prod_vars())
    ),
    "variables.+are.+not.+valid.+for.+an.+persons.+object"
  )

  oag_parse_object(
    oag_proj,
    entity = "projects",
    selection = names(get_proj_vars())
  )
})
