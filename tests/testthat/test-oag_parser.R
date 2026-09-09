test_that("Parsing functions work (research products", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  day_from <- sample(1:22, 1)
  day_to <- day_from + 6
  day_from <- as.character(ifelse(
    day_from < 10,
    paste0("0", day_from),
    day_from
  ))
  day_to <- as.character(ifelse(day_to < 10, paste0("0", day_to), day_to))
  month <- sample(1:12, 1)
  month <- as.character(ifelse(month < 10, paste0("0", month), month))
  year <- as.character(sample(2023:2025, 1))

  snsf_pub <- suppressMessages(
    oag_fetch(
      "research-products",
      type = "publication",
      relProjectFundingShortName = "SNSF",
      fromPublicationDate = paste0(year, "-", month, "-", day_from),
      toPublicationDate = paste0(year, "-", month, "-", day_to),
      options = oag_options(pageSize = 100, cursor = TRUE)
    )
  )

  snsf_ds <- suppressMessages(
    oag_fetch(
      "research-products",
      type = "dataset",
      relProjectFundingShortName = "SNSF",
      fromPublicationDate = paste0(year, "-", month, "-", day_from),
      toPublicationDate = paste0(year, "-", month, "-", day_to),
      options = oag_options(pageSize = 100, cursor = TRUE)
    )
  )

  snsf_soft <- suppressMessages(
    oag_fetch(
      "research-products",
      type = "software",
      relProjectFundingShortName = "SNSF",
      fromPublicationDate = paste0(year, "-", month, "-", day_from),
      toPublicationDate = paste0(year, "-", month, "-", day_to),
      options = oag_options(pageSize = 100, cursor = TRUE)
    )
  )

  snsf_other <- suppressMessages(
    oag_fetch(
      "research-products",
      type = "other",
      relProjectFundingShortName = "SNSF",
      fromPublicationDate = paste0(year, "-", month, "-", day_from),
      toPublicationDate = paste0(year, "-", month, "-", day_to),
      options = oag_options(pageSize = 100, cursor = TRUE)
    )
  )

  expect_no_error(
    parsed_pub <- suppressMessages(
      parse_research_products(snsf_pub, type = "publication")
    )
  )
  expect_equal(length(snsf_pub), nrow(parsed_pub))

  expect_no_error(
    parsed_ds <- suppressMessages(
      parse_research_products(snsf_ds, type = "dataset")
    )
  )
  expect_equal(length(snsf_ds), nrow(parsed_ds))

  expect_no_error(
    parsed_soft <- suppressMessages(
      parse_research_products(snsf_soft, type = "software")
    )
  )
  expect_equal(length(snsf_soft), nrow(parsed_soft))

  expect_no_error(
    parsed_other <- suppressMessages(
      parse_research_products(snsf_other, type = "other")
    )
  )
  expect_equal(length(snsf_other), nrow(parsed_other))
})

test_that("Parsing functions work (organizations)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_orgs <- suppressMessages(
    oag_fetch(
      "organizations",
      countryCode = "BE",
      options = oag_options(pageSize = 100, cursor = TRUE)
    )
  )

  samp <- sample(seq_along(res_orgs), 100)

  expect_no_error(
    parsed_orgs <- suppressMessages(parse_entity_organizations(res_orgs[samp]))
  )

  expect_equal(length(res_orgs[samp]), nrow(parsed_orgs))
})

test_that("Parsing functions work (projects)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_proj <- suppressMessages(
    oag_fetch(
      "projects",
      fundingShortName = "SNSF",
      startYear = "2025",
      options = oag_options(pageSize = 100, cursor = TRUE)
    )
  )

  samp <- sample(seq_along(res_proj), 100)

  expect_no_error(
    parsed_proj <- suppressMessages(parse_entity_projects(res_proj[samp]))
  )

  expect_equal(length(res_proj[samp]), nrow(parsed_proj))
})

test_that("Parsing functions work (persons)", {
  skip_if_not(nzchar(Sys.getenv("oag_api_refresh_token")))

  res_prsn <- suppressMessages(
    oag_fetch(
      "persons",
      lastName = "Gorin",
      options = oag_options(pageSize = 100, cursor = TRUE)
    )
  )

  expect_no_error(
    parsed_prsn <- suppressMessages(parse_entity_projects(res_prsn))
  )

  expect_equal(length(res_prsn), nrow(parsed_prsn))
})
