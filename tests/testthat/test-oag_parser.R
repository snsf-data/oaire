test_that("Parsing functions work", {
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

  snsf_pub <- oag_fetch(
    "research-products",
    type = "publication",
    relProjectFundingShortName = "SNSF",
    fromPublicationDate = paste0(year, "-", month, "-", day_from),
    toPublicationDate = paste0(year, "-", month, "-", day_to),
    options = oag_options(pageSize = 100, cursor = TRUE)
  )

  snsf_ds <- oag_fetch(
    "research-products",
    type = "dataset",
    relProjectFundingShortName = "SNSF",
    fromPublicationDate = paste0(year, "-", month, "-", day_from),
    toPublicationDate = paste0(year, "-", month, "-", day_to),
    options = oag_options(pageSize = 100, cursor = TRUE)
  )

  snsf_soft <- oag_fetch(
    "research-products",
    type = "software",
    relProjectFundingShortName = "SNSF",
    fromPublicationDate = paste0(year, "-", month, "-", day_from),
    toPublicationDate = paste0(year, "-", month, "-", day_to),
    options = oag_options(pageSize = 100, cursor = TRUE)
  )

  snsf_other <- oag_fetch(
    "research-products",
    type = "other",
    relProjectFundingShortName = "SNSF",
    fromPublicationDate = paste0(year, "-", month, "-", day_from),
    toPublicationDate = paste0(year, "-", month, "-", day_to),
    options = oag_options(pageSize = 100, cursor = TRUE)
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
