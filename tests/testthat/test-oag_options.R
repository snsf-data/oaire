# Tests for `oag_options()` ----------------------------------------------------

test_that("oag_options() returns NULL when no input provided", {
  expect_null(oag_options())
  expect_null(
    oag_options(
      sortBy = NULL,
      includeStats = NULL,
      pageSize = NULL,
      page = NULL,
      cursor = NULL
    )
  )
})

test_that("oag_options() returns object of class `oag_options`", {
  expect_s3_class(oag_options(sortBy = ""), "oag_options")
  expect_s3_class(oag_options(includeStats = ""), "oag_options")
  expect_s3_class(oag_options(pageSize = ""), "oag_options")
  expect_s3_class(oag_options(page = ""), "oag_options")
  expect_s3_class(oag_options(cursor = ""), "oag_options")
})

# Tests for `oag_set_options()` ------------------------------------------------

## Errors ----

test_that("oag_set_options() returns an error for incorrectly formatted options", {
  # `entity` arg is missing
  expect_error(oag_set_options(), "`entity`")
  # `entity` arg does not exist
  expect_error(oag_set_options(entity = "aaa"), "`entity`.+must.+be.+one.+of.+")
  # `entity` is mispelled but partially match
  expect_error(
    oag_set_options(entity = "datasource"),
    "`entity`.+must.+be.+one.+of.+"
  )

  # `sortBy` arg is not a character vector or NULL
  expect_error(
    oag_set_options(entity = "datasources", sortBy = 1),
    "`sortBy`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_set_options(entity = "datasources", sortBy = TRUE),
    "`sortBy`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_set_options(entity = "datasources", sortBy = list()),
    "`sortBy`.+must.+be.+a.+character.+vector"
  )

  # `sortBy` arg is not a named character vector or NULL
  expect_error(
    oag_set_options(entity = "datasources", sortBy = c("a", "b")),
    "`sortBy`.+must.+be.+named"
  )
  expect_error(
    oag_set_options(entity = "datasources", sortBy = c(a = "a", "b")),
    "`sortBy`.+must.+be.+named"
  )

  # `sortBy` arg is a named vector, but with incorrect input
  expect_error(
    oag_set_options(entity = "datasources", sortBy = c(a = "b")),
    "`sortBy`.+can.+only.+contain.+\"ASC\".+\"DESC\""
  )
  expect_error(
    oag_set_options(entity = "datasources", sortBy = c(a = "asc")),
    "`sortBy`.+can.+only.+contain.+\"ASC\".+\"DESC\""
  )

  # `sortBy` arg is a named vector, but sorting fields in names are not valid
  expect_error(
    oag_set_options(entity = "research-products", sortBy = c(a = "ASC")),
    "\"research-products\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_set_options(entity = "organizations", sortBy = c(a = "ASC")),
    "\"organizations\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_set_options(entity = "datasources", sortBy = c(a = "ASC")),
    "\"datasources\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_set_options(entity = "projects", sortBy = c(a = "ASC")),
    "\"projects\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_set_options(entity = "persons", sortBy = c(a = "ASC")),
    "\"persons\".+entity.+only.+following.+fields"
  )

  # `includeStats` arg is not a logical scalar or NULL
  expect_error(
    oag_set_options(entity = "datasources", includeStats = 1),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_set_options(entity = "datasources", includeStats = "TRUE"),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_set_options(entity = "datasources", includeStats = "NULL"),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_set_options(entity = "datasources", includeStats = c(TRUE, TRUE)),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )

  # `cursor` arg is not a logical scalar or NULL
  expect_error(
    oag_set_options(entity = "datasources", cursor = 1),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_set_options(entity = "datasources", cursor = "TRUE"),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_set_options(entity = "datasources", cursor = "NULL"),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_set_options(entity = "datasources", cursor = c(TRUE, TRUE)),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )

  # `page` arg is not a positive integer or NULL
  expect_error(
    oag_set_options(entity = "datasources", page = "1"),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", page = "TRUE"),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", page = "NULL"),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", page = c(1, 1)),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", page = 0),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", page = 1.1),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )

  # `pageSize` arg is not a positive integer (up to 100) or NULL
  expect_error(
    oag_set_options(entity = "datasources", pageSize = "1"),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", pageSize = "TRUE"),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", pageSize = "NULL"),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", pageSize = c(1, 1)),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", pageSize = 0),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_set_options(entity = "datasources", pageSize = 101),
    "`pageSize`.+cannot.+exceed.+100"
  )
  expect_error(
    oag_set_options(entity = "datasources", pageSize = 1.1),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )

  # Expect error when `pageSize` * `page` is greater than 10,000
  expect_error(
    oag_set_options(entity = "datasources", pageSize = 1, page = 10001),
    "Offset-based.+paging.+can.+be.+used.+to.+retrieve.+up.+to.+10,000"
  )
  expect_error(
    oag_set_options(entity = "datasources", pageSize = 21, page = 500),
    "Offset-based.+paging.+can.+be.+used.+to.+retrieve.+up.+to.+10,000"
  )

  # Expect error when cursor- and offset-based paging are used
  expect_error(
    oag_set_options(entity = "datasources", cursor = TRUE, page = 1),
    "Offset-.+and.+cursor-based.+paging.+cannot.+be.+used.+simultaneously."
  )
})

## Success ----

test_that("oag_set_options() returns is successful when expected", {
  # None of the valid entities should thrown an error
  expect_no_error(lapply(oag_entities(), \(x) oag_set_options(entity = x)))

  # None of the valid entities should thrown an error (for all valid values of
  # `includeStats`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, includeStats = FALSE)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, includeStats = NULL)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, includeStats = TRUE)
  }))

  # None of the valid entities should thrown an error (for all valid values of
  # `cursor`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, cursor = FALSE)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, cursor = NULL)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, cursor = TRUE)
  }))

  # None of the valid entities should thrown an error (for all valid values of
  # `page`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, page = 1)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, page = 1.0)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, page = 50)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, page = 100)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, page = 1000)
  }))

  # None of the valid entities should thrown an error (for all valid values of
  # `pageSize`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, pageSize = 1)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, pageSize = 1.0)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, pageSize = 50)
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_set_options(entity = x, page = 100)
  }))

  # Go across all entities and sorting fields and test they do not throw an
  # error when used as sorting field.
  expect_no_error(
    lapply(
      oag_entities(),
      \(ent) {
        lapply(
          get_sorting_fields(ent),
          \(fld) {
            sf <- c("ASC")
            names(sf) <- fld
            oag_set_options(entity = ent, sortBy = sf)
            sf[1] <- c("DESC")
            oag_set_options(entity = ent, sortBy = sf)
          }
        )
      }
    )
  )

  # Test a lot of possible combinations when using offset-based paging
  expect_no_error(
    lapply(
      oag_entities(),
      \(ent) {
        lapply(
          get_sorting_fields(ent),
          \(fld) {
            lapply(
              list(TRUE, FALSE, NULL),
              \(is) {
                lapply(
                  list(1, 50, 90, NULL),
                  \(ps) {
                    lapply(
                      list(1, 50, 90, NULL),
                      \(pg) {
                        sf <- c("ASC")
                        names(sf) <- fld
                        oag_set_options(ent, sf, is, ps, pg)
                        sf[1] <- c("DESC")
                        oag_set_options(ent, sf, is, ps, pg)
                      }
                    )
                  }
                )
              }
            )
          }
        )
      }
    )
  )

  # Test a lot of possible combinations when using cursor-based paging
  expect_no_error(
    lapply(
      oag_entities(),
      \(ent) {
        lapply(
          get_sorting_fields(ent),
          \(fld) {
            lapply(
              list(TRUE, FALSE, NULL),
              \(is) {
                lapply(
                  list(1, 50, 90, NULL),
                  \(ps) {
                    lapply(
                      list(TRUE, FALSE, NULL),
                      \(cs) {
                        sf <- c("ASC")
                        names(sf) <- fld
                        oag_set_options(ent, sf, is, ps, cursor = cs)
                        sf[1] <- c("DESC")
                        oag_set_options(ent, sf, is, ps, cursor = cs)
                      }
                    )
                  }
                )
              }
            )
          }
        )
      }
    )
  )
})
