# Tests for `oag_api_url()` ----------------------------------------------------

test_that("oag_api_url() returns an error for incorrect entities", {
  # `entity` arg is missing
  expect_error(oag_api_url(), "`entity`")
  # `entity` arg does not exist
  expect_error(oag_api_url(entity = "aaa"), "`entity`.+must.+be.+one.+of.+")
  # `entity` is mispelled but partially match
  expect_error(
    oag_api_url(entity = "datasource"),
    "`entity`.+must.+be.+one.+of.+"
  )
})

## Success ----

test_that("oag_api_url() is successful when entity is valid", {
  # None of the valid entities should thrown an error
  expect_no_error(lapply(oag_entities(), \(x) oag_api_url(entity = x)))
})


# Tests for `oag_query()` ------------------------------------------------------

# The tests in this section are basically a copy of the tests for
# `oag_set_options()`, but used via the of `oag_options()` passed to
# `oag_query()`.

## Errors ----

test_that("oag_query() returns an error for incorrectly formatted options", {
  # `entity` arg is missing
  expect_error(oag_query(), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_query(1), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_query(c()), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_query(list()), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_query(list("a")), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_query(TRUE), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_query(NULL), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(NA), "`entity`.+must.+be.+a.+character.+vector")

  # Value passed to `entity` arg is not a valid entity
  expect_error(oag_query(entity = "aaa"), "`entity`.+must.+be.+one.+of.+")
  expect_error(oag_query(entity = c("a", "b"), "`entity`.+must.+be.+one.+of.+"))
  # `entity` is mispelled but partially match
  expect_error(
    oag_query(entity = "datasource"),
    "`entity`.+must.+be.+one.+of.+"
  )

  # Options are not of `oag_options` class
  expect_error(
    oag_query(entity = "datasources", options = 1),
    "Options.+passed.+to.+a.+query.+must.+be.+of.+class.+`oag_options`"
  )
  expect_error(
    oag_query(entity = "datasources", options = NA),
    "Options.+passed.+to.+a.+query.+must.+be.+of.+class.+`oag_options`"
  )
  expect_error(
    oag_query(entity = "datasources", options = TRUE),
    "Options.+passed.+to.+a.+query.+must.+be.+of.+class.+`oag_options`"
  )
  expect_error(
    oag_query(entity = "datasources", options = list()),
    "Options.+passed.+to.+a.+query.+must.+be.+of.+class.+`oag_options`"
  )
  expect_error(
    oag_query(entity = "datasources", options = list(cursor="*")),
    "Options.+passed.+to.+a.+query.+must.+be.+of.+class.+`oag_options`"
  )

  # `sortBy` arg is not a character vector or NULL
  expect_error(
    oag_query(entity = "datasources", options = oag_options(sortBy = 1)),
    "`sortBy`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_query(entity = "datasources", options = oag_options(sortBy = NA)),
    "`sortBy`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(sortBy = TRUE)
    ),
    "`sortBy`.+must.+be.+a.+character.+vector"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(sortBy = list())
    ),
    "`sortBy`.+must.+be.+a.+character.+vector"
  )

  # `sortBy` arg is not a named character vector or NULL
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(sortBy = c("a", "b"))
    ),
    "`sortBy`.+must.+be.+named"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(sortBy = c(a = "a", "b"))
    ),
    "`sortBy`.+must.+be.+named"
  )

  # `sortBy` arg is a named vector, but with incorrect input
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(sortBy = c(a = "b"))
    ),
    "`sortBy`.+can.+only.+contain.+\"ASC\".+\"DESC\""
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(sortBy = c(a = "asc"))
    ),
    "`sortBy`.+can.+only.+contain.+\"ASC\".+\"DESC\""
  )

  # `sortBy` arg is a named vector, but sorting fields in names are not valid
  expect_error(
    oag_query(
      entity = "research-products",
      options = oag_options(sortBy = c(a = "ASC"))
    ),
    "\"research-products\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_query(
      entity = "organizations",
      options = oag_options(sortBy = c(a = "ASC"))
    ),
    "\"organizations\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(sortBy = c(a = "ASC"))
    ),
    "\"datasources\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_query(
      entity = "projects",
      options = oag_options(sortBy = c(a = "ASC"))
    ),
    "\"projects\".+entity.+only.+following.+fields"
  )
  expect_error(
    oag_query(
      entity = "persons",
      options = oag_options(sortBy = c(a = "ASC"))
    ),
    "\"persons\".+entity.+only.+following.+fields"
  )

  # `includeStats` arg is not a logical scalar or NULL
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(includeStats = 1)
    ),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(includeStats = NA)
    ),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(includeStats = "TRUE")
    ),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(includeStats = "NULL")
    ),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(includeStats = c(TRUE, TRUE))
    ),
    "`includeStats`.+must.+be.+a.+logical.+scalar"
  )

  # `cursor` arg is not a logical scalar or NULL
  expect_error(
    oag_query(entity = "datasources", options = oag_options(cursor = 1)),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(entity = "datasources", options = oag_options(cursor = NA)),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = "TRUE")
    ),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = "NULL")
    ),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = c(TRUE, TRUE))
    ),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = 1, is_cursor_next = NULL)
    ),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = 1, is_cursor_next = FALSE)
    ),
    "`cursor`.+must.+be.+a.+logical.+scalar"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = 1, is_cursor_next = TRUE)
    ),
    "`cursor`.+must.+be.+a.+bare.+string.+when.+`is_cursor_next`.+is.+`TRUE`"
  )

  # `is_cursor_next` arg is not a logical scalar or NULL
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(is_cursor_next = 1)
    ),
    "In.+the.+list.+of.+options,.+`is_cursor_next`.+must.+be.+a.+logical.+"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(is_cursor_next = "a")
    ),
    "In.+the.+list.+of.+options,.+`is_cursor_next`.+must.+be.+a.+logical.+"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(is_cursor_next = "TRUE")
    ),
    "In.+the.+list.+of.+options,.+`is_cursor_next`.+must.+be.+a.+logical.+"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(is_cursor_next = NA)
    ),
    "In.+the.+list.+of.+options,.+`is_cursor_next`.+must.+be.+a.+logical.+"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(is_cursor_next = c(TRUE, TRUE))
    ),
    "In.+the.+list.+of.+options,.+`is_cursor_next`.+must.+be.+a.+logical.+"
  )

  # Incompatibilities between `is_cursor_next` and `cursor`
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = TRUE, is_cursor_next = TRUE)
    ),
    "`is_cursor_next`.+cannot.+be.+set.+to.+`TRUE`.+string.+to.+`cursor`"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = FALSE, is_cursor_next = TRUE)
    ),
    "`is_cursor_next`.+cannot.+be.+set.+to.+`TRUE`.+string.+to.+`cursor`"
  )

  # `page` arg is not a positive integer or NULL
  expect_error(
    oag_query(entity = "datasources", options = oag_options(page = "1")),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(entity = "datasources", options = oag_options(page = NA)),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(page = "TRUE")
    ),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(page = "NULL")
    ),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(page = c(1, 1))
    ),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(entity = "datasources", options = oag_options(page = 0)),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(entity = "datasources", options = oag_options(page = 1.1)),
    "`page`.+must.+be.+a.+scalar.+positive.+integer"
  )

  # `pageSize` arg is not a positive integer (up to 100) or NULL
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = "1")
    ),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = NA)
    ),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = "TRUE")
    ),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = "NULL")
    ),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = c(1, 1))
    ),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = 0)
    ),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = 101)
    ),
    "`pageSize`.+cannot.+exceed.+100"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = 1.1)
    ),
    "`pageSize`.+must.+be.+a.+scalar.+positive.+integer"
  )

  # Expect error when `pageSize` * `page` is greater than 10,000
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = 1, page = 10001)
    ),
    "Offset-based.+paging.+can.+be.+used.+to.+retrieve.+up.+to.+10,000"
  )
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(pageSize = 21, page = 500)
    ),
    "Offset-based.+paging.+can.+be.+used.+to.+retrieve.+up.+to.+10,000"
  )

  # Expect error when cursor- and offset-based paging are used
  expect_error(
    oag_query(
      entity = "datasources",
      options = oag_options(cursor = TRUE, page = 1)
    ),
    "Offset-.+and.+cursor-based.+paging.+cannot.+be.+used.+simultaneously."
  )
})

## Success ----

test_that("oag_query() returns is successful when expected", {
  # None of the valid entities should thrown an error
  expect_no_error(lapply(oag_entities(), \(x) oag_query(entity = x)))

  # None of the valid entities should thrown an error (for all valid values of
  # `includeStats`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(includeStats = FALSE))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(includeStats = NULL))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(includeStats = TRUE))
  }))

  # None of the valid entities should thrown an error (for all valid values of
  # `cursor`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(cursor = FALSE))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(cursor = NULL))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(cursor = TRUE))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(
      entity = x,
      options = oag_options(cursor = FALSE, is_cursor_next = NULL)
    )
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(
      entity = x,
      options = oag_options(cursor = NULL, is_cursor_next = NULL)
    )
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(
      entity = x,
      options = oag_options(cursor = TRUE, is_cursor_next = NULL)
    )
  }))

  # None of the valid entities should thrown an error (for all valid values of
  # `is_cursor_next`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(
      entity = x,
      options = oag_options(cursor = "a", is_cursor_next = TRUE)
    )
  }))

  # None of the valid entities should thrown an error (for all valid values of
  # `page`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(page = 1))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(page = 1.0))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(page = 50))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(page = 100))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(page = 1000))
  }))

  # None of the valid entities should thrown an error (for all valid values of
  # `pageSize`).
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(pageSize = 1))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(pageSize = 1.0))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(pageSize = 50))
  }))
  expect_no_error(lapply(oag_entities(), \(x) {
    oag_query(entity = x, options = oag_options(page = 100))
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
            oag_query(entity = ent, options = oag_options(sortBy = sf))
            sf[1] <- c("DESC")
            oag_query(entity = ent, options = oag_options(sortBy = sf))
          }
        )
      }
    )
  )

  # Test a lot of possible combinations when using offset-based paging
  expect_no_error(
    offset_based <- lapply(
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
                        oag_query(ent, options = oag_options(sf, is, ps, pg))
                        sf[1] <- c("DESC")
                        oag_query(ent, options = oag_options(sf, is, ps, pg))
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
    cursor_based <- lapply(
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
                        oag_query(
                          ent,
                          options = oag_options(sf, is, ps, cursor = cs)
                        )
                        sf[1] <- c("DESC")
                        oag_query(
                          ent,
                          options = oag_options(sf, is, ps, cursor = cs)
                        )
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

  # Check that no query end with a separator used to concatenate the different
  # parts when building the query
  expect_all_false(grepl("\\?$", unlist(cursor_based)))
  expect_all_false(grepl("\\?$", unlist(offset_based)))
  expect_all_false(grepl("&$", unlist(cursor_based)))
  expect_all_false(grepl("&$", unlist(offset_based)))
})

# Tests for filters in `oag_query()` -------------------------------------------

## Errors ----

test_that("oag_query() returns an error for incorrectly formatted options", {
  # Filters are not named
  expect_error(
    oag_query("datasources", list()),
    "All.+filters.+must.+be.+named.+following.+the.+scheme.+`field.+=.+value`"
  )
  expect_error(
    oag_query("datasources", NULL),
    "All.+filters.+must.+be.+named.+following.+the.+scheme.+`field.+=.+value`"
  )
  expect_error(
    oag_query("datasources", 1),
    "All.+filters.+must.+be.+named.+following.+the.+scheme.+`field.+=.+value`"
  )
  expect_error(
    oag_query("datasources", c()),
    "All.+filters.+must.+be.+named.+following.+the.+scheme.+`field.+=.+value`"
  )
  expect_error(
    oag_query("datasources", TRUE),
    "All.+filters.+must.+be.+named.+following.+the.+scheme.+`field.+=.+value`"
  )
  expect_error(
    oag_query("datasources", NA),
    "All.+filters.+must.+be.+named.+following.+the.+scheme.+`field.+=.+value`"
  )

  # Filters are not named, but not bare strings
  expect_error(
    oag_query("datasources", a = list()),
    "Values.+assigned.+to.+filters.+must.+be.+a.+string"
  )
  expect_error(
    oag_query("datasources", a = NULL),
    "Values.+assigned.+to.+filters.+must.+be.+a.+string"
  )
  expect_error(
    oag_query("datasources", a = 1),
    "Values.+assigned.+to.+filters.+must.+be.+a.+string"
  )
  expect_error(
    oag_query("datasources", a = c()),
    "Values.+assigned.+to.+filters.+must.+be.+a.+string"
  )
  expect_error(
    oag_query("datasources", a = TRUE),
    "Values.+assigned.+to.+filters.+must.+be.+a.+string"
  )
  expect_error(
    oag_query("datasources", a = NA),
    "Values.+assigned.+to.+filters.+must.+be.+a.+string"
  )
})

## Success ----

test_that("oag_query() is successful when filters are named argument or `oag_st` objects", {
  # Valid entity with valid filter format (even though ultimately the field
  # would be invalidated by the API).
  expect_no_error(oag_query("datasources", a = "a"))

  expect_no_error(oag_query("datasources", a = concat_and("a", "b")))
  expect_no_error(oag_query(
    "datasources",
    a = concat_and(concat_or("a", "b"), concat_or("c", "d"))
  ))
  expect_no_error(
    oag_query(
      "datasources",
      a = concat_and(concat_or("a", "b"), concat_or("c", "d")),
      b = concat_not("e")
    )
  )
})

# Tests for filters in `oag_fetch()` -------------------------------------------

## Errors ----

test_that("oag_fetch() is successful when query is correctly formatted", {
  # `entity` arg is missing
  expect_error(oag_fetch(), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(1), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(c()), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(list()), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(list("a")), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(TRUE), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(NULL), "`entity`.+must.+be.+a.+character.+vector")
  expect_error(oag_fetch(NA), "`entity`.+must.+be.+a.+character.+vector")

  # Value passed to `entity` arg is not a valid entity
  expect_error(oag_fetch(entity = "aaa"), "`entity`.+must.+be.+one.+of.+")
  expect_error(oag_fetch(entity = c("a", "b"), "`entity`.+must.+be.+one.+of.+"))
  # `entity` is mispelled but partially match
  expect_error(
    oag_fetch(entity = "datasource"),
    "`entity`.+must.+be.+one.+of.+"
  )

  expect_error(oag_fetch("invalid_string"))
  expect_error(
    oag_fetch(
      "research-products",
      oag_test = "oag_test",
      options = oag_options(page = 1)
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "research-products",
      oag_test = "oag_test",
      options = oag_options(page = 1),
      token = oag_api_token(refresh_token = "", token = "")
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "organizations",
      oag_test = "oag_test",
      options = oag_options(page = 1)
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "organizations",
      oag_test = "oag_test",
      options = oag_options(page = 1),
      token = oag_api_token(refresh_token = "", token = "")
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "datasources",
      oag_test = "oag_test",
      options = oag_options(page = 1)
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "datasources",
      oag_test = "oag_test",
      options = oag_options(page = 1),
      token = oag_api_token(refresh_token = "", token = "")
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "projects",
      oag_test = "oag_test",
      options = oag_options(page = 1)
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "projects",
      oag_test = "oag_test",
      options = oag_options(page = 1),
      token = oag_api_token(refresh_token = "", token = "")
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "persons",
      oag_test = "oag_test",
      options = oag_options(page = 1)
    ),
    "Unknown.+parameter.+oag_test"
  )
  expect_error(
    oag_fetch(
      "persons",
      oag_test = "oag_test",
      options = oag_options(page = 1),
      token = oag_api_token(refresh_token = "", token = "")
    ),
    "Unknown.+parameter.+oag_test"
  )
})

## Success ----

test_that("oag_fetch() is successful when query is correctly formatted", {
  options <- oag_options(
    sortBy = c(relevance = "ASC"),
    page = 1,
    pageSize = 1
  )

  expect_gt(
    length(
      expect_no_error(
        oag_fetch("research-products", type = "publication", options = options)
      )
    ),
    0
  )
  expect_gt(
    length(
      expect_no_error(
        oag_request(
          oag_query(
            "research-products",
            type = "publication",
            options = options
          )
        )
      )
    ),
    0
  )

  expect_gt(
    length(
      expect_no_error(
        oag_fetch("organizations", countryCode = "CH", options = options)
      )
    ),
    0
  )
  expect_gt(
    length(
      expect_no_error(
        oag_request(
          oag_query("organizations", countryCode = "CH", options = options)
        )
      )
    ),
    0
  )

  expect_gt(
    length(
      expect_no_error(
        oag_fetch("datasources", thematic = "true", options = options)
      )
    ),
    0
  )
  expect_gt(
    length(
      expect_no_error(
        oag_request(
          oag_query("datasources", thematic = "true", options = options)
        )
      )
    ),
    0
  )

  expect_gt(
    length(
      expect_no_error(
        oag_fetch("projects", fundingShortName = "SNSF", options = options)
      )
    ),
    0
  )
  expect_gt(
    length(
      expect_no_error(
        oag_request(
          oag_query("projects", fundingShortName = "SNSF", options = options)
        )
      )
    ),
    0
  )

  expect_gt(
    length(
      expect_no_error(
        oag_fetch(
          "persons",
          originalId = "0000-0001-7462-0446",
          options = options
        )
      )
    ),
    0
  )
  expect_gt(
    length(
      expect_no_error(
        oag_request(
          oag_query(
            "persons",
            originalId = "0000-0001-7462-0446",
            options = options
          )
        )
      )
    ),
    0
  )

  options[["cursor"]] <- TRUE
  options[["page"]] <- NULL
  options[["pageSize"]] <- 100

  # Cursor-based paging
  expect_gt(
    length(
      expect_no_error(
        oag_fetch(
          "research-products",
          type = "publication",
          relProjectFundingShortName = "SNSF",
          fromPublicationDate = "2020-12-15",
          toPublicationDate = "2020-12-31",
          options = options
        )
      )
    ),
    1
  )

  options[["cursor"]] <- NULL
  # Offset-based paging
  expect_gt(
    length(
      expect_no_error(
        oag_fetch(
          "research-products",
          type = "publication",
          relProjectFundingShortName = "SNSF",
          fromPublicationDate = "2020-12-15",
          toPublicationDate = "2020-12-31",
          options = options
        )
      )
    ),
    1
  )
})

# Tests for URL format in `oag_request()` --------------------------------------

## Errors ----
test_that("oag_request() throws an error when `url` is not a string or an `oag_query`", {
  # `url` is neither a string or an `oag_query` object
  expect_error(
    oag_request(1),
    "The.+object.+passed.+to.+`url`.+must.+be.+string.+of.+class.+`oag_query`"
  )
  expect_error(
    oag_request(c()),
    "The.+object.+passed.+to.+`url`.+must.+be.+string.+of.+class.+`oag_query`"
  )
  expect_error(
    oag_request(NA),
    "The.+object.+passed.+to.+`url`.+must.+be.+string.+of.+class.+`oag_query`"
  )
  expect_error(
    oag_request(NULL),
    "The.+object.+passed.+to.+`url`.+must.+be.+string.+of.+class.+`oag_query`"
  )
  expect_error(
    oag_request(list()),
    "The.+object.+passed.+to.+`url`.+must.+be.+string.+of.+class.+`oag_query`"
  )
  expect_error(
    oag_request(TRUE),
    "The.+object.+passed.+to.+`url`.+must.+be.+string.+of.+class.+`oag_query`"
  )
  expect_error(
    oag_request(letters),
    "The.+object.+passed.+to.+`url`.+must.+be.+string.+of.+class.+`oag_query`"
  )

  # `url` is a string but not a proper one
  expect_error(oag_request("aaa"), "Failed.+to.+parse.+URL")
})
