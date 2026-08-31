# Tests for `concat_and()` ------------------------------------------------

## Errors ----

test_that("concat_and() returns an error for incorrectly formatted options", {
  # Not enough arguments to concatenate
  expect_error(concat_and("a"), "At.+least.+two.+strings")
  expect_error(concat_or("a"), "At.+least.+two.+strings")
  # Too many arguments for NOT
  expect_error(concat_not("a", "b"), "No.+more.+that.+1.+argument")

  # Arguments are not string (AND)
  expect_error(
    concat_and("1", 1),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_and("1", TRUE),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_and("1", letters),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_and(concat_and("a", "b"), letters),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_and("1", NULL),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_and("1", NA),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_and(list("a", "b")),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )

  # Arguments are not string (OR)
  expect_error(
    concat_or("1", 1),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_or("1", TRUE),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_or("1", letters),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_or(concat_and("a", "b"), letters),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_or("1", NULL),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_or("1", NA),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )
  expect_error(
    concat_or(list("a", "b")),
    "Arguments.+must.+be.+strings.+or.+of.+class.+`oag_str`"
  )

  # Arguments are not string (NOT)
  expect_error(concat_not(1), "must.+be.+a.+bare.+string")
  expect_error(concat_not(TRUE), "must.+be.+a.+bare.+string")
  expect_error(concat_not(letters), "must.+be.+a.+bare.+string")
  expect_error(concat_not(NULL), "must.+be.+a.+bare.+string")
  expect_error(concat_not(NA), "must.+be.+a.+bare.+string")
  expect_error(concat_not(list("a", "b")), "must.+be.+a.+bare.+string")
})

## Errors ----

test_that("concat_and() returns a concatenated string without error", {
  # `concat_and()` when only bare characters, a single character vector, or a
  # combination of characters and `oag_str`.
  expect_s3_class(expect_no_error(concat_and(letters)), "oag_str")
  expect_s3_class(expect_no_error(concat_and("a", "b", "c")), "oag_str")
  expect_s3_class(
    expect_no_error(concat_and(concat_or("a", "b", "c"), "d", "e")),
    "oag_str"
  )
  expect_s3_class(
    expect_no_error(concat_and(concat_not("a"), "d", "e")),
    "oag_str"
  )

  # `concat_or()` when only bare characters, a single character vector, or a
  # combination of characters and `oag_str`.
  expect_s3_class(expect_no_error(concat_or(letters)), "oag_str")
  expect_s3_class(expect_no_error(concat_or("a", "b", "c")), "oag_str")
  expect_s3_class(
    expect_no_error(concat_or(concat_and("a", "b", "c"), "d", "e")),
    "oag_str"
  )
  expect_s3_class(
    expect_no_error(concat_or(concat_not("a"), "d", "e")),
    "oag_str"
  )

  expect_s3_class(expect_no_error(concat_not("a")), "oag_str")
  expect_s3_class(expect_no_error(concat_not(concat_or(letters))), "oag_str")
})
