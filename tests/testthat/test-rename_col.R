test_that("rename_col works", {
  dat <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  expect_error(
    rename_col(dat, c("species", "class"), "foo"),
    regexp = "old_colnames and new_colnames are different lengths"
  )

  # Check 1:1 column name conversion
  dat_simple <- data.frame(
    "foo" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "foofy" = c(1, 2, 3, 4)
  )
  expect_equal(
    rename_col(dat, c("species", "genus", "numbers"), c("foo", "bar", "foofy")),
    dat_simple
  )

  # Check overlapping 3:1 and 1:3 column name conversions
  dat_complex <- data.frame(
    "foo" = c("aardvark", "bittern", "mammal", 4),
    "bar" = c(1, 2, 3, 4),
    "foofy" = c(1, 2, 3, 4)
  )

  expect_equal(
    rename_col(
      dat,
      c("species", "class", "numbers", "numbers", "numbers"),
      c("foo", "foo", "foo", "bar", "foofy")
    ),
    dat_complex
  )
})

test_that("concat_columns works", {
  dat <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  dat2 <- data.frame(
    "foo" = c('aardvark', 'bittern', 'mammal', NA),
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  dat3 <- data.frame(
    "numbers" = c('aardvark', 'bittern', 'mammal', 4),
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA)
  )

  # Test concatenate multiple columns
  expect_equal(concat_columns(dat, c('species', 'class'), 'foo'), dat2)
  expect_equal(
    concat_columns(dat, c('species', 'class', 'numbers'), 'numbers'),
    dat3
  )

  # Test for only one in column, invalid in column
  dat4 <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4),
    "foo" = c("aardvark", "bittern", NA, NA)
  )

  dat5 <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4),
    "bar" = c(NA, NA, NA, NA)
  )

  expect_equal(concat_columns(dat, "species", "foo"), dat4)
  expect_equal(concat_columns(dat, "foo", "bar"), dat5)
})
