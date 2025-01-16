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
  df_simple <- data.frame(
    "foo" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "foofy" = c(1, 2, 3, 4)
  )
  expect_equal(
    rename_col(dat, c("species", "genus", "numbers"), c("foo", "bar", "foofy")),
    df_simple
  )

  # Check overlapping 3:1 and 1:3 column name conversions
  df_complex <- data.frame(
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
    df_complex
  )
})

test_that("concat_columns works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  df2 <- data.frame(
    "foo" = c('aardvark', 'bittern', 'mammal', NA),
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  df3 <- data.frame(
    "numbers" = c('aardvark', 'bittern', 'mammal', 4),
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA)
  )

  expect_equal(concat_columns(df, c('species', 'class'), 'foo'), df2)
  expect_equal(
    concat_columns(df, c('species', 'class', 'numbers'), 'numbers'),
    df3
  )
})
