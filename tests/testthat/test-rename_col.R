test_that("rename_col works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  expect_error(
    rename_col(df, c("species", "class"), "foo"),
    regexp = "old_colnames and new_colnames are different lengths"
  )

  # Check 1:1 column name conversion
  df_simple <- data.frame(
    "foo" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "foofy" = c(1, 2, 3, 4)
  )
  expect_equal(
    rename_col(df, c("species", "genus", "numbers"), c("foo", "bar", "foofy")),
    df_simple
  )

  # Check 2:1 and 1:2 column name conversions
  df_complex <- data.frame(
    "foo" = c("aardvark", "bittern", "mammal", NA),
    "bar" = c(1, 2, 3, 4),
    "foofy" = c(1, 2, 3, 4)
  )

  expect_equal(
    rename_col(
      df,
      c("species", "class", "numbers", "numbers"),
      c("foo", "foo", "bar", "foofy")
    ),
    df_complex
  )
})
