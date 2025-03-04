# Test rename_col ----
test_that("rename_col works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  # Check auto-return
  expect_equal(
    rename_col(df, NA, NA),
    df
  )
  expect_equal(
    rename_col(df, c("species", "genus"), c("species", "genus")),
    df
  )
  expect_equal(
    rename_col(df, c("cat", "dog"), c("kitten", "puppy")),
    df
  )

  # Check 1:1 column name conversion
  expect_equal(
    rename_col(df, c("species", "genus", "numbers"), c("foo", "bar", "foofy")),
    data.frame(
      "foo" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "foofy" = c(1, 2, 3, 4)
    )
  )

  # Check pipes
  expect_equal(
    df %>% rename_col(
      c("species", "genus", "numbers"),
      c("foo", "bar", "foofy")
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "foofy" = c(1, 2, 3, 4)
    )
  )

  # Check 2:1 column name conversion
  expect_equal(
    rename_col(
      df,
      c("species", "class"),
      c("foo", "foo")
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", "mammal", NA),
      "numbers" = c(1, 2, 3, 4)
    )
  )

  # Check overlapping 3:1 and 1:3 column name conversions
  expect_equal(
    rename_col(
      df,
      c("species", "class", "numbers", "numbers", "numbers"),
      c("foo", "foo", "foo", "bar", "foofy")
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", "mammal", 4),
      "bar" = c(1, 2, 3, 4),
      "foofy" = c(1, 2, 3, 4)
    )
  )
})

test_that("rename_col error messages", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  expect_error(
    rename_col(df, c("species", "class"), "foo"),
    regexp = "old_colnames and new_colnames are different lengths"
  )
  expect_error(
    rename_col(df, c("species", "class"), c(NA, "foo")),
    regexp = "Can not include NA values in old_colnames or new_colnames"
  )
})

# Check concat_Columns ----

test_that("concat_columns works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  # Test intended use
  expect_equal(
    concat_columns(
      df,
      c("species", "class"),
      "foo"
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", "mammal", NA),
      "species" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "numbers" = c(1, 2, 3, 4)
    )
  )
  expect_equal(
    concat_columns(
      df,
      c("species", "class", "numbers"),
      "numbers"
    ),
    data.frame(
      "numbers" = c("aardvark", "bittern", "mammal", 4),
      "species" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA)
    )
  )

  # Test edge cases, pipes
  expect_equal(
    df %>%
      concat_columns("species", "foo") %>% # only 1 in_colnames
      concat_columns("foofy", "owl"), # invalid in_colnames
    data.frame(
      "species" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "numbers" = c(1, 2, 3, 4),
      "foo" = c("aardvark", "bittern", NA, NA),
      "owl" = NA
    )
  )
})
