# test rename_var -----
test_that("rename_var works", {
  expect_equal(
    rename_var("cat", c("cat", "dog"), c("kitten", "puppy")),
    "kitten"
  )
  expect_equal(
    rename_var(1, c(1, 2), c(11, 22)),
    11
  )

  # Test multiple out values
  in_list <- c("cat", "dog", "cat")
  out_list <- c("kitten", "puppy", "kitty")

  expect_equal(
    rename_var("cat", in_list, out_list),
    "kitten"
  )
  expect_equal(
    rename_var("cat", in_list, out_list, allow_multiple = TRUE),
    c("kitten", "kitty")
  )
})

test_that("rename_var handles NA values appropriately", {
  expect_error(
    rename_var("dog", c(NA, "dog"), c("kitten", "puppy")),
    regexp = "old_varname and new_varname must not contain NA values"
  )
  expect_equal(
    rename_var(NA, c("cat", "dog"), c("kitten", "puppy")),
    NA
  )
  expect_error(
    rename_var(NA, c(NA, "dog"), c("kitten", "puppy")),
    regexp = "old_varname and new_varname must not contain NA values"
  )
})

test_that("rename_var sends error messages", {
  expect_error(
    rename_var("dog", c("bird", "cat", "dog"), c("kitten", "puppy")),
    regexp = "old_varname and new_varname must be the same length"
  )
})

# test rename_all_var -----
test_that("rename_all_var works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, 2, 3)
  )

  # Test works
  df2 <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("Mammalia", "bird", "Mammalia"),
    "numbers" = c(1, 2, 3)
  )
  df3 <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(12, 23, 3)
  )

  expect_equal(
    rename_all_var(
      df, "species",
      c("aardvark", "bittern"),
      c("aardvark", "bittern")
    ),
    df
  )
  expect_equal(
    rename_all_var(
      df, "class",
      c("mammal", "reptile"),
      c("Mammalia", "Reptilia")
    ),
    df2
  )
  expect_equal(
    rename_all_var(df, "numbers", c(1, 2), c(12, 23)),
    df3
  )
})

test_that("rename_all_var handles NA values appropriately", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, 2, 3)
  )

  expect_equal(
    rename_all_var(df, "species", NA, NA),
    df
  )
  expect_error(
    rename_all_var(df, "class", c("mammal", "bird"), c(NA, "Aves")),
    regexp = "old_varname and new_varname must not contain NA values"
  )
})

test_that("rename_all_var sends error messages", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, 2, 3)
  )

  expect_error(
    rename_all_var(df, "genus", "chinchilla", "cat"),
    regexp = "col_name not in dataframe"
  )
  expect_error(
    rename_all_var(df, "species", c("ant", "chinchilla"), "cat"),
    regexp = "old_varname and new_varname are different lengths"
  )
})

# test rename_var -----
test_that("warn_invalid_var works", {
  df <- data.frame(
    "col1" = c("A", "B", "C"),
    "col2" = c("D", "E", "F")
  )
  varlist <- c("A", "B", "C", "D")

  expect_silent(
    warn_invalid_var(df, "col1", varlist)
  )
  expect_warning(
    warn_invalid_var(df, "col2", varlist),
    regexp = "Invalid variables in col2: E, F"
  )
})

test_that("warn_invalid_var handles NA values appropriately", {
  df <- data.frame(
    "col1" = c(NA, "B", "C"),
    "col2" = c("D", NA, "F")
  )
  varlist <- c("A", "B", "C", "D")

  expect_silent(
    warn_invalid_var(df, "col1", varlist)
  )
  expect_warning(
    warn_invalid_var(df, "col2", varlist),
    regexp = "Invalid variables in col2: F"
  )
})

test_that("rename_var sends error messages", {
  df <- data.frame(
    "col1" = c(NA, "B", "C"),
    "col2" = c("D", NA, "F")
  )
  varlist <- c("A", "B", "C", "D")

  expect_error(
    warn_invalid_var(df, "col3", varlist),
    regexp = "col_name not in dataframe"
  )
})
