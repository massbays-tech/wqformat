# test update_var -----
test_that("update_var works", {
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
    update_var(
      df, "species",
      c("aardvark", "bittern"),
      c("aardvark", "bittern")
    ),
    df
  )
  expect_equal(
    update_var(
      df, "class",
      c("mammal", "reptile"),
      c("Mammalia", "Reptilia")
    ),
    df2
  )
  expect_equal(
    update_var(df, "numbers", c(1, 2), c(12, 23)),
    df3
  )
})

test_that("update_var handles NA values appropriately", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, 2, 3)
  )

  expect_equal(
    update_var(df, "species", NA, NA),
    df
  )
  expect_error(
    update_var(df, "class", c("mammal", "bird"), c(NA, "Aves")),
    regexp = "old_varname and new_varname must not contain NA values"
  )
})

test_that("update_var sends error messages", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, 2, 3)
  )

  expect_error(
    update_var(df, "genus", "chinchilla", "cat"),
    regexp = "col_name not in dataframe"
  )
  expect_error(
    update_var(df, "species", c("ant", "chinchilla"), "cat"),
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

# Test state_to_abb() ----
test_that("state_to_abb works", {
  df_in <- data.frame(
    "State_name" = c("Rhode Island", "Massachusetts"),
    "State_mix" = c("Rhode Island", "MA")
  )

  expect_equal(
    df_in %>%
      state_to_abb("State_name") %>%
      state_to_abb("State_mix"),
    data.frame(
      "State_name" = c("RI", "MA"),
      "State_mix" = c("RI", "MA")
    )
  )
})

test_that("state_to_abb error messages", {
  df_test <- data.frame(
    "State_error" = c("Rhode Island", "foo")
  )

  expect_warning(
    state_to_abb(df_test, "State_error"),
    regexp = "foo is not a valid state name"
  )
})

# Test abb_to_state() ----
test_that("abb_to_state works", {
  df_in <- data.frame(
    "State_abb" = c("RI", "MA"),
    "State_mix" = c("Rhode Island", "MA")
  )

  expect_equal(
    df_in %>%
      abb_to_state("State_abb") %>%
      abb_to_state("State_mix"),
    data.frame(
      "State_abb" = c("Rhode Island", "Massachusetts"),
      "State_mix" = c("Rhode Island", "Massachusetts")
    )
  )
})

test_that("abb_to_state error messages", {
  dat <- data.frame(
    "State_error" = c("Rhode Island", "foo")
  )

  expect_warning(
    abb_to_state(dat, "State_error"),
    regexp = "foo is not a valid state name"
  )
})
