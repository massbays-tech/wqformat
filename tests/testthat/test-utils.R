# Test unique_var ----
test_that("unique_var works", {
  df <- data.frame(
    col1 = c("X|Y", "Z", "X", NA, "Y|Z"),
    col2 = c("2", "1|4", NA, "4|5", "3"),
    col3 = NA
  )

  # Test basic
  expect_equal(
    unique_var(df, "col1"),
    c("X", "Y", "Z")
  )
  expect_equal(
    unique_var(df, "col2"),
    c("2", "1", "4", "5", "3")
  )
  expect_equal(
    unique_var(df, "col3"),
    NA
  )

  # Test limit_var
  expect_equal(
    unique_var(df, "col2", limit_var = TRUE),
    c("2", "1", "4", "3")
  )

  # Test error
  expect_error(
    unique_var(df, "col4"),
    regexp="col_name is invalid"
  )
})

# Test fetch_var ----
test_that("fetch_var works", {
  df <- data.frame(
    col1 = c("01||A", "03||B|C", "D|E", "F", "02||G", NA),
    col2 = c("X|Y", "Z", "T|U", NA, "V", "W"),
    col3 = c(NA, 2, NA, NA, 5, 6),
    col4 = c("foo bar", NA, "foo (bar)", "foo_bar", NA, NA)
  )

  # Basic tests
  expect_equal(
    fetch_var(df, "col1", "col2"),
    list(
      old_names = c("D", "E", "G", "A", "B", "C"),
      new_names = c("T", "T", "V", "X", "Z", "Z"),
      keep_var = c("T", "U", "V", "W", "X", "Y", "Z")
    )
  )
  expect_equal(
    fetch_var(df, "col2", "col1"),
    list(
      old_names = c("X", "Y", "V", "Z", "T", "U"),
      new_names = c("A", "A", "G", "B", "D", "D"),
      keep_var = c("A", "G", "B", "C", "D", "E", "F")
    )
  )
  expect_equal(
    fetch_var(df, "col1", "col3"),
    list(
      old_names = c("B", "C", "G"),
      new_names = c("2", "2", "5"),
      keep_var = c("2", "5", "6")
    )
  )
  # Test name_repair
  expect_equal(
    fetch_var(df, "col4", "col1"),
    list(
      old_names = c("foo bar", "foo (bar)", "foo_bar"),
      new_names = c("A", "D", "F"),
      keep_var = c("A", "G", "B", "C", "D", "E", "F")
    )
  )
  expect_equal(
    fetch_var(df, "col4", "col1", name_repair = TRUE),
    list(
      old_names = c("foo.bar", "foo..bar.", "foo_bar"),
      new_names = c("A", "D", "F"),
      keep_var = c("A", "G", "B", "C", "D", "E", "F")
    )
  )
  # Test limit_var
  expect_equal(
    fetch_var(df, "col1", "col2", limit_var = TRUE),
    list(
      old_names = c("D", "E", "G", "A", "B", "C"),
      new_names = c("T", "T", "V", "X", "Z", "Z"),
      keep_var = c("T", "V", "W", "X", "Z")
    )
  )
  expect_equal(
    fetch_var(df, "col2", "col1", limit_var = TRUE),
    list(
      old_names = c("X", "Y", "V", "Z", "T", "U"),
      new_names = c("A", "A", "G", "B", "D", "D"),
      keep_var = c("A", "G", "B", "D", "F")
    )
  )
  # Test edge case - same in_format, out_format
  expect_equal(
    fetch_var(df, "col3", "col3"),
    list(
      old_names = NA,
      new_names = NA,
      keep_var = c("2", "5", "6")
    )
  )
  expect_equal(
    fetch_var(df, "col2", "col2"),
    list(
      old_names = c("U", "Y"),
      new_names = c("T", "X"),
      keep_var = c("T", "U", "V", "W", "X", "Y", "Z")
    )
  )
  expect_equal(
    fetch_var(df, "col4", "col4", name_repair = TRUE),
    list(
      old_names = c("foo..bar.", "foo.bar"),
      new_names = c("foo (bar)", "foo bar"),
      keep_var = c("foo (bar)", "foo bar", "foo_bar")
    )
  )
  # Test edge case - no matches
  expect_equal(
    fetch_var(df, "col4", "col3"),
    list(
      old_names = NA,
      new_names = NA,
      keep_var = c("2", "5", "6")
    )
  )
})

test_that("fetch_var error messages", {
  df <- data.frame(
    col1 = c(1, NA, 2, 3, 5, 6),
    col2 = NA
  )

  expect_error(
    fetch_var(df, "col1", "col3"),
    regexp = "Invalid in_format or out_format"
  )
  expect_error(
    fetch_var(df, "col1", "col2"),
    regexp = "out_format is blank"
  )
})

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
    rename_var("cat", in_list, out_list, multiple = TRUE),
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

# Test str_unique ----
test_that("str_unique works", {
  expect_equal(
    str_unique("foo,bar, foo , bar, f o o"),
    "foo,bar,f o o"
  )
  expect_equal(
    str_unique("the most superb owl of all the owls", delim = " "),
    "the most superb owl of all owls"
  )
  expect_equal(
    str_unique("tweedle dee| tweedle dum |tweedle dee", delim = "|"),
    "tweedle dee|tweedle dum"
  )
})
