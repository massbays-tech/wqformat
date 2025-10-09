test_that("fetch_var works", {
  df <- data.frame(
    col1 = c("01||A", "03||B|C", "D|E", "F", "02||G", NA),
    col2 = c("X|Y", "Z", "T|U", NA, "V", "W"),
    col3 = c(1, NA, 2, 3, 5, 6),
    col4 = c("foo bar", NA, "foo (bar)", "foo_bar", NA, NA)
  )

  # Multiple out formats
  expect_equal(
    fetch_var(df, "col1", "col2"),
    list(
      old_names = c("D", "E", "G", "A", "B", "C"),
      new_names = c("T", "T", "V", "X", "Z", "Z"),
      keep_var = c("T", "V", "W", "X", "Z")
    )
  )
  # Multiple out formats, numbered list
  expect_equal(
    fetch_var(df, "col2", "col1"),
    list(
      old_names = c("X", "Y", "V", "Z", "T", "U"),
      new_names = c("A", "A", "G", "B", "D", "D"),
      keep_var = c("A", "G", "B", "D", "F")
    )
  )
  # Single out format, NA values
  expect_equal(
    fetch_var(df, "col1", "col3"),
    list(
      old_names = c("A", "D", "E", "F", "G"),
      new_names = c("1", "2", "2", "3", "5"),
      keep_var = c("1", "2", "3", "5", "6")
    )
  )
  # name_repair is FALSE (default)
  expect_equal(
    fetch_var(df, "col4", "col3"),
    list(
      old_names = c("foo bar", "foo (bar)", "foo_bar"),
      new_names = c("1", "2", "3"),
      keep_var = c("1", "2", "3", "5", "6")
    )
  )
  # name_repair is TRUE
  expect_equal(
    fetch_var(df, "col4", "col3", name_repair = TRUE),
    list(
      old_names = c("foo.bar", "foo..bar.", "foo_bar"),
      new_names = c("1", "2", "3"),
      keep_var = c("1", "2", "3", "5", "6")
    )
  )
  # Convert same to same
  expect_equal(
    fetch_var(df, "col3", "col3"),
    list(
      old_names = NA,
      new_names = NA,
      keep_var = c("1", "2", "3", "5", "6")
    )
  )
  # Convert same to same, multiple in/out formats
  expect_equal(
    fetch_var(df, "col2", "col2"),
    list(
      old_names = c("U", "Y"),
      new_names = c("T", "X"),
      keep_var = c("T", "V", "W", "X", "Z")
    )
  )
  # Convert same to same, name_repair is TRUE
  expect_equal(
    fetch_var(df, "col4", "col4", name_repair = TRUE),
    list(
      old_names = c("foo..bar.", "foo.bar"),
      new_names = c("foo (bar)", "foo bar"),
      keep_var = c("foo (bar)", "foo bar", "foo_bar")
    )
  )
})

test_that("fetch_var error messages", {
  df <- data.frame(
    col1 = c(1, NA, 2, 3, 5, 6),
    col2 = NA
  )

  # Check errors ----
  expect_error(
    fetch_var("foo", "col1", "col2"),
    regexp = "in_table must be a dataframe"
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
