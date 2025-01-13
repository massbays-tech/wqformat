test_that("find_var_names works", {
  df <- data.frame(
    "col1" = c("A", "B|C", "D|E", "F", "G", NA),
    "col2" = c("X|Y", "Z", "T|U", NA, "V", "W"),
    "col3" = c(1, NA, 2, 3, 5, 6))

  # Check errors
  expect_error(
    find_var_names("foo", "col1", "col2"),
    regexp = "df must be type dataframe")
  expect_error(
    find_var_names(df, "col4", "col1"),
    regexp = "Invalid in_format. Acceptable formats: col1, col2, col3")
  expect_error(
    find_var_names(df, "col1", "col4"),
    regexp = "Invalid out_format. Acceptable formats: col1, col2, col3")
  expect_error(
    find_var_names(df, "col4", "col5"),
    regexp = "Invalid in_format and out_format. Acceptable formats: col1, col2, col3")

  # Check works
  var_names <- find_var_names(df, "col1", "col2")
  expect_equal(var_names$old_names, c("A", "B", "C", "D", "E", "G"))
  expect_equal(var_names$new_names, c("X", "Z", "Z", "T", "T", "V"))
  expect_equal(var_names$keep_var, c("X", "Z", "T", "V", "W"))

  var_num <- find_var_names(df, "col1", "col3")
  expect_equal(var_num$old_names, c("A", "D", "E", "F", "G"))
  expect_equal(var_num$new_names, c('1', '2', '2', '3', '5'))
  expect_equal(var_num$keep_var, c('1', '2', '3', '5', '6'))

  var_mult <- find_var_names(df, "col1", "col2", multiple_out_var = TRUE)
  expect_equal(var_mult$old_names, c("A", "A", "B", "C", "D", "E", "D", "E", "G"))
  expect_equal(var_mult$new_names, c("X", "Y", "Z", "Z", "T", "T", "U", "U", "V"))
  expect_equal(var_mult$keep_var, c("X", "Y", "Z", "T", "U", "V", "W"))
})
