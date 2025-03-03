test_that("find_var_names works", {
  df <- data.frame(
    "col1" = c("01||A", "03||B|C", "D|E", "F", "02||G", NA),
    "col2" = c("X|Y", "Z", "T|U", NA, "V", "W"),
    "col3" = c(1, NA, 2, 3, 5, 6)
  )

  # Check errors
  expect_error(
    find_var_names("foo", "col1", "col2"),
    regexp = "df must be type dataframe"
  )
  expect_error(
    find_var_names(df, "col4", "col1"),
    regexp = "Invalid in_format. Acceptable formats: col1, col2, col3"
  )
  expect_error(
    find_var_names(df, "col1", "col4"),
    regexp = "Invalid out_format. Acceptable formats: col1, col2, col3"
  )
  expect_error(
    find_var_names(df, "col4", "col5"),
    regexp =
      "Invalid in_format and out_format. Acceptable formats: col1, col2, col3"
  )

  # Check works
  var_names <- find_var_names(df, "col1", "col2")
  expect_equal(var_names$old_names, c("D", "E", "G", "A", "B", "C"))
  expect_equal(var_names$new_names, c("T", "T", "V", "X", "Z", "Z"))
  expect_equal(var_names$keep_var, c("T", "V", "W", "X", "Z"))

  var_order <- find_var_names(df, "col2", "col1")
  expect_equal(var_order$old_names, c("X", "Y", "V", "Z", "T", "U"))
  expect_equal(var_order$new_names, c("A", "A", "G", "B", "D", "D"))
  expect_equal(var_order$keep_var, c("A", "G", "B", "D", "F"))

  var_num <- find_var_names(df, "col1", "col3")
  expect_equal(var_num$old_names, c("A", "D", "E", "F", "G"))
  expect_equal(var_num$new_names, c("1", "2", "2", "3", "5"))
  expect_equal(var_num$keep_var, c("1", "2", "3", "5", "6"))

  var_equal <- find_var_names(df, "col3", "col3")
  expect_equal(var_equal$old_names, NA)
  expect_equal(var_equal$new_names, NA)
  expect_equal(var_equal$keep_var, c("1", "2", "3", "5", "6"))
})
