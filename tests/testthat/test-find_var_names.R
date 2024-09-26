test_that("find_var_names works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "genus" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, NA, 2))

  # Check errors
  expect_error(
    find_var_names("foo", "numbers", "species"),
    regexp = "df must be type dataframe")
  expect_error(
    find_var_names(df, "class", "species"),
    regexp = "Invalid in_format. Acceptable formats: species, genus, numbers")
  expect_error(
    find_var_names(df, "species", "class"),
    regexp = "Invalid out_format. Acceptable formats: species, genus, numbers")
  expect_error(
    find_var_names(df, "class", "order"),
    regexp = "Invalid in_format and out_format. Acceptable formats: species, genus, numbers")

  # Check works
  var_names <- find_var_names(df, "numbers", "species")
  expect_equal(var_names$old_names, c("1", "2"))
  expect_equal(var_names$new_names, c("aardvark", "chinchilla"))
  expect_equal(var_names$keep_var, c("aardvark", "bittern", "chinchilla"))
})
