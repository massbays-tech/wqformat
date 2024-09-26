test_that("rename_var works", {
  expect_equal(
    rename_var("cat", c("cat","dog"), c("kitten", "puppy")),
    "kitten")
  expect_equal(rename_var(1, c(1,2), c(11, 22)), 11)
})

test_that("rename_all_var works", {
  df <- data.frame("species" = c("aardvark", "bittern", "chinchilla"),
                   "class" = c("mammal", "bird", "mammal"),
                   "numbers" = c(1, 2, 3))
  expect_error(
    rename_all_var(df, "genus", "chinchilla", "cat"),
    regexp = "col_name not in dataframe")
  expect_error(
    rename_all_var(df, "species", c("ant", "chinchilla"), "cat"),
    regexp = "old_varname and new_varname are different lengths")

  df_test <- rename_all_var(df, "class",
                            c("mammal", "reptile"),
                            c("Mammalia", "Reptilia"))
  expect_equal(df_test$class, c("Mammalia", "bird", "Mammalia"))

  df_test <- rename_all_var(df, "numbers", c(1, 2), c(12, 23))
  expect_equal(df_test$numbers, c(12, 23, 3))
})
