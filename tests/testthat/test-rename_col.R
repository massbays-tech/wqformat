test_that("rename_col works", {
  df <- data.frame("species" = c("aardvark", "bittern", "chinchilla"),
                   "class" = c("mammal", "bird", "mammal"),
                   "numbers" = c(1, 2, 3))

  expect_error(
    rename_col(df, c("species", "class"), "foo"),
    regexp = "old_colnames and new_colnames are different lengths")

  df_test <- rename_col(df,
    c("species", "genus", "numbers"),
    c("foo", "bar", "foofy"))
  expect_equal(colnames(df_test), c("foo", "class", "foofy"))
})
