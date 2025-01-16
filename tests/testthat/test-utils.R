test_that("col_to_numeric works", {
  df <- data.frame(
    'col1' = c('1', '2', '4'),
    'col2' = c('A', '5', '6')
  )

  df2 <- data.frame(
    'col1' = c(1, 2, 4),
    'col2' = c('A', '5', '6')
  )

  expect_equal(col_to_numeric(df, 'col1'), df2)
  expect_equal(col_to_numeric(df, 'col2'), df)
})
