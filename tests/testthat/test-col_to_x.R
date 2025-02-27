test_that("col_to_numeric works", {
  df <- data.frame(
    "col1" = c("1", "2", "4"),
    "col2" = c("A", "5", "6")
  )

  df2 <- data.frame(
    "col1" = c(1, 2, 4),
    "col2" = c("A", "5", "6")
  )

  expect_equal(col_to_numeric(df, "col1"), df2)
  expect_equal(col_to_numeric(df, "col2"), df)
})

test_that("col_to_date works", {
  df <- data.frame(
    "good_date" = c("2022-03-01", "2023-04-18", "2004-06-12", "2024-09-26"),
    "na_date" = c("2022-03-01", NA, NA, "2024-09-26"),
    "mdy_date" = c("3/1/22", NA, "4/18/23", NA),
    "bad_date" = c("3/1/2022", "2023/6/4", "18/4/2023", NA)
  )
  df[["good_date"]] <- as.Date(df[["good_date"]])
  df[["na_date"]] <- as.Date(df[["na_date"]])

  df_format <- data.frame(
    "good_date" = c("2022-03-01", "2023-04-18", "2004-06-12", "2024-09-26"),
    "na_date" = c("2022-03-01", NA, NA, "2024-09-26"),
    "mdy_date" = c("2022-03-01", NA, "2023-04-18", NA),
    "bad_date" = c("3/1/2022", "2023/6/4", "18/4/2023", NA)
  )
  df_format[["good_date"]] <- as.Date(df_format[["good_date"]])
  df_format[["na_date"]] <- as.Date(df_format[["na_date"]])
  df_format[["mdy_date"]] <- as.Date(df_format[["mdy_date"]])

  # Check error messages
  expect_error(
    col_to_date(df, "mdy_date", "foobar"),
    regexp = "date_format contains invalid variables: foo")
  expect_error(
    col_to_date(df, "mdy_date", "m/d/Y"),
    regexp = 'Date does not match format "m/d/Y"')
  expect_error(
    col_to_date(df, "bad_date", "m/d/Y"),
    regexp = "Date is improperly formatted in rows: 2, 3")

  # Check works
  expect_equal(col_to_date(df, "good_date"), df)
  expect_equal(col_to_date(df, "na_date"), df)
  expect_equal(col_to_date(df, "mdy_date", "m/d/y"), df_format)
})

test_that("col_to_state works", {
  df <- data.frame(
    "State_abb" = c("RI", "MA"),
    "State_name" = c("Rhode Island", "Massachusetts"),
    "State_mix" = c("Rhode Island", "MA"),
    "State_misspell" = c("RI", "foo")
  )

  df <- col_to_state(df, "State_abb", full_name = TRUE)
  df <- col_to_state(df, "State_name")
  df <- col_to_state(df, "State_mix", full_name = TRUE)
  df <- col_to_state(df, "State_misspell")

  df2 <- data.frame(
    "State_abb" = c("Rhode Island", "Massachusetts"),
    "State_name" = c("RI", "MA"),
    "State_mix" = c("Rhode Island", "Massachusetts"),
    "State_misspell" = c("RI", "foo")
  )

  expect_equal(df, df2)
})
