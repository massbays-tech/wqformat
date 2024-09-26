test_that("format_date works", {
  df <- data.frame(
    "good_date" = c(
      lubridate::ymd("20220301"),
      lubridate::ymd("20230418"),
      lubridate::ymd("20040612"),
      lubridate::ymd("20240926")),
    "na_date" = c(
      lubridate::ymd("20220301"),
      NA,
      NA,
      lubridate::ymd("20240926")),
    "mdy_date" = c("3/1/22", NA, "4/18/23", NA),
    "bad_date" = c("3/1/2022", "2023/6/4", "18/4/2023", NA))

  df_format <- data.frame(
    "good_date" = c(
      lubridate::ymd("20220301"),
      lubridate::ymd("20230418"),
      lubridate::ymd("20040612"),
      lubridate::ymd("20240926")),
    "na_date" = c(
      lubridate::ymd("20220301"),
      NA,
      NA,
      lubridate::ymd("20240926")),
    "mdy_date" = c(
      lubridate::ymd("20220301"),
      NA,
      lubridate::ymd("20230418"),
      NA),
    "bad_date" = c("3/1/2022", "2023/6/4", "18/4/2023", NA))

  # Check error messages
  expect_error(
    format_date(df, "mdy_date", "foobar"),
    regexp = "date_format contains invalid variables: foo")
  expect_error(
    format_date(df, "mdy_date", "m/d/Y"),
    regexp = 'Date does not match format "m/d/Y"')
  expect_error(
    format_date(df, "bad_date", "m/d/Y"),
    regexp = 'Date is improperly formatted in rows: 2, 3')

  # Check works
  expect_equal(format_date(df, "good_date"), df)
  expect_equal(format_date(df, "na_date"), df)
  expect_equal(format_date(df, "mdy_date", "m/d/y"), df_format)
})
