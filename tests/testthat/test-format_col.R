# Test col_to_numeric() ----
test_that("col_to_numeric works", {
  df_in <- data.frame(
    "col1" = c("1", "2", "4"),
    "col2" = c("A", "5", "6"),
    "col3" = c(7, 8, 9)
  )

  df_out <- data.frame(
    "col1" = c(1, 2, 4),
    "col2" = c("A", "5", "6"),
    "col3" = c(7, 8, 9)
  )

  expect_equal(
    df_in %>%
      col_to_numeric("col1") %>%
      col_to_numeric("col2") %>%
      col_to_numeric("col3"),
    df_out
  )
})

# Test col_to_date() ----
test_that("col_to_date works", {
  df_in <- data.frame(
    "good_date" = c("2022-03-01", "2023-04-18", "2004-06-12", "2024-09-26"),
    "na_date" = c("2022-03-01", NA, NA, "2024-09-26"),
    "mdy_date" = c("3/1/2022", NA, "4/18/2023", NA),
    "ymd_date" = c("22/3/1", NA, "23/4/18", NA),
    "datetime" = c("3/1/22 10:30", NA, "4/18/23 8:20", "9/26/24 15:50"),
    "blank_col" = NA
  )
  df_in$good_date <- as.Date(df_in$good_date)
  df_in$na_date <- as.Date(df_in$na_date)

  df_out <- data.frame(
    "good_date" = c("2022-03-01", "2023-04-18", "2004-06-12", "2024-09-26"),
    "na_date" = c("2022-03-01", NA, NA, "2024-09-26"),
    "mdy_date" = c("2022-03-01", NA, "2023-04-18", NA),
    "ymd_date" = c("2022-03-01", NA, "2023-04-18", NA),
    "datetime" = c("2022-03-01 10:30", NA, "2023-04-18 8:20", "24-09-26 15:50"),
    "blank_col" = NA
  )
  df_out$good_date <- as.Date(df_out$good_date)
  df_out$na_date <- as.Date(df_out$na_date)
  df_out$datetime <- lubridate::ymd_hm(df_out$datetime, tz = Sys.timezone())
  df_out$mdy_date <- as.Date(df_out$mdy_date)
  df_out$ymd_date <- as.Date(df_out$ymd_date)
  df_out$blank_col <- as.Date(df_out$blank_col)

  expect_equal(
    df_in %>%
      col_to_date("good_date") %>%
      col_to_date("na_date") %>%
      col_to_date("mdy_date") %>%
      col_to_date("datetime", date_format = "m/d/y H:M") %>%
      col_to_date("ymd_date", date_format = "y/m/d") %>%
      col_to_date("blank_col"),
    df_out
  )
})

test_that("col_to_date error messages", {
  df_in <- data.frame(
    "mdy_date" = c("3/1/22", NA, "4/18/23", NA),
    "bad_date" = c("3/1/2022", "2023/6/4", "18/4/2023", NA)
  )

  # Check error messages
  expect_error(
    col_to_date(df_in, "good_date", "foobar"),
    regexp = "good_date is not a valid column"
  )
  expect_error(
    col_to_date(df_in, "mdy_date", "foobar"),
    regexp = "date_format contains invalid variables: foo"
  )
  expect_error(
    col_to_date(df_in, "mdy_date", "m/d/Y"),
    regexp = 'Date does not match format "m/d/Y"'
  )
  expect_error(
    col_to_date(df_in, "bad_date", "m/d/Y"),
    regexp = "Date is improperly formatted in rows: 2, 3"
  )
  expect_error(
    col_to_date(df_in, "mdy_date", date_format = ""),
    regexp = "Date format is missing"
  )
})

# Test col_to_state() ----
test_that("col_to_state works", {
  df_in <- data.frame(
    "State_abb" = c("RI", "MA"),
    "State_name" = c("Rhode Island", "Massachusetts"),
    "State_mix" = c("Rhode Island", "MA"),
    "State_error" = c("RI", "foo")
  )

  expect_equal(
    df_in %>%
      col_to_state("State_abb", abb = FALSE) %>%
      col_to_state("State_name") %>%
      col_to_state("State_mix", abb = FALSE),
    data.frame(
      "State_abb" = c("Rhode Island", "Massachusetts"),
      "State_name" = c("RI", "MA"),
      "State_mix" = c("Rhode Island", "Massachusetts"),
      "State_error" = c("RI", "foo")
    )
  )
  expect_equal(
    suppressWarnings(
      col_to_state(df_in, "State_error", abb = FALSE)
    ),
    data.frame(
      "State_abb" = c("RI", "MA"),
      "State_name" = c("Rhode Island", "Massachusetts"),
      "State_mix" = c("Rhode Island", "MA"),
      "State_error" = c("Rhode Island", "foo")
    )
  )
})

test_that("col_to_state error messages", {
  dat <- data.frame(
    "State_name" = c("Rhode Island", "Massachusetts"),
    "State_error" = c("RI", "foo")
  )

  expect_warning(
    col_to_state(dat, "State_error"),
    regexp = "foo is not a valid state name"
  )
})
