# Test rename_col ----
test_that("rename_col works", {
  dat <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  # Check auto-return
  expect_equal(
    rename_col(dat, NA, NA),
    dat
  )
  expect_equal(
    rename_col(dat, c("species", "genus"), c("species", "genus")),
    dat
  )
  expect_equal(
    rename_col(dat, c("cat", "dog"), c("kitten", "puppy")),
    dat
  )

  # Check 1:1 column name conversion
  expect_equal(
    rename_col(
      dat,
      c("species", "genus", "numbers"),
      c("foo", "bar", "foofy")
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "foofy" = c(1, 2, 3, 4)
    )
  )

  # Check 2:1 column name conversion
  expect_equal(
    rename_col(
      dat,
      c("species", "class"),
      c("foo", "foo")
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", "mammal", NA),
      "numbers" = c(1, 2, 3, 4)
    )
  )

  # Check overlapping 3:1 and 1:3 column name conversions
  expect_equal(
    rename_col(
      dat,
      c("species", "class", "numbers", "numbers", "numbers"),
      c("foo", "foo", "foo", "bar", "foofy")
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", "mammal", 4),
      "bar" = c(1, 2, 3, 4),
      "foofy" = c(1, 2, 3, 4)
    )
  )

  # Check NA handling, pipes
  expect_equal(
    dat |>
      rename_col(
        c("species", "genus", "numbers"),
        c(NA, "bar", "foofy")
      ),
    data.frame(
      "species" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "foofy" = c(1, 2, 3, 4)
    )
  )
})

test_that("rename_col error messages", {
  dat <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4)
  )

  expect_error(
    rename_col(dat, c("species", "class"), "foo"),
    regexp = "old_colnames and new_colnames are different lengths"
  )
})

# Check concat_col ----

test_that("concat_col works", {
  dat <- data.frame(
    "species" = c("aardvark", "bittern", NA, NA),
    "class" = c("mammal", "bird", "mammal", NA),
    "numbers" = c(1, 2, 3, 4),
    "numbers2" = c(1, 3, 2, NA)
  )

  # Test intended use
  expect_equal(
    concat_col(
      dat,
      c("species", "class"),
      "foo"
    ),
    data.frame(
      "foo" = c("aardvark", "bittern", "mammal", NA),
      "species" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "numbers" = c(1, 2, 3, 4),
      "numbers2" = c(1, 3, 2, NA)
    )
  )
  expect_equal(
    concat_col(
      dat,
      c("species", "class", "numbers"),
      "numbers"
    ),
    data.frame(
      "numbers" = c("aardvark", "bittern", "mammal", 4),
      "species" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "numbers2" = c(1, 3, 2, NA)
    )
  )

  # Test concatenation
  expect_equal(
    concat_col(
      dat,
      c("class", "numbers", "numbers2"),
      "foo",
      concat = TRUE
    ),
    data.frame(
      "species" = c("aardvark", "bittern", NA, NA),
      "foo" = c("mammal; 1", "bird; 2; 3", "mammal; 3; 2", "4"),
      "class" = c("mammal", "bird", "mammal", NA),
      "numbers" = c(1, 2, 3, 4),
      "numbers2" = c(1, 3, 2, NA)
    )
  )

  # Test edge cases, pipes
  expect_equal(
    dat |>
      concat_col("species", "foo") |> # only 1 in_colnames
      concat_col("foofy", "owl"), # invalid in_colnames
    data.frame(
      "species" = c("aardvark", "bittern", NA, NA),
      "class" = c("mammal", "bird", "mammal", NA),
      "numbers" = c(1, 2, 3, 4),
      "numbers2" = c(1, 3, 2, NA),
      "foo" = c("aardvark", "bittern", NA, NA),
      "owl" = NA
    )
  )
})

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
    df_in |>
      col_to_numeric("col1") |>
      col_to_numeric("col2") |>
      col_to_numeric("col3"),
    df_out
  )
})

test_that("col_to_numeric error message", {
  df_in <- data.frame(
    "col1" = c("1", "2", "4"),
    "col2" = c("A", "5", "6")
  )

  expect_silent(
    col_to_numeric(df_in, "col1", FALSE)
  )

  expect_error(
    col_to_numeric(df_in, "col2", FALSE),
    regexp = "Non-numeric values detected in col2. Check rows: 1"
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
    df_in |>
      col_to_date("good_date") |>
      col_to_date("na_date") |>
      col_to_date("mdy_date") |>
      col_to_date("datetime", date_format = "m/d/y H:M", datetime = TRUE) |>
      col_to_date("ymd_date", date_format = "y/m/d") |>
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
