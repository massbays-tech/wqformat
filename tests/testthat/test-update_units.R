# Test convert_unit ----
test_that("convert_unit works", {
  expect_equal(
    convert_unit(32, "deg F", "deg C"),
    0
  )

  expect_equal(
    convert_unit(0, "deg C", "deg F"),
    32
  )

  expect_equal(
    convert_unit(0, "deg C", "deg C"),
    0
  )
})

test_that("convert_unit error messages", {
  expect_error(
    convert_unit(32, "deg F", "deg C", unit_format = "foo"),
    regexp = "unit_format is invalid"
  )

  expect_warning(
    convert_unit(10, "feet", "m"),
    regexp = "Unable to convert feet to m. Did not recognize units."
  )

  expect_warning(
    convert_unit(32, "deg F", "m"),
    regexp = "Unable to convert deg F to m. Incompatible units."
  )
})

# Test set_units ----
test_that("set_units works", {
  # Test - no NA
  df_in <- data.frame(
    Result = c(1, 2000, 3000, 4, 5000, NA),
    Result_Unit = c("mg/L", "ug/L", "ug/L", "mg/L", "ug/L", NA)
  )

  df_out <- data.frame(
    Result = c(1, 2, 3, 4, 5, NA),
    Result_Unit = "mg/L"
  )

  expect_equal(
    set_units(
      df_in,
      "Result",
      "Result_Unit",
      "mg/L"
    ),
    df_out
  )

  # Check - early return
  expect_equal(
    set_units(
      df_out,
      "Result",
      "Result_Unit",
      "mg/L"
    ),
    df_out
  )
})


test_that("set_units error messages", {
  df_in <- data.frame(
    Result = c(1, 2, 3),
    Result_Unit = "mg/L"
  )

  expect_warning(
    set_units(
      df_in,
      "Result",
      "Result_Unit",
      "deg C"
    ),
    regexp = "Unable to update Result units in 3 rows"
  )
})

# Test standardize_units ----
test_that("standardize_units works", {
  # Test - no NA
  df_in <- tst$wqx_data[c(9, 11, 29, 34:35, 48), ]
  rownames(df_in) <- NULL

  df_out <- df_in
  df_out["Result.Value"] <- c(
    11.2, 119, 16, 134, 16.3333333, 13.8
  )
  df_out["Result.Unit"] <- c(
    "deg C", "#/100ml", "deg C", "#/100ml", "deg C", "deg C"
  )

  expect_equal(
    standardize_units(
      df_in,
      "Characteristic.Name",
      "Result.Value",
      "Result.Unit"
    ),
    df_out
  )

  # Test - with NA
  df_in <- tst$wqx_data[c(9, 11, 29, 34:35, 44, 48), ]
  rownames(df_in) <- NULL

  df_out <- df_in
  df_out["Result.Value"] <- c(
    11.2, 119, 16, 134, 16.3333333, NA, 13.8
  )
  df_out["Result.Unit"] <- c(
    "deg C", "#/100ml", "deg C", "#/100ml", "deg C", "deg C", "deg C"
  )

  expect_equal(
    standardize_units(
      df_in,
      "Characteristic.Name",
      "Result.Value",
      "Result.Unit"
    ),
    df_out
  )

  # Test - early return
  expect_equal(
    suppressWarnings(
      standardize_units(
        tst$mwr_data,
        "Characteristic.Name",
        "Result.Value",
        "Result.Unit"
      )
    ),
    tst$mwr_data
  )
})

test_that("standardize_units error messages", {
  expect_warning(
    standardize_units(
      tst$wqx_data,
      "Characteristic.Name",
      "Result.Value",
      "Result.Unit"
    ),
    regexp = "Unable to standardize units"
  )

  expect_error(
    standardize_units(
      tst$wqx_data,
      "Characteristic.Name",
      "Result.Value",
      "Result.Unit",
      warn_only = FALSE
    ),
    regexp = "Unable to standardize units"
  )
})

# Test standardize_units_across ----
test_that("standardize_units_across works", {
  df_in <- data.frame(
    Result_Unit = c("mg/L", "mg/L", "deg C"),
    Lower_Detection_Limit = c(NA, 2, 32),
    Upper_Detection_Limit = c(10000, NA, 212),
    Detection_Limit_Unit = c("ug/L", "mg/L", "deg F")
  )

  df_out <- data.frame(
    Result_Unit =  c("mg/L", "mg/L", "deg C"),
    Lower_Detection_Limit = c(NA, 2, 0),
    Upper_Detection_Limit = c(10, NA, 100),
    Detection_Limit_Unit =  c("mg/L", "mg/L", "deg C")
  )

  expect_equal(
    standardize_units_across(
      df_in,
      "Result_Unit",
      "Detection_Limit_Unit",
      c("Lower_Detection_Limit", "Upper_Detection_Limit")
    ),
    df_out
  )

  # Test edge cases
  df_in <- data.frame(
    Result_Unit =  c("mg/L", "mg/L", "deg C"),
    Lower_Detection_Limit = c(NA, 2, 0),
    Upper_Detection_Limit = c(10, NA, 100),
    Detection_Limit_Unit =  c("mg/L", "mg/L", "deg C")
  )

  expect_equal(
    standardize_units_across(
      df_in,
      "Result_Unit",
      "Detection_Limit_Unit",
      c("Lower_Detection_Limit", "Upper_Detection_Limit")
    ),
    df_in
  )

  df_in <- data.frame(
    Result_Unit = c("mg/L", "mg/L", "deg C", "deg C"),
    Lower_Detection_Limit = c(NA, 2, 2, 32),
    Upper_Detection_Limit = c(10000, NA, 10000, 212),
    Detection_Limit_Unit = c("ug/L", "mg/L", "ug/L", "deg F")
  )

  df_out <- data.frame(
    Result_Unit =  c("mg/L", "mg/L", "deg C", "deg C"),
    Lower_Detection_Limit = c(NA, 2, 2, 0),
    Upper_Detection_Limit = c(10, NA, 10000, 100),
    Detection_Limit_Unit =  c("mg/L", "mg/L", "ug/L", "deg C")
  )

  expect_equal(
    suppressWarnings(
      standardize_units_across(
        df_in,
        "Result_Unit",
        "Detection_Limit_Unit",
        c("Lower_Detection_Limit", "Upper_Detection_Limit")
      )
    ),
    df_out
  )
})

test_that("standardize_units_across error messages", {
  df_in <- data.frame(
    Result_Unit = c("mg/L", "mg/L", "deg C", "deg C"),
    Lower_Detection_Limit = c(NA, 2, 2, 32),
    Upper_Detection_Limit = c(10000, NA, 10000, 212),
    Detection_Limit_Unit = c("ug/L", "mg/L", "ug/L", "deg F")
  )

  expect_warning(
    standardize_units_across(
      df_in,
      "Result_Unit",
      "Detection_Limit_Unit",
      c("Lower_Detection_Limit", "Upper_Detection_Limit")
    ),
    regexp = "Unable to standardize units in row"
  )

  expect_error(
    standardize_units_across(
      df_in,
      "Result_Unit",
      "Detection_Limit_Unit",
      c("Lower_Detection_Limit", "Upper_Detection_Limit"),
      warn_only = FALSE
    ),
    regexp = "Unable to standardize units in row"
  )
})
