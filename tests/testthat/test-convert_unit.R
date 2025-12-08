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
    convert_unit(32, "deg F", "deg C", unit_format = 'foo'),
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
