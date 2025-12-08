# Test prep_brc_sites----
test_that("prep_brc_sites works", {
  df_in <- tst$ma_brc_sites
  # add fake depths to test unit conversion
  df_in$WATER_DEPTH_FT <- c(NA, NA, 1, 3.28084, NA)

  df_out <- df_in
  df_out$CFR <- c(
    "Warmwater", "Coldwater", "Coldwater", "Coldwater", "Warmwater"
  )
  df_out$STATE <- "MA"
  df_out$WATER_DEPTH_M <- c(NA, NA, 0.30479999, 1, NA)

  expect_equal(
    prep_brc_sites(df_in),
    df_out
  )

  # Edge case - no depth column
  df_in <- tst$ma_brc_sites
  df_in$WATER_DEPTH_FT <- NULL

  df_out$WATER_DEPTH_FT <- NA
  df_out$WATER_DEPTH_M <- NA_integer_

  expect_equal(
    prep_brc_sites(df_in),
    df_out
  )
})

# Test sites_to_brc ----
test_that("sites_to_brc works", {
  # fake data to test unit conversion, data handling
  df_in <- data.frame(
    "STATE" = c("MA", "MA", "RI", "MA", "MA", "RI", "MA", "RI"),
    "WATER_DEPTH_M" = c(0.3048, 3.048, 3.6576, NA, NA, 1, 10, 12),
    "WATER_DEPTH_FT" = c(1, 2, 3, 4, NA, NA, NA, NA),
    "CFR" = c(
      "Coldwater", "Coldwater", "Warmwater", "Warmwater", "Warmwater", NA,
      "Coldwater", "Warmwater"
    )
  )
  df_out <- data.frame(
    "WATER_DEPTH_FT" = c(
      1, 2, 3, 4, NA, 3.28084, 32.8084, 39.37008
    ),
    "CFR" = c("Yes", "Yes", "No", "No", "No", NA, "Yes", "No")
  )

  expect_equal(sites_to_brc(df_in), df_out)
})

# Test sites_to_wqdashboard ----
test_that("sites_to_wqdashboard works", {
  # Basic test - fake data for simplicity's sake
  df_in <- data.frame(
    "Group" = c("Coldwater", NA, "Warmwater", NA),
    "Location_Type" = c("River", "River", "Ocean", "Ocean")
  )

  df_out <- data.frame(
    "Group" = c("Coldwater", NA, "Warmwater", "Saltwater"),
    "Location_Type" = c("River", "River", "Ocean", "Ocean")
  )

  expect_equal(sites_to_wqdashboard(df_in), df_out)

  # Edge case 1 - drop extra col is TRUE
  df_out$Location_Type <- NULL

  expect_equal(sites_to_wqdashboard(df_in, TRUE), df_out)

  # Edge case 2 - Location_Type is blank
  df_in$Location_Type <- NA
  df_out <- data.frame(
    "Group" = c("Coldwater", NA, "Warmwater", NA)
  )

  expect_equal(sites_to_wqdashboard(df_in), df_out)
})
