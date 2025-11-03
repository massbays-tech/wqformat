# Test prep_brc_sites----
test_that("prep_brc_sites works", {
  df_in <- tst$ma_brc_sites
  # add fake depths to test unit conversion
  df_in$WATER_DEPTH_FT <- c(NA, NA, 1, 3.28083990, NA)

  df_out <- df_in
  df_out$CFR <- c(
    "Warmwater", "Coldwater", "Coldwater", "Coldwater", "Warmwater"
  )
  df_out$STATE <- "MA"
  df_out$WATER_DEPTH_M <- c(NA, NA, 0.3048, 1, NA)

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
      1, 2, 3, 4, NA, 3.280839895, 32.80839895, 39.3700787402
    ),
    "CFR" = c("Yes", "Yes", "No", "No", "No", NA, "Yes", "No")
  )

  expect_equal(sites_to_brc(df_in), df_out)
})
