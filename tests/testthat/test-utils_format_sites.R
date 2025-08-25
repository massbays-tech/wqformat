# Test prep_brc_sites----
test_that("prep_brc_sites works", {
  df <- data.frame(
    "TOWN" = c(
      "Worcester", "Uxbridge", "Grafton", "Cumberland", "Lincoln", "Blackstone",
      "Northbridge", "Woonsocket", "Pawtucket", "North Smithfield", "Millbury",
      "Sutton", "Upton", "Douglas", "Burrillville", "Auburn", "Shrewsbury",
      "Hopedale", "Mendon", "Bellingham", "Boylston", "foo", "bar"
    ),
    "WATER_DEPTH_FT" = c(
      1, 10, 12, NA, NA, 3.280839895, 32.80839895, 39.3700787402, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ),
    "CFR" = c(
      "Yes", "No", "No", "No", "Yes", "No", NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA
    )
  )

  df_out <- data.frame(
    "TOWN" = c(
      "Worcester", "Uxbridge", "Grafton", "Cumberland", "Lincoln", "Blackstone",
      "Northbridge", "Woonsocket", "Pawtucket", "North Smithfield", "Millbury",
      "Sutton", "Upton", "Douglas", "Burrillville", "Auburn", "Shrewsbury",
      "Hopedale", "Mendon", "Bellingham", "Boylston", "foo", "bar"
    ),
    "WATER_DEPTH_FT" = c(
      1, 10, 12, NA, NA, 3.280839895, 32.80839895, 39.3700787402, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ),
    "CFR" = c(
      "Coldwater", "Warmwater", "Warmwater", "Warmwater", "Coldwater",
      "Warmwater", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA
    ),
    "STATE" = c(
      "MA", "MA", "MA", "RI", "RI", "MA", "MA", "RI", "RI", "RI", "MA", "MA",
      "MA", "MA", "RI", "MA", "MA", "MA", "MA", "MA", "MA", NA, NA
    ),
    "WATER_DEPTH_M" = c(
      0.3048, 3.048, 3.6576, NA, NA, 1, 10, 12, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA
    )
  )

  expect_equal(prep_brc_sites(df), df_out)
})

# Test sites_to_brc ----
test_that("sites_to_brc works", {
  df <- data.frame(
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

  expect_equal(sites_to_brc(df), df_out)
})
