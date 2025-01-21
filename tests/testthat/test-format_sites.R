test_that("format_sites works", {
  df_me_focb <- data.frame(
    "Site ID" = c("RI01", "MA01", "ME01"),
    "Station_Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Town" = c("Providence", "Boston", "Portland"),
    "X" = c(-71.41924, -71.03931, -70.27354),
    "Y" = c(41.82897, 42.31481, 43.66218),
    "Category" = c("Freshwater", "Saltwater", "Saltwater"),
    check.names = FALSE
  )

  df_masswater <- data.frame(
    "Monitoring Location ID" = c("RI01", "MA01", "ME01"),
    "Monitoring Location Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Monitoring Location Latitude" = c(41.82897, 42.31481, 43.66218),
    "Monitoring Location Longitude" = c(-71.41924, -71.03931, -70.27354),
    "Location Group" = c("Freshwater", "Saltwater", "Saltwater"),
    check.names = FALSE
  )

  df_wqdashboard <- data.frame(
    "Site_ID" = c("RI01", "MA01", "ME01"),
    "Site_Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Latitude" = c(41.82897, 42.31481, 43.66218),
    "Longitude" = c(-71.41924, -71.03931, -70.27354),
    "Town" = c(NA, NA, NA),
    "County" = c(NA, NA, NA),
    "State" = c(NA_character_, NA_character_, NA_character_),
    "Watershed" = c(NA, NA, NA),
    "Group" = c("Freshwater", "Saltwater", "Saltwater"),
    "Location_Type" = c(NA, NA, NA),
    "Max_Depth_Surface" = c(NA, NA, NA),
    "Max_Depth_Midwater" = c(NA, NA, NA),
    "Max_Depth_Near_Bottom" = c(NA, NA, NA)
  )

  # Test conversions
  expect_equal(
    format_sites(df_me_focb, "ME_FOCB", "MassWateR", show_messages = FALSE),
    df_masswater
  )
  expect_equal(
    format_sites(df_masswater, "MassWateR", "WQdashboard", show_messages = FALSE),
    df_wqdashboard
  )

  # Test conversions - extra columns retained
  Town <- c("Providence", "Boston", "Portland")
  df_extracol <- cbind(df_masswater, Town)

  expect_equal(
    suppressWarnings(
      format_sites(
        df_me_focb, "ME_FOCB", "MassWateR", drop_extra_col = FALSE,
        show_messages = FALSE
      )
    ),
    df_extracol
  )

  # Test warnings
  expect_warning(
    format_sites(
      df_wqdashboard, "WQdashboard", "MassWateR", drop_extra_col = FALSE,
      show_messages = FALSE
    ),
    regexp = "\tUnable to rename 8 columns: Town, County, State, Watershed, Location_Type, Max_Depth_Surface, Max_Depth_Midwater, Max_Depth_Near_Bottom"
  )
})
