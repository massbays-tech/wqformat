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
    "Town" = NA,
    "County" = NA,
    "State" = NA_character_,
    "Watershed" = NA,
    "Group" = NA,
    "Location_Type" = NA,
    "Surface_Depth" = NA,
    "Midwater_Depth" = NA,
    "Near_Bottom_Depth" = NA,
    "Bottom_Depth" = NA
  )

  # Test conversions
  expect_equal(
    suppressMessages(
      format_sites(df_me_focb, "ME_FOCB", "MassWateR")
    ),
    df_masswater
  )
  expect_equal(
    suppressMessages(
      format_sites(df_masswater, "MassWateR", "WQdashboard")
    ),
    df_wqdashboard
  )

  # Test conversions - extra columns retained
  Town <- c("Providence", "Boston", "Portland")
  df_extracol <- cbind(df_masswater, Town)

  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_sites(df_me_focb, "ME_FOCB", "MassWateR", drop_extra_col = FALSE)
      )
    ),
    df_extracol
  )

  # Test warnings
  expect_warning(
    suppressMessages(
      format_sites(
        df_wqdashboard,
        "WQdashboard", "MassWateR",
        drop_extra_col = FALSE
      )
    ),
    regexp = "\tUnable to rename 10 columns: Town, County, State, Watershed, Group, Location_Type, Surface_Depth, Midwater_Depth, Near_Bottom_Depth, Bottom_Depth"
  )
})
