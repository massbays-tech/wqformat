# Maine ----
test_that("format_sites converts ME_FOCB", {
  df_test <- data.frame(
    "Site ID" = c("RI01", "MA01", "ME01"),
    "Station_Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Town" = c("Providence", "Boston", "Portland"),
    "X" = c(-71.41924, -71.03931, -70.27354),
    "Y" = c(41.82897, 42.31481, 43.66218),
    "Category" = c("Freshwater", "Saltwater", "Saltwater"),
    check.names = FALSE
  )

  expect_equal(
    suppressMessages(
      format_sites(df_test, "ME_FOCB", "MassWateR")
    ),
    data.frame(
      "Monitoring Location ID" = c("RI01", "MA01", "ME01"),
      "Monitoring Location Name" = c("NBEP", "MassBays", "Casco Bay"),
      "Monitoring Location Latitude" = c(41.82897, 42.31481, 43.66218),
      "Monitoring Location Longitude" = c(-71.41924, -71.03931, -70.27354),
      "Location Group" = c("Freshwater", "Saltwater", "Saltwater"),
      check.names = FALSE
    )
  )

  expect_equal(
    suppressMessages(
      format_sites(df_test, "ME_FOCB", "WQdashboard")
    ),
    data.frame(
      "Site_ID" = c("RI01", "MA01", "ME01"),
      "Site_Name" = c("NBEP", "MassBays", "Casco Bay"),
      "Latitude" = c(41.82897, 42.31481, 43.66218),
      "Longitude" = c(-71.41924, -71.03931, -70.27354),
      "Town" = c("Providence", "Boston", "Portland"),
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
  )
})

# Massachusetts ----
test_that("format_sites converts MA_BRC", {
  df_brc <- data.frame(
    "SITE_NUMBER" = c(1, 2),
    "BRC_CODE" = c("RI01", "MA01"),
    "SITE_NAME" = c("NBEP", "BRC"),
    "WATERBODY_NAME" = c("Woonasquatucket River", "Blackstone River"),
    "ZONE" = c(NA, "Headwaters"),
    "SITE_DESCR" = "lorem ipsum",
    "LATITUDE" = c(41.82897, 42.26034),
    "LONGITUDE" = c(-71.41924, -71.80386),
    "WATERSHED" = c("Woonasquatucket River", "Tatnuck Brook-Blackstone River"),
    "TOWN" = c("Providence", "Worcester"),
    "CFR" = "No",
    "HUC12_NUM" = c("010900040502", "010900030102"),
    "HUC_NAME" = c("Woonasquatucket River", "Tatnuck Brook-Blackstone River"),
    "DIRECTIONS" = "dolor sit amet",
    "JUSTIFICATION" = "consectetur adipiscing elit",
    "STATUS" = "Active",
    "CONDUCTIVITY_USCM" = NA,
    "WATER_DEPTH_FT" = 1
  )

  df_wqdashboard <- data.frame(
    "Site_ID" = c("RI01", "MA01"),
    "Site_Name" = c("NBEP", "BRC"),
    "Latitude" = c(41.82897, 42.26034),
    "Longitude" = c(-71.41924, -71.80386),
    "Town" = c("Providence", "Worcester"),
    "County" = NA,
    "State" = c(NA, "MA"),
    "Watershed" = c(
      "Woonasquatucket River",
      "Tatnuck Brook-Blackstone River"
    ),
    "Group" = "Warmwater",
    "Location_Type" = NA,
    "Surface_Depth" = NA,
    "Midwater_Depth" = NA,
    "Near_Bottom_Depth" = NA,
    "Bottom_Depth" = 0.3048
  )

  # Test from BRC
  expect_equal(
    suppressMessages(
      format_sites(df_brc, "MA_BRC", "MassWateR")
    ),
    data.frame(
      "Monitoring Location ID" = c("RI01", "MA01"),
      "Monitoring Location Name" = c("NBEP", "BRC"),
      "Monitoring Location Latitude" = c(41.82897, 42.26034),
      "Monitoring Location Longitude" = c(-71.41924, -71.80386),
      "Location Group" = c(NA, "Headwaters"),
      check.names = FALSE
    )
  )

  expect_equal(
    suppressMessages(
      format_sites(df_brc, "MA_BRC", "WQdashboard")
    ),
    df_wqdashboard
  )

  # Test to BRC
  expect_equal(
    suppressMessages(
      format_sites(df_wqdashboard, "WQdashboard", "MA_BRC")
    ),
    df_brc <- data.frame(
      "SITE_NUMBER" = NA,
      "BRC_CODE" = c("RI01", "MA01"),
      "SITE_NAME" = c("NBEP", "BRC"),
      "WATERBODY_NAME" = NA,
      "ZONE" = NA,
      "SITE_DESCR" = NA,
      "LATITUDE" = c(41.82897, 42.26034),
      "LONGITUDE" = c(-71.41924, -71.80386),
      "WATERSHED" = NA,
      "TOWN" = c("Providence", "Worcester"),
      "CFR" = "No",
      "HUC12_NUM" = NA,
      "HUC_NAME" = c("Woonasquatucket River", "Tatnuck Brook-Blackstone River"),
      "DIRECTIONS" = NA,
      "JUSTIFICATION" = NA,
      "STATUS" = NA,
      "CONDUCTIVITY_USCM" = NA,
      "WATER_DEPTH_FT" = 1
    )
  )
})

# Rhode Island ----
test_that("format_sites converts RI_WW", {
  df_test <- data.frame(
    "WW_StaNumb" = c(1, 2),
    "WW_Station" = c("WW001", "WW002"),
    "WBID" = c("RI0008040L-01", "RI0008040L-01"),
    "WB_Type" = c("Reservoir", "Lake or Pond"),
    "Site_DESCR" = c("Alton Pond", "Barber Pond"),
    "PARKING_ACCESS" = NA,
    "Town" = c("Hopkinton", "South Kingstown"),
    "BorderTown" = c("Richmon, Hopkinton", NA),
    "COUNTY" = "WASHINGTON",
    "State" = "RI",
    "LAT_DD" = c(41.438171, 41.504129),
    "LON_DD" = c(-71.721642, -71.564271),
    "ProjID" = 12,
    "Year_added" = 1988,
    "Active" = c(2022, 2018),
    "Status" = c("Active", "Not Active"),
    "DepthCode" = c("S", "D"),
    "MaxDepth_m" = c(4.2, 4.6),
    "PublicAcce" = "Yes",
    "LakeAcreag" = c(99.7, 28.5),
    "sq__meter" = NA,
    "HUC_8" = "01090005",
    "HUC_10" = c("0109000501", "0109000502"),
    "HUC_12" = c("010900050102", "010900050204"),
    "HUC_10_NAME" = c("Wood River", "Upper Pawcatuck River"),
    "HUC_12_NAME" = c("Lower Wood River", "Usquepaug River-Pawcatuck River"),
    "Image" = NA,
    "elevation_Feet" = c(46.9, 114.8),
    "elevation_meters" = c(14.3, 35.0),
    "Ord_Id" = "WPWA"
  )

  expect_equal(
    suppressMessages(
      format_sites(df_test, "RI_WW", "MassWateR")
    ),
    data.frame(
      "Monitoring Location ID" = c("WW001", "WW002"),
      "Monitoring Location Name" = c("Alton Pond", "Barber Pond"),
      "Monitoring Location Latitude" = c(41.438171, 41.504129),
      "Monitoring Location Longitude" = c(-71.721642, -71.564271),
      "Location Group" = NA,
      check.names = FALSE
    )
  )

  expect_equal(
    suppressMessages(
      format_sites(df_test, "RI_WW", "WQdashboard")
    ),
    data.frame(
      "Site_ID" = c("WW001", "WW002"),
      "Site_Name" = c("Alton Pond", "Barber Pond"),
      "Latitude" = c(41.438171, 41.504129),
      "Longitude" = c(-71.721642, -71.564271),
      "Town" = c("Hopkinton", "South Kingstown"),
      "County" = "WASHINGTON",
      "State" = "RI",
      "Watershed" = c("Lower Wood River", "Usquepaug River-Pawcatuck River"),
      "Group" = NA,
      "Location_Type" = c("Reservoir", "Lake or Pond"),
      "Surface_Depth" = NA,
      "Midwater_Depth" = NA,
      "Near_Bottom_Depth" = NA,
      "Bottom_Depth" = c(4.2, 4.6)
    )
  )
})

# Other ----
test_that("format_sites to WQX", {
  df_test <- data.frame(
    "Site_ID" = c("RI01", "MA01", "ME01"),
    "Site_Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Latitude" = c(41.82897, 42.31481, 43.66218),
    "Longitude" = c(-71.41924, -71.03931, -70.27354),
    "State" = c("RI", "MA", "ME")
  )

  expect_equal(
    suppressMessages(
      format_sites(df_test, "WQdashboard", "WQX")
    ),
    data.frame(
      "Monitoring Location ID" = c("RI01", "MA01", "ME01"),
      "Monitoring Location Name" = c("NBEP", "MassBays", "Casco Bay"),
      "Monitoring Location Type" = NA,
      "Tribal Land Indicator (Yes/No)" = NA,
      "Tribal Land Name" = NA,
      "Monitoring Location Latitude (DD.DDDD)" = c(
        41.82897, 42.31481, 43.66218
      ),
      "Monitoring Location Longitude (-DDD.DDDD)" = c(
        -71.41924, -71.03931, -70.27354
      ),
      "Monitoring Location Source Map Scale" = NA,
      "Monitoring Location Horizontal Collection Method" = NA,
      "Monitoring Location Horizontal Coordinate Reference System" = NA,
      "State Code" = c("RI", "MA", "ME"),
      "Monitoring Location County Name" = NA,
      "County Code (Auto-Generated)" = NA,
      "HUC 8" = NA,
      "HUC 12" = NA,
      check.names = FALSE
    )
  )
})

test_that("format_sites error messages", {
  df_wqdashboard <- data.frame(
    "Site_ID" = c("RI01", "MA01", "ME01"),
    "Site_Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Latitude" = c(41.82897, 42.31481, 43.66218),
    "Longitude" = c(-71.41924, -71.03931, -70.27354),
    "Town" = c("Providence", "Boston", "Portland"),
    "State" = c("RI", "MA", "ME")
  )

  expect_error(
    suppressMessages(
      format_sites(df_in, "WQdashboard", "bar")
    ),
    regexp = "Invalid format. Acceptable options: "
  )

  expect_warning(
    suppressMessages(
      format_sites(
        df_wqdashboard,
        "WQdashboard",
        "MassWateR",
        drop_extra_col = FALSE
      )
    ),
    regexp = "\tUnable to rename 2 columns: Town, State"
  )
})


test_that("format_sites extra options", {
  df_me_focb <- data.frame(
    "Site ID" = c("RI01", "MA01", "ME01"),
    "Station_Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Town" = c("Providence", "Boston", "Portland"),
    "X" = c(-71.41924, -71.03931, -70.27354),
    "Y" = c(41.82897, 42.31481, 43.66218),
    "Category" = c("Freshwater", "Saltwater", "Saltwater"),
    check.names = FALSE
  )

  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_sites(df_me_focb, "ME_FOCB", "MassWateR", drop_extra_col = FALSE)
      )
    ),
    data.frame(
      "Monitoring Location ID" = c("RI01", "MA01", "ME01"),
      "Monitoring Location Name" = c("NBEP", "MassBays", "Casco Bay"),
      "Monitoring Location Latitude" = c(41.82897, 42.31481, 43.66218),
      "Monitoring Location Longitude" = c(-71.41924, -71.03931, -70.27354),
      "Location Group" = c("Freshwater", "Saltwater", "Saltwater"),
      "Town" = c("Providence", "Boston", "Portland"),
      check.names = FALSE
    )
  )
})
