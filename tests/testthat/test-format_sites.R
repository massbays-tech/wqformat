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
test_that("format_sites converts MassWateR to wqdashboard", {
  df_wqd <- data.frame(
    Site_ID = c("ABT-010", "ABT-144", "SUD-086"),
    Site_Name = c(
      "477 Lowell Rd, Concord", "Rte 62, Stow", "River Rd, Wayland"
    ),
    Latitude = c(42.47037, 42.404519, 42.37398),
    Longitude = c(-71.362579, -71.526349, -71.381739),
    Town = NA,
    County = NA,
    State = NA_character_,
    Watershed = NA,
    Group = NA,
    Location_Type = NA,
    Surface_Depth = NA,
    Midwater_Depth = NA,
    Near_Bottom_Depth = NA,
    Bottom_Depth = NA,
    Location.Group = c("Lower Assabet", "Upper Assabet", NA)
  )

  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_sites(
          tst$mwr_sites,
          "MassWateR",
          "WQdashboard",
          drop_extra_col = FALSE
        )
      )
    ),
    df_wqd
  )
})

test_that("format_sites converts WQX to wqdashboard", {
  df_wqd <- data.frame(
    Site_ID = c(
      "ML-01", "ML-02", "ML-03", "ML-04", "ML-05", "ML-06", "ML-07", "ML-08",
      "ML-09"
    ),
    Site_Name = c(
      "Template ML 1", "Template ML 2", "Template ML 3", "Template ML 4",
      "Template ML 5", "Template ML 6", "Template ML 7", "Template ML 8",
      "Template ML 9"
    ),
    Latitude = c(
      40.594, 40.594, 40.527, 40.657, 40.522, 40.765, 40.771, 40.779, 40.598
    ),
    Longitude = c(
      -111.72, -111.72, -111.755, -111.77, -112.149, -111.848, -111.892,
      -112.099, -111.685
    ),
    Town = NA,
    County = c(
      "Salt Lake", "Berkeley", "Berkeley", "Custer", "Custer", "Salt Lake",
      "Salt Lake", "Salt Lake", "Salt Lake"
    ),
    State = c("UT", "SC", "WV", "CO", "ID", "UT", "UT", "UT", "UT"),
    Watershed = NA,
    Group = NA,
    Location_Type = c(
      "Spring", "River/Stream", "River/Stream", "Spring", "River/Stream",
      "River/Stream", "River/Stream", "River/Stream", "River/Stream"
    ),
    Surface_Depth = NA,
    Midwater_Depth = NA,
    Near_Bottom_Depth = NA,
    Bottom_Depth = NA,
    Tribal.Land.Indicator..Yes.No. = "No",
    Tribal.Land.Name = NA,
    Monitoring.Location.Source.Map.Scale = c(
      24000, NA, NA, 12000, NA, NA, NA, NA, NA
    ),
    Monitoring.Location.Horizontal.Collection.Method = c(
      "Interpolation-Map", "GPS-Unspecified", "GPS-Unspecified",
      "Interpolation-Map", "GPS-Unspecified", "GPS-Unspecified",
      "GPS-Unspecified", "GPS-Unspecified", "GPS-Unspecified"
    ),
    Monitoring.Location.Horizontal.Coordinate.Reference.System = c(
      "NAD27", "NAD83", "NAD83", "NAD27", "NAD83", "NAD83", "NAD83", "NAD83",
      "NAD83"
    ),
    County.Code..Auto.Generated. = c(35, 15, 3, 27, 37, 35, 35, 35, 35),
    HUC.8 = NA,
    HUC.12 = NA
  )

  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_sites(
          tst$wqx_sites,
          "WQX",
          "wqdashboard",
          drop_extra_col = FALSE
        )
      )
    ),
    df_wqd
  )
})

test_that("format_sites to WQX updates state names", {
  df_wqd <- data.frame(
    "Site_ID" = c("RI01", "MA01", "ME01"),
    "Site_Name" = c("NBEP", "MassBays", "Casco Bay"),
    "Latitude" = c(41.82897, 42.31481, 43.66218),
    "Longitude" = c(-71.41924, -71.03931, -70.27354),
    "State" = c("Rhode Island", "Massachusetts", "Maine")
  )

  df_wqx <- suppressMessages(
    format_sites(df_wqd, "wqdashboard", "wqx")
  )

  expect_equal(
    df_wqx[["State Code"]],
    c("RI", "MA", "ME")
  )
})

# Error messages -----

test_that("format_sites error messages", {
  # Invalid in_format or out_format
  expect_error(
    suppressMessages(
      format_sites(tst$mwr_sites, "MassWateR", "bar")
    ),
    regexp = "Invalid format. Acceptable options: "
  )

  expect_error(
    suppressMessages(
      format_sites(tst$mwr_sites, "foo", "WQX")
    ),
    regexp = "Invalid format. Acceptable options: "
  )

  # Can't rename column
  expect_warning(
    suppressMessages(
      format_sites(
        tst$mwr_sites,
        "MassWateR",
        "WQdashboard",
        drop_extra_col = FALSE
      )
    ),
    regexp = "\tUnable to rename 1 columns: Location.Group"
  )
})
