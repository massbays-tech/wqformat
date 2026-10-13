# Maine ----
test_that("format_sites converts ME_FOCB to MassWateR", {
  df_mwr <- data.frame(
    "Monitoring Location ID" = c("BMR02", "EEB18", "HR2", "HR4", "KVL84"),
    "Monitoring Location Name" = c(
      "B&M Railroad /Back Cove", "East End Beach", "Harraseeket-South Freeport",
      "Upper Harraseeket", "Knightville Landing"
    ),
    "Monitoring Location Latitude" = c(
      43.676295, 43.671281, 43.820075, 43.842034, 43.6398783
    ),
    "Monitoring Location Longitude" = c(
      -70.250053, -70.241942, -70.104023, -70.095875, -70.2547617
    ),
    "Location Group" = "Seasonal (Surface)",
    check.names = FALSE
  )

  expect_equal(
    suppressMessages(
      format_sites(tst$me_focb_sites, "ME_FOCB", "MassWateR")
    ),
    df_mwr
  )
})

# Massachusetts ----
test_that("format_sites converts MA_BRC to MassWateR", {
  df_mwr <- data.frame(
    "Monitoring Location ID" = c(
      "A-06-01-010", "A-06-01-020", "B-01-01-020", "B-01-01-010", "A-01-01-010"
    ),
    "Monitoring Location Name" = c(
      "Esta Brook", "Shore Dr.",
      "Intersection of 146A and Balm of Life Spring Road", "South St.",
      "Jewish Community Center"
    ),
    "Monitoring Location Latitude" = c(
      42.31425, 42.304617, 42.02896, 42.01908, 42.29549
    ),
    "Monitoring Location Longitude" = c(
      -71.82114, -71.81665, -71.60853, -71.61399, -71.83817
    ),
    "Location Group" = c(
      "Headwaters", "Headwaters", "Mid-Reach", "Mid-Reach", "Headwaters"
    ),
    check.names = FALSE
  )

  expect_equal(
    suppressMessages(
      format_sites(tst$ma_brc_sites, "MA_BRC", "MassWateR")
    ),
    df_mwr
  )
})

test_that("format_sites converts MA_BRC to wqdashboard", {
  df_wqd <- data.frame(
    Site_ID = c(
      "A-06-01-010", "A-06-01-020", "B-01-01-020", "B-01-01-010", "A-01-01-010"
    ),
    Site_Name = c(
      "Esta Brook", "Shore Dr.",
      "Intersection of 146A and Balm of Life Spring Road", "South St.",
      "Jewish Community Center"
    ),
    Latitude = c(42.31425, 42.304617, 42.02896, 42.01908, 42.29549),
    Longitude = c(-71.82114, -71.81665, -71.60853, -71.61399, -71.83817),
    Town = c("Worcester", "Worcester", "Uxbridge", "Uxbridge", "Worcester"),
    State = "MA",
    Watershed = c(
      "Tatnuck Brook-Blackstone River", "Tatnuck Brook-Blackstone River",
      "Emerson Brook-Blackstone River", "Emerson Brook-Blackstone River",
      "Tatnuck Brook-Blackstone River"
    ),
    Group = c("Warmwater", "Coldwater", "Coldwater", "Coldwater", "Warmwater"),
    Max_Surface_Depth_m = NA,
    Max_Midwater_Depth_m = NA,
    Max_Depth_m = NA_integer_
  )

  expect_equal(
    suppressMessages(
      format_sites(tst$ma_brc_sites, "MA_BRC", "wqdashboard")
    ),
    df_wqd
  )
})

# Rhode Island ----
test_that("format_sites converts RI_WW to MassWateR", {
  df_mwr <- data.frame(
    "Monitoring Location ID" = c("WW260", "WW261", "WW262", "WW263", "WW265"),
    "Monitoring Location Name" = c(
      "Green Hill Pond - In Pond (center)", "Green Hill Pond - Indigo Point",
      "Green Hill Pond - Sea Lea (Allen Cove)", "Green Hill Pond - Teal Road",
      "Ninigret Pond - Crawford Dock"
    ),
    "Monitoring Location Latitude" = c(
      41.370716, 41.374138, 41.369175, 41.37732, 41.347473
    ),
    "Monitoring Location Longitude" = c(
      -71.615082, -71.618774, -71.625053, -71.609808, -71.697365
    ),
    "Location Group" = NA,
    check.names = FALSE
  )

  expect_equal(
    suppressMessages(
      format_sites(tst$ri_ww_sites, "RI_WW", "MassWateR")
    ),
    df_mwr
  )
})

test_that("format_sites converts RI_WW to wqdashboard", {
  df_wqd <- data.frame(
    Site_ID = c("WW260", "WW261", "WW262", "WW263", "WW265"),
    Site_Name = c(
      "Green Hill Pond - In Pond (center)", "Green Hill Pond - Indigo Point",
      "Green Hill Pond - Sea Lea (Allen Cove)", "Green Hill Pond - Teal Road",
      "Ninigret Pond - Crawford Dock"
    ),
    Latitude = c(41.370716, 41.374138, 41.369175, 41.37732, 41.347473),
    Longitude = c(-71.615082, -71.618774, -71.625053, -71.609808, -71.697365),
    Town = c(
      "South Kingstown", "South Kingstown", "Charlestown", "South Kingstown",
      "Charlestown"
    ),
    State = "RI",
    Watershed = c(
      "Point Judith Harbor of Refuge-Block Island Sound",
      "Point Judith Harbor of Refuge-Block Island Sound",
      "Ninigret Pond-Frontal Block Island Sound",
      "Point Judith Harbor of Refuge-Block Island Sound",
      "Ninigret Pond-Frontal Block Island Sound"
    ),
    Group = NA_character_,
    Max_Surface_Depth_m = NA,
    Max_Midwater_Depth_m = NA,
    Max_Depth_m = NA
  )

  expect_equal(
    suppressMessages(
      format_sites(tst$ri_ww_sites, "RI_WW", "WQdashboard")
    ),
    df_wqd
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
    State = NA_character_,
    Watershed = NA,
    Group = NA_character_,
    Max_Surface_Depth_m = NA,
    Max_Midwater_Depth_m = NA,
    Max_Depth_m = NA,
    "Location Group" = c("Lower Assabet", "Upper Assabet", NA),
    check.names = FALSE
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
    State = c("UT", "SC", "WV", "CO", "ID", "UT", "UT", "UT", "UT"),
    Watershed = NA,
    Group = NA_character_,
    Max_Surface_Depth_m = NA,
    Max_Midwater_Depth_m = NA,
    Max_Depth_m = NA,
    Location_Type = c(
      "Spring", "River/Stream", "River/Stream", "Spring", "River/Stream",
      "River/Stream", "River/Stream", "River/Stream", "River/Stream"
    ),
    "Tribal Land Indicator (Yes/No)" = "No",
    "Tribal Land Name" = NA,
    "Monitoring Location Source Map Scale" = c(
      24000, NA, NA, 12000, NA, NA, NA, NA, NA
    ),
    "Monitoring Location Horizontal Collection Method" = c(
      "Interpolation-Map", "GPS-Unspecified", "GPS-Unspecified",
      "Interpolation-Map", "GPS-Unspecified", "GPS-Unspecified",
      "GPS-Unspecified", "GPS-Unspecified", "GPS-Unspecified"
    ),
    "Monitoring Location Horizontal Coordinate Reference System" = c(
      "NAD27", "NAD83", "NAD83", "NAD27", "NAD83", "NAD83", "NAD83", "NAD83",
      "NAD83"
    ),
    "Monitoring Location County Name" = c(
      "Salt Lake", "Berkeley", "Berkeley", "Custer", "Custer", "Salt Lake",
      "Salt Lake", "Salt Lake", "Salt Lake"
    ),
    "County Code (Auto-Generated)" = c(35, 15, 3, 27, 37, 35, 35, 35, 35),
    "HUC 8" = NA,
    "HUC 12" = NA,
    check.names = FALSE
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
    Site_ID = c("RI01", "MA01", "ME01"),
    Site_Name = c("NBEP", "MassBays", "Casco Bay"),
    Latitude = c(41.82897, 42.31481, 43.66218),
    Longitude = c(-71.41924, -71.03931, -70.27354),
    State = c("Rhode Island", "Massachusetts", "Maine")
  )

  df_wqx <- suppressMessages(
    format_sites(df_wqd, "wqdashboard", "wqx")
  )

  expect_equal(
    df_wqx[["State Code"]],
    c("RI", "MA", "ME")
  )
})

test_that("format_sites to MA_BRC updates CFR, WATER_DEPTH_FT", {
  df_wqd <- data.frame(
    Site_ID = c("RI01", "MA01", "ME01"),
    Site_Name = c("NBEP", "MassBays", "Casco Bay"),
    Latitude = c(41.82897, 42.31481, 43.66218),
    Longitude = c(-71.41924, -71.03931, -70.27354),
    Group = c("Warmwater", "Warmwater", "Coldwater"),
    Max_Depth_m = c(NA, 0.304799990246, 1)
  )

  df_brc <- suppressMessages(
    format_sites(df_wqd, "wqdashboard", "ma_brc")
  )

  expect_equal(
    df_brc$CFR,
    c("No", "No", "Yes")
  )

  expect_equal(
    df_brc$WATER_DEPTH_FT,
    c(NA, 1, 3.28084)
  )
})

# Error messages -----

test_that("format_sites error messages", {
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
})
