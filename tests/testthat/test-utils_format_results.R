# MassWateR -----
test_that("prep_MassWateR_results works", {
  df_in <- data.frame(
    "Activity Type" = c(
      "Field Msr/Obs",
      "Quality Control-Meter Lab Duplicate",
      "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Duplicate"
    ),
    "Activity Start Date" = c(
      "2024-05-01", "2024-05-02", "2024-05-02", "2024-05-04"
    ),
    "Activity Start Time" = c("9:00", "8:00", "9:00", "10:00"),
    "Characteristic Name" = c("TDN", "TDP", "TDN", "TDP"),
    "Result Value" = c("12", "BDL", "AQL", "15"),
    "Result Measure Qualifier" = c(NA, NA, NA, "Q"),
    "QC Reference Value" = c(11, 6, NA, 16),
    check.names = FALSE
  )
  df_in[["Activity Start Date"]] <- as.Date(df_in[["Activity Start Date"]])

  df_out <- data.frame(
    "Activity Type" = c(
      "Field Msr/Obs",
      "Quality Control Field Replicate Msr/Obs",
      "Quality Control-Meter Lab Duplicate",
      "Quality Control-Meter Lab Duplicate 2",
      "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Duplicate 2"
    ),
    "Activity Start Date" = c(
      "2024-05-01", "2024-05-01", "2024-05-02", "2024-05-02", "2024-05-02",
      "2024-05-04", "2024-05-04"
    ),
    "Activity Start Time" = c(
      "9:00", "9:00", "8:00", "8:00", "9:00", "10:00", "10:00"
    ),
    "Characteristic Name" = c("TDN", "TDN", "TDP", "TDP", "TDN", "TDP", "TDP"),
    "Result Value" = c(12, 11, NA, 6, NA, 15, 16),
    "Result Measure Qualifier" = c(NA, NA, "DL", NA, "GT", "Q", "Q"),
    check.names = FALSE
  )
  df_out[["Activity Start Date"]] <- as.Date(df_out[["Activity Start Date"]])

  expect_equal(prep_MassWateR_results(df_in), df_out)
})

test_that("results_to_MassWateR works", {
  df_in <- data.frame(
    "Monitoring Location ID" = "foo",
    "Activity Type" = c(
      "Sample-Routine",
      "Quality Control Field Replicate Msr/Obs",
      "Field Msr/Obs",
      "Field Msr/Obs",
      "Field Msr/Obs",
      "Field Msr/Obs"
    ),
    "Activity Start Date" = as.Date("2025-08-13"),
    "Activity Start Time" = c("8:00", "8:00", "8:00", "9:00", "9:00", "9:00"),
    "Characteristic Name" = c("TN", "TP", "TP", "TN", "TN", "TN"),
    "Result Value" = c(1, 2, 3, 4, 5, 6),
    "Result Unit" = "mg/l",
    "QC Reference Value" = NA,
    "Result Measure Qualifier" = c(NA, "Q", "Q", "Q", NA, "Q"),
    "Local Record ID" = c(NA, "foofy", NA, NA, NA, "owl"),
    "Result Comment" = c(NA, "foo", "bar", NA, NA, NA),
    check.names = FALSE
  )

  df_out <- data.frame(
    "Monitoring Location ID" = "foo",
    "Activity Type" = c(
      "Sample-Routine",
      "Field Msr/Obs",
      "Field Msr/Obs",
      "Field Msr/Obs",
      "Field Msr/Obs"
    ),
    "Activity Start Date" = as.Date("2025-08-13"),
    "Activity Start Time" = c("8:00", "8:00", "9:00", "9:00", "9:00"),
    "Characteristic Name" = c("TN", "TP", "TN", "TN", "TN"),
    "Result Value" = c(1, 3, 4, 5, 6),
    "Result Unit" = "mg/l",
    "QC Reference Value" = c(NA, 2, NA, NA, NA),
    "Result Measure Qualifier" = c(NA, "Q", "Q", NA, "Q"),
    "Local Record ID" = c(NA, "foofy", NA, NA, "owl"),
    "Result Comment" = c(NA, "bar; foo", NA, NA, NA),
    check.names = FALSE
  )

  # Test conversion
  # - Test paired records correctly grouped, even if equivalent but not
  #   identical activity types
  # - Test groups of 3 dropped from group
  # - Test concatenation of select columns (Qualifier, Record ID, Comment)
  expect_equal(results_to_MassWateR(df_in), df_out)
})

# MA_BRC -----
test_that("prep_MA_BRC_results works", {
  df_in <- data.frame(
    "DATE_TIME" = c(
      "2024-02-04 12:56", "2024-02-05 15:25", "2024-02-06 7:24",
      "2024-02-07 17:30", "2024-03-06 18:20"
    ),
    "PARAMETER" = c(
      "Nitrate", "Nitrate Replicate", "E. coli", "E. coli Field Blank",
      "E. coli Lab Blank"
    )
  )

  df_out <- data.frame(
    "DATE_TIME" = c(
      "2024-02-04 12:56", "2024-02-05 15:25", "2024-02-06 07:24",
      "2024-02-07 17:30", "2024-03-06 18:20"
    ),
    "PARAMETER" = c("Nitrate", "Nitrate", "E. coli", "E. coli", "E. coli"),
    "DATE" = c(
      "2024-02-04", "2024-02-05", "2024-02-06", "2024-02-07", "2024-03-06"
    ),
    "TIME" = c("12:56", "15:25", "07:24", "17:30", "18:20"),
    "ACTIVITY_TYPE" = c(
      "Grab", "Replicate", "Grab", "Field Blank", "Lab Blank"
    ),
    "DEPTH_CATEGORY" = "Surface",
    "PROJECT_ID" = "BRC"
  )
  df_out[["DATE_TIME"]] <- as.POSIXct(
    df_out[["DATE_TIME"]],
    tz = "America/New_York"
  )
  df_out[["DATE"]] <- as.Date(df_out[["DATE"]])

  expect_equal(prep_MA_BRC_results(df_in), df_out)
})

test_that("results_to_MA_BRC works", {
  df <- data.frame(
    "SITE_BRC_CODE" = c(1, 2, 3, 4, 5),
    "PARAMETER" = c("Nitrate", "Nitrate", "E. coli", "E. coli", "E. coli"),
    "DATE" = c(
      "2024-02-04", "2024-02-05", "2024-02-06", "2024-02-07", "2024-03-06"
    ),
    "TIME" = c("12:56", "15:25", "07:24", "17:30", "18:20"),
    "ACTIVITY_TYPE" = c(
      "Grab", "Replicate", "Grab", "Field Blank", "Lab Blank"
    ),
    "DEPTH_CATEGORY" = "Surface",
    "PROJECT_ID" = "BRC"
  )

  df_out <- data.frame(
    "SITE_BRC_CODE" = c(1, 2, 3, 4, 5),
    "DATE_TIME" = c(
      "2024-02-04 12:56", "2024-02-05 15:25", "2024-02-06 07:24",
      "2024-02-07 17:30", "2024-03-06 18:20"
    ),
    "PARAMETER" = c(
      "Nitrate", "Nitrate Replicate", "E. coli", "E. coli Field Blank",
      "E. coli Lab Blank"
    ),
    "UNIQUE_ID" = c(
      "1_2024-02-04 12:56_NO3", "2_2024-02-05 15:25_NO3R",
      "3_2024-02-06 07:24_ECOL", "4_2024-02-07 17:30_ECOLFB",
      "5_2024-03-06 18:20_ECOLB"
    )
  )

  expect_equal(results_to_MA_BRC(df), df_out)
})

# ME_FOCB ----
test_that("prep_ME_FOCB_results works", {
  # Input formats - test data from ME_FOCB in 3 formats
  df_wide1 <- data.frame(
    "SiteID" = c("BMR02", "EEB18", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Time" = c("12:32", "12:45", "10:22"),
    "Cloud Cover_%" = c(50, 50, 50),
    "Wind Speed_BFT" = c(3, 3, 2),
    "Wind Direction_DEG True" = c(120, 150, 180),
    "Water Depth_m" = c(10.7, 3.2, NA),
    "Secchi_m" = c(1.9, "BSV", NA),
    check.names = FALSE
  )

  df_wide2 <- data.frame(
    "SiteID" = c("BMR02", "EEB18", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Time" = c("12:32", "12:45", "10:22"),
    "Sample Depth_m" = c(0.2, 0.2, 0),
    "Temperature_Deg C" = c(11.3, 11, 14),
    "Salinity_PSU" = c(27.5, 28, 28),
    "Dissolved Oxygen_mg/L" = c(9.3, 9.3, 8.2),
    "DO Saturation_%" = c(100.7, 100.7, 94.9),
    "pH" = c(7.93, 7.93, 7.82),
    "Chlorophyll_ug/L" = c(1.2, 1.2, 1.4),
    "Turbidity_FNU" = c(2.7, 1.4, 2.4),
    check.names = FALSE
  )

  df_long <- data.frame(
    "Site ID" = c("BMR02", "EEB18", "HR2"),
    "Sample Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "QC Type" = c("foo", "bar", NA),
    "Lab" = "UMWL",
    "Analysis Date" = c("07/06/23", "07/06/23", "06/07/23"),
    "Parameter" =
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
    "Result" = c(0.22, 0.18, 0.28),
    "Unit" = "MG/L",
    "RL" = 0.1,
    "MDL" = 0.73,
    "Method" = "SM4500NE_2021",
    "Sample Depth m" = 0.2,
    check.names = FALSE
  )

  # Expected outputs
  df_wide1_b <- data.frame(
    "SiteID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18", "HR2", "HR2", "HR2"
    ),
    "Sample Date" = c(
      "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23",
      "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/24/23", "05/24/23",
      "05/24/23"
    ),
    "Time" = c(
      "12:32", "12:32", "12:32", "12:32", "12:32", "12:45", "12:45", "12:45",
      "12:45", "12:45", "10:22", "10:22", "10:22"
    ),
    "Project" = "FRIENDS OF CASCO BAY ALL SITES",
    "Sampled By" = "FRIENDS OF CASCO BAY",
    "QC Type" = "Field Measurement",
    "Parameter" = c(
      "Cloud Cover", "Wind Speed", "Wind Direction", "Water Depth", "Secchi",
      "Cloud Cover", "Wind Speed", "Wind Direction", "Water Depth", "Secchi",
      "Cloud Cover", "Wind Speed", "Wind Direction"
    ),
    "Result" = c(
      "50", "3", "120", "10.7", "1.9", "50", "3", "150", "3.2", "BSV", "50",
      "2", "180"
    ),
    "Unit" = c(
      "%", "BFT", "DEG True", "m", "m", "%", "BFT", "DEG True", "m", "m", "%",
      "BFT", "DEG True"
    ),
    "Qualifier" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "G", NA, NA, NA),
    check.names = FALSE
  )

  df_wide2_b <- data.frame(
    "SiteID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18",
      "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "HR2", "HR2", "HR2",
      "HR2", "HR2", "HR2", "HR2"
    ),
    "Sample Date" = c(
      "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23",
      "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23",
      "05/23/23", "05/23/23", "05/24/23", "05/24/23", "05/24/23", "05/24/23",
      "05/24/23", "05/24/23", "05/24/23"
    ),
    "Time" = c(
      "12:32", "12:32", "12:32", "12:32", "12:32", "12:32", "12:32", "12:45",
      "12:45", "12:45", "12:45", "12:45", "12:45", "12:45", "10:22", "10:22",
      "10:22", "10:22", "10:22", "10:22", "10:22"
    ),
    "Sample Depth_m" = c(
      0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0,
      0, 0, 0, 0, 0, 0
    ),
    "Project" = "FRIENDS OF CASCO BAY ALL SITES",
    "Sampled By" = "FRIENDS OF CASCO BAY",
    "Sample Depth Unit" = "m",
    "QC Type" = "Field Measurement",
    "Parameter" = c(
      "Temperature", "Salinity", "Dissolved Oxygen", "DO Saturation", "pH",
      "Chlorophyll", "Turbidity", "Temperature", "Salinity", "Dissolved Oxygen",
      "DO Saturation", "pH", "Chlorophyll", "Turbidity", "Temperature",
      "Salinity", "Dissolved Oxygen", "DO Saturation", "pH", "Chlorophyll",
      "Turbidity"
    ),
    "Result" = c(
      11.3, 27.5, 9.3, 100.7, 7.93, 1.2, 2.7, 11, 28, 9.3, 100.7, 7.93, 1.2,
      1.4, 14, 28, 8.2, 94.9, 7.82, 1.4, 2.4
    ),
    "Unit" = c(
      "Deg C", "PSU", "mg/L", "%", "STU", "ug/L", "FNU", "Deg C", "PSU", "mg/L",
      "%", "STU", "ug/L", "FNU", "Deg C", "PSU", "mg/L", "%", "STU", "ug/L",
      "FNU"
    ),
    "Qualifier" = c(
      NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA,
      NA, "J", NA
    ),
    check.names = FALSE
  )

  df_long2 <- data.frame(
    "Site ID" = c("BMR02", "EEB18", "HR2"),
    "Sample Date" = c("2023-05-23", "2023-05-23", "2023-05-24"),
    "QC Type" = c("foo", "bar", "Field Measurement"),
    "Lab" = "UMWL",
    "Analysis Date" = c("2023-07-06", "2023-07-06", "2023-06-07"),
    "Parameter" = "TN as N",
    "Result" = c(0.22, 0.18, 0.28),
    "Unit" = "MG/L",
    "RL" = 0.1,
    "MDL" = 0.73,
    "Method" = "SM4500NE_2021",
    "Sample Depth m" = 0.2,
    "Project" = "FRIENDS OF CASCO BAY ALL SITES",
    "Sampled By" = "FRIENDS OF CASCO BAY",
    "Sample Depth Unit" = "m",
    "Qualifier" = c("J", "J", NA),
    check.names = FALSE
  )
  df_long2[["Sample Date"]] <- as.Date(df_long2[["Sample Date"]])
  df_long2[["Analysis Date"]] <- as.Date(df_long2[["Analysis Date"]])

  # Test
  expect_equal(prep_ME_FOCB_results(df_wide1), df_wide1_b)
  expect_equal(prep_ME_FOCB_results(df_wide2), df_wide2_b)
  expect_equal(prep_ME_FOCB_results(df_long), df_long2)
})
