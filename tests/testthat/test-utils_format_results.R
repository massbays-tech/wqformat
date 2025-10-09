# MassWateR -----
test_that("prep_mwr_results works", {
  # Test example data
  df_out <- data.frame(
    Monitoring.Location.ID = c("HBS-016", "HBS-016", "HBS-016", NA, NA, NA, NA),
    Activity.Type = c(
      "Field Msr/Obs", "Quality Control Field Replicate Msr/Obs",
      "Sample-Routine", "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Duplicate 2",
      "Quality Control-Calibration Check",
      "Quality Control-Calibration Check Buffer"
    ),
    Activity.Start.Date = c(
      "6/13/2021", "6/13/2021", "8/15/2021", "5/16/2021", "5/16/2021",
      "9/12/2021", "9/12/2021"
    ),
    Activity.Start.Time = c("8:00", "8:00", "7:40", NA, NA, NA, NA),
    Activity.Depth.Height.Measure = c(1, 1, 0.75, NA, NA, NA, NA),
    Activity.Depth.Height.Unit = c("ft", "ft", "ft", NA, NA, NA, NA),
    Activity.Relative.Depth.Name = NA,
    Characteristic.Name = c(
      "DO saturation", "DO saturation", "TSS", "Nitrate", "Nitrate",
      "Sp Conductance", "Sp Conductance"
    ),
    Result.Value = c(46.8, 7, 5, 0.45, 0.46, 980, 1000),
    Result.Unit = c("%", "%", "mg/l", "mg/l", "mg/l", "uS/cm", "uS/cm"),
    Quantitation.Limit = NA,
    Result.Measure.Qualifier = c(NA, NA, "Q", NA, NA, NA, NA),
    Result.Attribute = c(NA, NA, NA, "K16452-MB3", "K16452-MB3", NA, NA),
    Sample.Collection.Method.ID = c(NA, NA, "Grab-MassWateR", NA, NA, NA, NA),
    Project.ID = "Water Quality",
    Local.Record.ID = NA,
    Result.Comment = c(NA, NA, "River was very full", NA, NA, NA, NA)
  )

  expect_equal(
    prep_mwr_results(tst$mwr_data, name_repair = TRUE),
    df_out
  )

  # Additional test - BDL, AQL values
  df_in <- data.frame(
    "Monitoring Location ID" = "HBS-016",
    "Activity Type" = c("Field Msr/Obs", "Sample-Routine"),
    "Activity Start Date" = c("6/13/2021", "8/15/2021"),
    "Activity Start Time" = c("8:00", "7:40"),
    "Activity Depth/Height Measure" = c(1, 0.75),
    "Activity Depth/Height Unit" = "ft",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c("DO saturation", "TSS"),
    "Result Value" = c("BDL", "AQL"),
    "Result Unit" = c("%", "mg/l"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = NA,
    "Result Measure Qualifier" = NA,
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "Water Quality",
    "Local Record ID" = NA,
    "Result Comment" = NA,
    check.names = FALSE
  )

  df_out <- data.frame(
    "Monitoring Location ID" = "HBS-016",
    "Activity Type" = c("Field Msr/Obs", "Sample-Routine"),
    "Activity Start Date" = c("6/13/2021", "8/15/2021"),
    "Activity Start Time" = c("8:00", "7:40"),
    "Activity Depth/Height Measure" = c(1, 0.75),
    "Activity Depth/Height Unit" = "ft",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c("DO saturation", "TSS"),
    "Result Value" = NA_integer_,
    "Result Unit" = c("%", "mg/l"),
    "Quantitation Limit" = NA,
    "Result Measure Qualifier" = c("DL", "GT"),
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "Water Quality",
    "Local Record ID" = NA,
    "Result Comment" = NA,
    check.names = FALSE
  )

  expect_equal(
    prep_mwr_results(df_in),
    df_out
  )
})

test_that("results_to_mwr works", {
  # Test example data
  df_in <- data.frame(
    "Monitoring Location ID" = c(
      "HBS-016", "HBS-016", "HBS-016", NA, NA, NA, NA
    ),
    "Activity Type" = c(
      "Field Msr/Obs", "Quality Control Field Replicate Msr/Obs",
      "Sample-Routine", "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Duplicate 2",
      "Quality Control-Calibration Check",
      "Quality Control-Calibration Check Buffer"
    ),
    "Activity Start Date" = c(
      "2021-06-13", "2021-06-13", "2021-08-15", "2021-05-16", "2021-05-16",
      "2021-09-12", "2021-09-12"
    ),
    "Activity Start Time" = c("8:00", "8:00", "7:40", NA, NA, NA, NA),
    "Activity Depth/Height Measure" = c(1, 1, 0.75, NA, NA, NA, NA),
    "Activity Depth/Height Unit" = c("ft", "ft", "ft", NA, NA, NA, NA),
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "DO saturation", "DO saturation", "TSS", "Nitrate", "Nitrate",
      "Sp Conductance", "Sp Conductance"
    ),
    "Result Value" = c(46.8, 7, 5, 0.45, 0.46, 980, 1000),
    "Result Unit" = c("%", "%", "mg/l", "mg/l", "mg/l", "uS/cm", "uS/cm"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = NA,
    "Result Measure Qualifier" = c(NA, NA, "Q", NA, NA, NA, NA),
    "Result Attribute" = c(NA, NA, NA, "K16452-MB3", "K16452-MB3", NA, NA),
    "Sample Collection Method ID" = c(NA, NA, "Grab-MassWateR", NA, NA, NA, NA),
    "Project ID" = "Water Quality",
    "Local Record ID" = NA,
    "Result Comment" = c(NA, NA, "River was very full", NA, NA, NA, NA),
    check.names = FALSE
  )
  df_in[["Activity Start Date"]] <- as.Date(df_in[["Activity Start Date"]])

  df_out <- tst$mwr_data
  df_out$Activity.Start.Date <- as.Date(
    df_out$Activity.Start.Date,
    format = "%m/%d/%Y"
  )
  df_out$Local.Record.ID <- NA_character_
  df_out <- df_out[order(df_out$Activity.Start.Date), ]
  rownames(df_out) <- NULL
  colnames(df_out) <- gsub("\\.", " ", colnames(df_out))
  colnames(df_out) <- gsub("Depth Height", "Depth/Height", colnames(df_out))

  expect_equal(
    results_to_mwr(df_in),
    df_out
  )

  # Test edge cases
  # - Groups of 3 dropped NOT grouped
  # - Concatenate select columns (Qualifier, Record ID, Comment)

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

  expect_equal(results_to_mwr(df_in), df_out)
})

# WQX -----
test_that("prep_wqx_results works", {
  # Set var
  out_qual <- c(
    "DL", "DL", "DL", NA, NA, "DL", "DL", "DL", "H", "H", "H", "H", "DL",
    "DL", "BQL", "DL", "DL", "DL", "DL", "DL", "DL", "DL", "BQL", "GT", "DL",
    "DL", "DL", "DL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, "NRR", "NRR", "NRR", "NRR", NA, NA, NA, NA
  )

  # Test - with name repair
  df_wqx <- tst$wqx_data %>%
    prep_wqx_results(name_repair = TRUE)

  expect_equal(
    df_wqx$Result.Measure.Qualifier,
    out_qual
  )

  # Test - no name repair
  df_wqx <- tst$wqx_data
  colnames(df_wqx) <- gsub("\\.", " ", colnames(df_wqx))
  df_wqx <- prep_wqx_results(df_wqx)

  expect_equal(
    df_wqx[["Result Measure Qualifier"]],
    out_qual
  )
})

test_that("results_to_wqx works", {
  # Modify WQX sample data so it uses proper column names, has more qualifiers
  df_wqx <- tst$wqx_data
  colnames(df_wqx) <- gsub("\\.", " ", colnames(df_wqx))
  df_wqx[["Result Measure Qualifier"]] <- c(
    "DL", "DL", "DL", NA, NA, "DL", "DL", "DL", "H", "H", "H", "H", "DL",
    "DL", "2-5B", "DL", "DL", "DL", "DL", "DL", "DL", "DL", "D>T", "GT", "DL",
    "DL", "DL", "DL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, "NRR", "NRR", "NRR", "NRR", NA, NA, NA, NA
  )

  # Create sample input dataset with "Result Detection Condition" set to NA
  df_in <- df_wqx
  df_in[["Result Detection Condition"]] <- NA

  # Test - "Result Detection Condition" updates appropriately?
  expect_equal(results_to_wqx(df_in), df_wqx)
})

# MA_BRC -----
test_that("prep_brc_results works", {
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

  expect_equal(prep_brc_results(df_in), df_out)
})

test_that("results_to_brc works", {
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

  expect_equal(results_to_brc(df), df_out)
})

# ME_FOCB ----
test_that("prep_focb_results works", {
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
  expect_equal(prep_focb_results(df_wide1), df_wide1_b)
  expect_equal(prep_focb_results(df_wide2), df_wide2_b)
  expect_equal(prep_focb_results(df_long), df_long2)
})
