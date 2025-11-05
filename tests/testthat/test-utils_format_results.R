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

  # Additional test - BDL, AQL values; no name repair
  df_in <- tst$mwr_data
  df_in$Result.Value <- c("BDL", 5, "AQL", 980)
  df_in$QC.Reference.Value <- c(7, NA, 0.46, "BDL")
  colnames(df_in) <- gsub("\\.", " ", colnames(df_in))
  colnames(df_in) <- gsub("Depth Height", "Depth/Height", colnames(df_in))


  df_out <- df_out
  df_out$Result.Value <- c(NA, 7, 5, NA, 0.46, 980, NA)
  df_out$Result.Measure.Qualifier <- c("DL", NA, "Q", "GT", NA, NA, "DL")
  colnames(df_out) <- gsub("\\.", " ", colnames(df_out))
  colnames(df_out) <- gsub("Depth Height", "Depth/Height", colnames(df_out))

  expect_equal(
    prep_mwr_results(df_in),
    df_out
  )
})

test_that("add_qc_ref works", {
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

  df_out <- data.frame(
    "Monitoring Location ID" = c(
      NA, "HBS-016", "HBS-016", NA
    ),
    "Activity Start Date" = c(
      "2021-05-16", "2021-06-13", "2021-08-15", "2021-09-12"
    ),
    "Activity Start Time" = c(NA, "8:00", "7:40", NA),
    "Activity Depth/Height Measure" = c(NA, 1, 0.75, NA),
    "Activity Depth/Height Unit" = c(NA, "ft", "ft", NA),
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "Nitrate", "DO saturation", "TSS", "Sp Conductance"
    ),
    "Result Unit" = c("mg/l", "%", "mg/l", "uS/cm"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = c("0.46", "7", NA, "1000"),
    "Result Attribute" = c("K16452-MB3", NA, NA, NA),
    "Sample Collection Method ID" = c(NA, NA, "Grab-MassWateR", NA),
    "Project ID" = "Water Quality",
    "Activity Type" = c(
      "Quality Control Sample-Lab Duplicate", "Field Msr/Obs", "Sample-Routine",
      "Quality Control-Calibration Check"
    ),
    "Result Value" = c("0.45", "46.8", "5", "980"),
    "Result Measure Qualifier" = c(NA, NA, "Q", NA),
    "Local Record ID" = NA_character_,
    "Result Comment" = c(NA, NA, "River was very full", NA),
    check.names = FALSE
  )
  df_out[["Activity Start Date"]] <- as.Date(df_out[["Activity Start Date"]])

  expect_equal(
    add_qc_ref(df_in),
    df_out
  )

  # Edge case 1 - no duplicates
  df_single <- data.frame(
    "Monitoring Location ID" = "foo",
    "Activity Type" = "Quality Control Field Replicate Sample-Composite",
    "Activity Start Date" = as.Date("2025-08-13"),
    "Activity Start Time" = c("8:00", "9:00"),
    "Characteristic Name" = c("TN", "TP"),
    "Result Value" = c(1, 2),
    "Result Unit" = "mg/l",
    "QC Reference Value" = NA,
    "Result Measure Qualifier" = NA,
    "Local Record ID" = NA,
    "Result Comment" = NA,
    check.names = FALSE
  )

  expect_equal(
    add_qc_ref(df_single),
    df_single
  )

  # Edge case 2 - trio instead of duplicate
  df_trio <- data.frame(
    "Monitoring Location ID" = "foo",
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = as.Date("2025-08-13"),
    "Activity Start Time" = "9:00",
    "Characteristic Name" = "TN",
    "Result Value" = c(1, 2, 3),
    "Result Unit" = "mg/l",
    "QC Reference Value" = NA,
    "Result Measure Qualifier" = NA,
    "Local Record ID" = NA,
    "Result Comment" = NA,
    check.names = FALSE
  )

  expect_equal(
    suppressWarnings(
      add_qc_ref(df_trio)
    ),
    df_trio
  )
  expect_warning(
    add_qc_ref(df_trio),
    regexp = "More than two matching samples found"
  )

  # Edge case 3 - concatenate selected columns (Qualifier, Record ID, Comment)
  df_in <- data.frame(
    "Monitoring Location ID" = "foo",
    "Activity Type" = c(
      "Quality Control Field Replicate Msr/Obs",
      "Field Msr/Obs"
    ),
    "Activity Start Date" = as.Date("2025-08-13"),
    "Activity Start Time" = "8:00",
    "Characteristic Name" = "TP",
    "Result Value" = c(1, 2),
    "Result Unit" = "mg/l",
    "QC Reference Value" = NA,
    "Result Measure Qualifier" = "Q",
    "Local Record ID" = c(NA, "foofy"),
    "Result Comment" = c("foo", "bar"),
    check.names = FALSE
  )

  df_out <- data.frame(
    "Monitoring Location ID" = "foo",
    "Activity Start Date" = as.Date("2025-08-13"),
    "Activity Start Time" = "8:00",
    "Characteristic Name" = "TP",
    "Result Unit" = "mg/l",
    "QC Reference Value" = "1",
    "Activity Type" = "Field Msr/Obs",
    "Result Value" = "2",
    "Result Measure Qualifier" = "Q",
    "Local Record ID" = "foofy",
    "Result Comment" = "bar; foo",
    check.names = FALSE
  )

  expect_equal(
    suppressWarnings(
      add_qc_ref(df_in)
    ),
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

  # Test edge case - BDL, AQL handling
  df_in <- df_in
  df_in[["Result Value"]] <- c(NA, 7, 5, NA, 0.46, 980, NA)
  df_in[["Result Measure Qualifier"]] <- c("DL", NA, "Q", "GT", NA, NA, "DL")

  df_out <- df_out
  df_out[["Result Value"]] <- c("AQL", "BDL", 5, 980)
  df_out[["QC Reference Value"]] <- c(0.46, 7, NA, "BDL")

  expect_equal(
    results_to_mwr(df_in),
    df_out
  )
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
  df_out <- data.frame(
    "ID" = c(96, 97, 98, 99, 100, 101, 102, 103, 104),
    "SEID" = 14,
    "SITE_BRC_CODE" = "B-06-01-050",
    "DATE_TIME" = as.POSIXct("2004-04-10 04:30", tz = "America/New_York"),
    "PARAMETER" = c(
      "Air Temperature", "Dissolved Oxy Saturation", "Dissolved Oxygen",
      "Nitrate", "Nitrate", "Orthophosphate", "Orthophosphate", "Turbidity",
      "Water Temperature"
    ),
    "RESULT" = c(7, 65, 7.8, 0.4, 0.4, 0.38, 0.36, 0.55, 7),
    "UNITS" = c("C", "%", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "NTU", "C"),
    "UNIQUE_ID" = c(
      "B-06-01-050_2004-04-10 04:30_TAC", "B-06-01-050_2004-04-10 04:30_OXYSAT",
      "B-06-01-050_2004-04-10 04:30_DOXY", "B-06-01-050_2004-04-10 04:30_NO3",
      "B-06-01-050_2004-04-10 04:30_NO3R", "B-06-01-050_2004-04-10 04:30_PO4",
      "B-06-01-050_2004-04-10 04:30_PO4R", "B-06-01-050_2004-04-10 04:30_TURB1",
      "B-06-01-050_2004-04-10 04:30_TWC"
    ),
    "DATE" = as.Date("2004-04-10"),
    "TIME" = "04:30",
    "ACTIVITY_TYPE" = c(
      "Grab", "Grab", "Grab", "Grab", "Replicate", "Grab", "Replicate", "Grab",
      "Grab"
    ),
    "DEPTH_CATEGORY" = "Surface",
    "PROJECT_ID" = "BRC"
  )

  expect_equal(prep_brc_results(tst$ma_brc_data), df_out)
})

test_that("results_to_brc works", {
  df_in <- data.frame(
    "ID" = c(96, 97, 98, 99, 100, 101, 102, 103, 104),
    "SEID" = 14,
    "SITE_BRC_CODE" = "B-06-01-050",
    "PARAMETER" = c(
      "Air Temperature", "Dissolved Oxy Saturation", "Dissolved Oxygen",
      "Nitrate", "Nitrate", "Orthophosphate", "Orthophosphate", "Turbidity",
      "Water Temperature"
    ),
    "RESULT" = c(7, 65, 7.8, 0.4, 0.4, 0.38, 0.36, 0.55, 7),
    "UNITS" = c("C", "%", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "NTU", "C"),
    "DATE" = as.Date("2004-04-10"),
    "TIME" = "04:30",
    "ACTIVITY_TYPE" = c(
      "Grab", "Grab", "Grab", "Grab", "Replicate", "Grab", "Replicate", "Grab",
      "Grab"
    ),
    "DEPTH_CATEGORY" = "Surface",
    "PROJECT_ID" = "BRC"
  )

  expect_equal(results_to_brc(df_in), tst$ma_brc_data)
})

# ME_FOCB ----
test_that("prep_focb_results works", {
  # Test - wide format 1
  df_out1 <- data.frame(
    "SiteID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18", "HR4", "HR4", "HR4"
    ),
    "Sample.Date" = c(
      "10/4/2023", "10/4/2023", "10/4/2023", "10/4/2023", "10/4/2023",
      "6/22/2023", "6/22/2023", "6/22/2023", "6/22/2023", "6/22/2023",
      "5/24/2023", "5/24/2023", "5/24/2023"
    ),
    "Time" = c(
      "12:25", "12:25", "12:25", "12:25", "12:25", "8:45", "8:45", "8:45",
      "8:45", "8:45", "13:51", "13:51", "13:51"
    ),
    "Project" = "FRIENDS OF CASCO BAY ALL SITES",
    "Sampled.By" = "FRIENDS OF CASCO BAY",
    "QC.Type" = "Field Measurement",
    "Parameter" = c(
      "Cloud Cover", "Wind Speed", "Wind Direction", "Water Depth", "Secchi",
      "Cloud Cover", "Wind Speed", "Wind Direction", "Water Depth", "Secchi",
      "Cloud Cover", "Wind Speed", "Wind Direction"
    ),
    "Result" = c(
      25, 1, 160, 9.5, 2.7, 0, 1, 45, 0.8, "BSV", 75, 3, 180
    ),
    "Unit" = c(
      "%", "BFT", "DEG True", "m", "m", "%", "BFT", "DEG True", "m", "m", "%",
      "BFT", "DEG True"
    ),
    "Qualifier" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "G", NA, NA, NA)
  )

  expect_equal(
    prep_focb_results(tst$me_focb_data1, name_repair = TRUE),
    df_out1
  )

  # Test - wide format 2
  df_out2 <- data.frame(
    "SiteID" = "P5BSD",
    "Sample.Date" = "7/19/2023",
    "Time" = c(
      "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:04",
      "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:05", "10:05",
      "10:05", "10:05", "10:05", "10:05", "10:05"
    ),
    "Sample.Depth_m" = c(
      11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 5, 5, 5, 5, 5, 5,
      5
    ),
    "Project" = "FRIENDS OF CASCO BAY ALL SITES",
    "Sampled.By" = "FRIENDS OF CASCO BAY",
    "Sample.Depth.Unit" = "m",
    "QC.Type" = "Field Measurement",
    "Parameter" = c(
      "Temperature", "Salinity", "Dissolved Oxygen", "DO Saturation", "pH",
      "Chlorophyll", "Turbidity", "Temperature", "Salinity", "Dissolved Oxygen",
      "DO Saturation", "pH", "Chlorophyll", "Turbidity", "Temperature",
      "Salinity", "Dissolved Oxygen", "DO Saturation", "pH", "Chlorophyll",
      "Turbidity"
    ),
    "Result" = c(
      12.7, 31, 7.3, 83.7, 7.93, 2.3, 1.1, 12.7, 31, 7.3, 83.4, 7.93, 2.5, 1.1,
      15.5, 30.1, 7.6, 91.3, 7.98, 7.8, 1.1
    ),
    "Unit" = c(
      "Deg C", "PSU", "mg/L", "%", "STU", "ug/L", "FNU", "Deg C", "PSU", "mg/L",
      "%", "STU", "ug/L", "FNU", "Deg C", "PSU", "mg/L", "%", "STU", "ug/L",
      "FNU"
    ),
    "Qualifier" = c(
      NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA,
      NA, "J", NA
    )
  )

  expect_equal(
    prep_focb_results(tst$me_focb_data2, name_repair = TRUE),
    df_out2
  )

  # Test - long format
  df_out3 <- data.frame(
    "Sample.ID" = "BMR02",
    "Sample.Date" = c(
      "2023-05-23", "2023-06-21", "2023-07-05", "2023-07-18", "2023-08-16"
    ),
    "Lab" = "UMWL",
    "Analysis.Date" = c(
      "2023-07-06", "2023-07-27", "2023-08-28", "2023-09-25", "2023-10-20"
    ),
    "Parameter" = "TN as N",
    "Result" = c(0.22, 0.34, 0.28, 0.28, 0.25),
    "Unit" = "MG/L",
    "RL" = 0.1,
    "MDL" = 0.073,
    "Method" = "SM4500NE_2021",
    "Sample.Depth.m" = 0.2,
    "Project" = "FRIENDS OF CASCO BAY ALL SITES",
    "Sampled.By" = "FRIENDS OF CASCO BAY",
    "Sample.Depth.Unit" = "m",
    "QC.Type" = "Field Measurement",
    "Qualifier" = "J"
  )
  df_out3$Sample.Date <- as.Date(df_out3$Sample.Date)
  df_out3$Analysis.Date <- as.Date(df_out3$Analysis.Date)

  expect_equal(
    prep_focb_results(tst$me_focb_data3, name_repair = TRUE),
    df_out3
  )

  # Test - edge case ("QC Type" column blank but present)
  df_in3b <- tst$me_focb_data3
  df_in3b$QC.Type <- NA

  df_out3b <- df_out3[, c(1:11, 15, 12:14, 16)]

  expect_equal(
    prep_focb_results(df_in3b, name_repair = TRUE),
    df_out3b
  )
})

# ME_FOCB ----
test_that("prep_me_dep works", {
  df_out <- tst$me_dep_data
  df_out$QC_TYPE <- "NOT APPLICABLE (NORMAL ENVIRONMENTAL SAMPLE)"
  df_out$SAMPLE_COMMENTS <- NA_character_
  df_out <- df_out[, c(1:18, 26, 19:25, 27)]

  expect_equal(
    prep_me_dep_results(tst$me_dep_data),
    df_out
  )

  # Test edge case
  df_in <- tst$me_dep_data
  df_in$LAB_COMMENT <- c("foo", NA, "piping", NA, NA)
  df_in$SAMPLE_COMMENTS <- c("bar", "superb", "plover", NA, NA)
  df_in$VALIDATION_COMMENT <- c("foofy", "owl", NA, NA, NA)

  df_out$LAB_COMMENT <- df_in$LAB_COMMENT
  df_out$SAMPLE_COMMENTS <- c(
    "foo; bar; foofy", "superb; owl", "piping; plover", NA, NA
  )
  df_out$VALIDATION_COMMENT <- df_in$VALIDATION_COMMENT

  expect_equal(
    prep_me_dep_results(df_in),
    df_out
  )
})
