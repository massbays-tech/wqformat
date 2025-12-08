# Test MAINE ----
test_that("format_results converts ME_FOCB to ME_DEP", {
  # Set var
  col_order <- c(
    "PROJECT/SITE", "SAMPLE_POINT_NAME", "LAB_SAMPLE_ID", "SAMPLE_ID",
    "ANALYSIS_LAB", "SAMPLE_DATE", "SAMPLE_TIME", "SAMPLE_TYPE", "QC_TYPE",
    "PARAMETER_NAME", "CONCENTRATION", "LAB_QUALIFIER", "REPORTING_LIMIT",
    "PARAMETER_UNITS", "TEST", "METER_ID", "ANALYSIS_DATE", "ANALYSIS_TIME",
    "MDL", "RESULT_TYPE_CODE", "LAB_COMMENT", "SAMPLE_DEPTH",
    "SAMPLE_DEPTH_UNIT", "SAMPLE_COLLECTION_METHOD", "SAMPLE_LOCATION",
    "TREATMENT_STATUS", "PARAMETER_FILTERED", "SAMPLED_BY", "SAMPLE_COMMENTS",
    "BATCH_ID", "PREP_DATE", "SAMPLE_DELIVERY_GROUP", "PARAMETER_QUALIFIER",
    "VALIDATION_QUALIFIER", "VALIDATION_LEVEL", "VALIDATION_COMMENT",
    "VALIDATION_COMMENT_TYPE"
  )

  # Test - FOCB wide format 1
  df_dep1 <- data.frame(
    "PROJECT/SITE" = "FRIENDS OF CASCO BAY ALL SITES",
    SAMPLE_POINT_NAME = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18", "HR4", "HR4", "HR4"
    ),
    SAMPLE_DATE = c(
      "2023-10-04", "2023-10-04", "2023-10-04", "2023-10-04", "2023-10-04",
      "2023-06-22", "2023-06-22", "2023-06-22", "2023-06-22", "2023-06-22",
      "2023-05-24", "2023-05-24", "2023-05-24"
    ),
    SAMPLE_TIME = c(
      "12:25", "12:25", "12:25", "12:25", "12:25", "8:45", "8:45", "8:45",
      "8:45", "8:45", "13:51", "13:51", "13:51"
    ),
    QC_TYPE = "NA",
    PARAMETER_NAME = c(
      "CLOUD COVER", "WSPD", "WDIR", "DEPTH", "SECCHI", "CLOUD COVER", "WSPD",
      "WDIR", "DEPTH", "SECCHI", "CLOUD COVER", "WSPD", "WDIR"
    ),
    CONCENTRATION = c(
      25, 1, 160, 9.5, 2.7, 0, 1, 45, 0.8, "BSV", 75, 3, 180
    ),
    LAB_QUALIFIER = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "G", NA, NA, NA),
    PARAMETER_UNITS = c(
      "%", "BFT", "DEG TRUE", "M", "M", "%", "BFT", "DEG TRUE", "M", "M", "%",
      "BFT", "DEG TRUE"
    ),
    ANALYSIS_DATE = as.Date(NA),
    SAMPLED_BY = "FRIENDS OF CASCO BAY",
    check.names = FALSE
  )
  df_dep1$SAMPLE_DATE <- as.Date(df_dep1$SAMPLE_DATE)
  missing_col <- setdiff(col_order, colnames(df_dep1))
  df_dep1[missing_col] <- NA
  df_dep1 <- df_dep1[, col_order]

  expect_equal(
    suppressMessages(
      format_results(tst$me_focb_data1, "ME_FOCB", "ME_DEP")
    ),
    df_dep1
  )

  # Test - FOCB wide format 2
  df_dep2 <- data.frame(
    "PROJECT/SITE" = "FRIENDS OF CASCO BAY ALL SITES",
    SAMPLE_POINT_NAME = "P5BSD",
    SAMPLE_DATE = as.Date("2023-07-19"),
    SAMPLE_TIME = c(
      "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:04",
      "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:05", "10:05",
      "10:05", "10:05", "10:05", "10:05", "10:05"
    ),
    QC_TYPE = "NA",
    SAMPLE_DEPTH = c(
      11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 5, 5, 5, 5, 5, 5,
      5
    ),
    SAMPLE_DEPTH_UNIT = "M",
    PARAMETER_NAME = c(
      "TEMP", "SALINITY", "DO", "DO SAT", "PH", "CHLOROPHYLL", "TUR", "TEMP",
      "SALINITY", "DO", "DO SAT", "PH", "CHLOROPHYLL", "TUR", "TEMP",
      "SALINITY", "DO", "DO SAT", "PH", "CHLOROPHYLL", "TUR"
    ),
    CONCENTRATION = c(
      12.7, 31, 7.3, 83.7, 7.93, 2.3, 1.1, 12.7, 31, 7.3, 83.4, 7.93, 2.5, 1.1,
      15.5, 30.1, 7.6, 91.3, 7.98, 7.8, 1.1
    ),
    PARAMETER_UNITS = c(
      "DEG C", "PSU", "MG/L", "%", "STU", "UG/L", "FNU", "DEG C", "PSU", "MG/L",
      "%", "STU", "UG/L", "FNU", "DEG C", "PSU", "MG/L", "%", "STU", "UG/L",
      "FNU"
    ),
    LAB_QUALIFIER = c(
      NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA,
      NA, "J", NA
    ),
    ANALYSIS_DATE = as.Date(NA),
    SAMPLED_BY = "FRIENDS OF CASCO BAY",
    check.names = FALSE
  )
  missing_col <- setdiff(col_order, colnames(df_dep2))
  df_dep2[missing_col] <- NA
  df_dep2 <- df_dep2[, col_order]

  expect_equal(
    suppressMessages(
      format_results(tst$me_focb_data2, "ME_FOCB", "ME_DEP")
    ),
    df_dep2
  )

  # Test - FOCB long format
  df_dep3 <- data.frame(
    "PROJECT/SITE" = "FRIENDS OF CASCO BAY ALL SITES",
    SAMPLE_POINT_NAME = "BMR02",
    SAMPLE_DATE = c(
      "2023-05-23", "2023-06-21", "2023-07-05", "2023-07-18", "2023-08-16"
    ),
    QC_TYPE = "NA",
    ANALYSIS_LAB = "UMWL",
    ANALYSIS_DATE = c(
      "2023-07-06", "2023-07-27", "2023-08-28", "2023-09-25", "2023-10-20"
    ),
    PARAMETER_NAME = "TN AS N",
    CONCENTRATION = c(0.22, 0.34, 0.28, 0.28, 0.25),
    PARAMETER_UNITS = "MG/L",
    REPORTING_LIMIT = 0.1,
    MDL = 0.073,
    TEST = "SM4500NE_2021",
    SAMPLE_DEPTH = 0.2,
    SAMPLE_DEPTH_UNIT = "M",
    LAB_QUALIFIER = "J",
    SAMPLED_BY = "FRIENDS OF CASCO BAY",
    check.names = FALSE
  )
  df_dep3$SAMPLE_DATE <- as.Date(df_dep3$SAMPLE_DATE)
  df_dep3$ANALYSIS_DATE <- as.Date(df_dep3$ANALYSIS_DATE)
  missing_col <- setdiff(col_order, colnames(df_dep3))
  df_dep3[missing_col] <- NA
  df_dep3 <- df_dep3[, col_order]

  expect_equal(
    suppressMessages(
      format_results(tst$me_focb_data3, "ME_FOCB", "ME_DEP")
    ),
    df_dep3
  )
})

test_that("format_results converts ME_FOCB to MassWateR", {
  # Test - FOCB wide format 1
  df_mwr1 <- data.frame(
    "Monitoring Location ID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18", "HR4", "HR4", "HR4"
    ),
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = c(
      "2023-10-04", "2023-10-04", "2023-10-04", "2023-10-04", "2023-10-04",
      "2023-06-22", "2023-06-22", "2023-06-22", "2023-06-22", "2023-06-22",
      "2023-05-24", "2023-05-24", "2023-05-24"
    ),
    "Activity Start Time" = c(
      "12:25", "12:25", "12:25", "12:25", "12:25", "8:45", "8:45", "8:45",
      "8:45", "8:45", "13:51", "13:51", "13:51"
    ),
    "Activity Depth/Height Measure" = NA,
    "Activity Depth/Height Unit" = NA,
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "Cloud Cover", "Wind Speed", "Wind Direction", "Depth", "Secchi Depth",
      "Cloud Cover", "Wind Speed", "Wind Direction", "Depth", "Secchi Depth",
      "Cloud Cover", "Wind Speed", "Wind Direction"
    ),
    "Result Value" = c(
      25, 1, 160, 9.5, 2.7, 0, 1, 45, 0.8, "AQL", 75, 3, 180
    ),
    "Result Unit" = c(
      "%", "BFT", "DEG True", "m", "m", "%", "BFT", "DEG True", "m", "m", "%",
      "BFT", "DEG True"
    ),
    "Quantitation Limit" = NA,
    "QC Reference Value" = NA_integer_,
    "Result Measure Qualifier" = NA_character_,
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA,
    "Result Comment" = NA,
    check.names = FALSE
  )
  df_mwr1[["Activity Start Date"]] <- as.Date(df_mwr1[["Activity Start Date"]])

  expect_warning(
    expect_warning(
      expect_warning(
        suppressMessages(
          format_results(tst$me_focb_data1, "ME_FOCB", "MassWateR")
        ),
        paste(
          "Invalid variables in Characteristic Name:",
          "Cloud Cover, Wind Speed, Wind Direction"
        )
      ),
      "Invalid variables in Result Unit: BFT, DEG True"
    ),
    "Invalid variables in Detection Limit Unit: BFT, DEG True"
  )

  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_results(tst$me_focb_data1, "ME_FOCB", "MassWateR")
      )
    ),
    df_mwr1
  )

  # Test - FOCB wide format 2
  df_mwr2 <- data.frame(
    "Monitoring Location ID" = "P5BSD",
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = as.Date("2023-07-19"),
    "Activity Start Time" = c(
      "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:04", "10:05",
      "10:05", "10:05", "10:05", "10:05", "10:05", "10:05"
    ),
    "Activity Depth/Height Measure" = c(
      11, 11, 11, 11, 11, 11, 11, 5, 5, 5, 5, 5, 5, 5
    ),
    "Activity Depth/Height Unit" = "m",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "Chl a", "DO", "DO saturation", "Salinity", "Turbidity", "Water Temp",
      "pH", "Water Temp", "Salinity", "DO", "DO saturation", "pH",
      "Chl a", "Turbidity"
    ),
    "Result Value" = c(
      2.3, 7.3, 83.7, 31, 1.1, 12.7, 7.93, 15.5, 30.1, 7.6, 91.3, 7.98, 7.8,
      1.1
    ),
    "Result Unit" = c(
      "ug/l", "mg/l", "%", "PSU", "FNU", "deg C", "None", "deg C", "PSU",
      "mg/l", "%", "None", "ug/l", "FNU"
    ),
    "Quantitation Limit" = NA,
    "QC Reference Value" = c(
      2.5, 7.3, 83.4, 31, 1.1, 12.7, 7.93, NA, NA, NA, NA, NA, NA, NA
    ),
    "Result Measure Qualifier" = c(
      "J", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "J", NA
    ),
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA_character_,
    "Result Comment" = NA_character_,
    check.names = FALSE
  )
  df_mwr2[["Activity Start Date"]] <- as.Date(df_mwr2[["Activity Start Date"]])

  expect_equal(
    suppressMessages(
      format_results(tst$me_focb_data2, "ME_FOCB", "MassWateR")
    ),
    df_mwr2
  )

  # Test - FOCB long format
  df_mwr3 <- data.frame(
    "Monitoring Location ID" = "BMR02",
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = c(
      "2023-05-23", "2023-06-21", "2023-07-05", "2023-07-18", "2023-08-16"
    ),
    "Activity Start Time" = NA,
    "Activity Depth/Height Measure" = 0.2,
    "Activity Depth/Height Unit" = "m",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = "TN",
    "Result Value" = c(0.22, 0.34, 0.28, 0.28, 0.25),
    "Result Unit" = "mg/l",
    "Quantitation Limit" = 0.073,
    "QC Reference Value" = NA_integer_,
    "Result Measure Qualifier" = "J",
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA,
    "Result Comment" = NA,
    check.names = FALSE
  )
  df_mwr3[["Activity Start Date"]] <- as.Date(df_mwr3[["Activity Start Date"]])

  expect_equal(
    suppressMessages(
      format_results(tst$me_focb_data3, "ME_FOCB", "MassWateR")
    ),
    df_mwr3
  )
})

test_that("format_results converts ME_DEP to MassWateR", {
  df_mwr <- data.frame(
    "Monitoring Location ID" = "BMR02",
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = as.Date("2023-05-23"),
    "Activity Start Time" = "12:32:11 PM",
    "Activity Depth/Height Measure" = 0.244,
    "Activity Depth/Height Unit" = "m",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "Water Temp", "Salinity", "DO", "DO saturation", "pH"
    ),
    "Result Value" = c(11.315, 27.46, 9.26, 100.7, 7.93),
    "Result Unit" = c("deg C", "ppth", "mg/l", "%", "None"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = NA_integer_,
    "Result Measure Qualifier" = NA,
    "Result Attribute" = NA,
    "Sample Collection Method ID" = "IS",
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA,
    "Result Comment" = NA_character_,
    check.names = FALSE
  )
  df_mwr[["Activity Start Date"]] <- as.Date(df_mwr[["Activity Start Date"]])

  # Test - ME_DEP to MassWateR
  expect_equal(
    suppressMessages(
      format_results(
        tst$me_dep_data,
        "ME_DEP",
        "MassWateR",
        date_format = "m/d/y"
      )
    ),
    df_mwr
  )
})

# Test MASSACHUSETTS ----
test_that("format_results converts MA_BRC to MassWateR", {
  df_mwr <- data.frame(
    "Monitoring Location ID" = "B-06-01-050",
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = as.Date("2004-04-10"),
    "Activity Start Time" = "04:30",
    "Activity Depth/Height Measure" = NA,
    "Activity Depth/Height Unit" = NA,
    "Activity Relative Depth Name" = "Surface",
    "Characteristic Name" = c(
      "Nitrate", "Ortho P", "Air Temp", "DO saturation", "DO", "Turbidity",
      "Water Temp"
    ),
    "Result Value" = c(0.4, 0.38, 7, 65, 7.8, 0.55, 7),
    "Result Unit" = c("mg/l", "mg/l", "deg C", "%", "mg/l", "NTU", "deg C"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = c(0.4, 0.36, NA, NA, NA, NA, NA),
    "Result Measure Qualifier" = NA_character_,
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "BRC",
    "Local Record ID" = c(
      "B-06-01-050_2004-04-10 04:30_NO3; B-06-01-050_2004-04-10 04:30_NO3R",
      "B-06-01-050_2004-04-10 04:30_PO4; B-06-01-050_2004-04-10 04:30_PO4R",
      "B-06-01-050_2004-04-10 04:30_TAC", "B-06-01-050_2004-04-10 04:30_OXYSAT",
      "B-06-01-050_2004-04-10 04:30_DOXY", "B-06-01-050_2004-04-10 04:30_TURB1",
      "B-06-01-050_2004-04-10 04:30_TWC"
    ),
    "Result Comment" = NA_character_,
    check.names = FALSE
  )

  # Test
  expect_equal(
    suppressMessages(
      format_results(
        tst$ma_brc_data,
        "MA_BRC",
        "MassWateR",
        date_format = "Y-m-d H:M"
      )
    ),
    df_mwr
  )
})

test_that("format_results converts MA_BRC to RI_DEM", {
  df_dem <- data.frame(
    "Station Name" = "B-06-01-050",
    Date = as.Date("2004-04-10"),
    Time = "04:30",
    "Sample Type" = c(
      "Grab", "Grab", "Grab", "Grab", "Replicate", "Grab", "Replicate", "Grab",
      "Grab"
    ),
    "Sample Media" = NA,
    Depth = NA,
    Parameter = c(
      "Air Temperature", "Dissolved Oxygen Percent Saturation - 00301",
      "Dissolved Oxygen - 00300", "Nitrate", "Nitrate",
      "Orthophosphate, Dissolved - 00671", "Orthophosphate, Dissolved - 00671",
      "Turbidity - 00070", "Temperature - 00011"
    ),
    Concentration = c(7, 65, 7.8, 0.4, 0.4, 0.38, 0.36, 0.55, 7),
    Unit = c("C", "%", "mg/l", "mg/l", "mg/l", "mg/l", "mg/l", "NTU", "C"),
    check.names = FALSE
  )
  blank_col <- c(
    "Qualifier Code", "Detection Limit", "Detection Limit Unit",
    "Quantitation Level", "Quantitation Level Unit", "Lab Name",
    "Analytical Method Number", "Sediment Particle Size", "Particle Size Unit",
    "Fish Sample Type", "Fish Taxa", "Comments"
  )
  df_dem[blank_col] <- NA

  # Test
  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_results(
          tst$ma_brc_data,
          "MA_BRC",
          "RI_DEM",
          date_format = "Y-m-d H:M"
        )
      )
    ),
    df_dem
  )
  expect_warning(
    suppressMessages(
      format_results(
        tst$ma_brc_data,
        "MA_BRC",
        "RI_DEM",
        date_format = "Y-m-d H:M"
      )
    ),
    regexp = "Invalid variables in Parameter: Air Temperature, Nitrate"
  )
})

test_that("format_results converts MA_BRC to WQdashboard", {
  df_wqd <- data.frame(
    Site_ID = "B-06-01-050",
    Activity_Type = c(
      "Field Msr/Obs", "Field Msr/Obs", "Field Msr/Obs", "Field Msr/Obs",
      "Quality Control Field Replicate Msr/Obs", "Field Msr/Obs",
      "Quality Control Field Replicate Msr/Obs", "Field Msr/Obs",
      "Field Msr/Obs"
    ),
    Date = as.Date("2004-04-10"),
    Depth = NA_integer_,
    Depth_Unit = NA_character_,
    Depth_Category = "Surface",
    Parameter = c(
      "Temperature, air", "Dissolved oxygen saturation",
      "Dissolved oxygen (DO)", "Nitrate", "Nitrate", "Orthophosphate",
      "Orthophosphate", "Turbidity", "Temperature, water"
    ),
    Result = c(7, 65, 7.8, 0.4, 0.4, 0.38, 0.36, 0.55, 7),
    Result_Unit = c(
      "deg C", "%", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "NTU", "deg C"
    ),
    Lower_Detection_Limit = NA,
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = NA,
    Qualifier = NA
  )

  # Test
  expect_equal(
    suppressMessages(
      format_results(
        tst$ma_brc_data,
        "MA_BRC",
        "WQdashboard",
        date_format = "Y-m-d H:M"
      )
    ),
    df_wqd
  )
})

# Test RHODE ISLAND ----
test_that("format_results converts RI_WW to RI_DEM and vice versa", {
  df_ww <- data.frame(
    "WW ID" = "WW066",
    "Date of Sample" = "5/7/2021",
    Time = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    Depth = c(0.5, 0.5, 1),
    Parameter = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    Concentration = c(1.25, 4.725, 1),
    Unit = c("MPN/100", "MPN/100", "mg/L"),
    "Qualifier Code" = c("U", NA, NA),
    "Detection Limit" = c(1, 1, 0.015),
    "Detection Limit Unit" = c("MPN/100", "MPN/100", "mg/L"),
    "Quantitation Level" = c(1, 1, 0.015),
    "Quantitation Level Unit" = c("MPN/100", "MPN/100", "mg/L"),
    "Lab Name" = "URIWW",
    "Analytical Method Number" = c(
      NA, NA, "4500-NO3(F): Nitrate in Water- Automated Cadmium Reduction"
    ),
    "Sediment Particle Size" = NA,
    "Particle Size Unit" = NA,
    "Fish Sample Type" = NA,
    "Fish Taxa" = NA,
    Comments = c(NA, NA, "Trilogy"),
    "Monitoring location" = "Worden Pond",
    Watershed = "Upper Pawcatuck River",
    "Watershed Code" = "WD",
    "MONITOR 1" = NA,
    "MONITOR 2" = NA,
    check.names = FALSE
  )

  df_dem <- data.frame(
    "Station Name" = "WW066",
    Date = as.Date("2021-05-07"),
    Time = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    Depth = c(0.5, 0.5, 1),
    Parameter = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    Concentration = c(1.25, 4.725, 1),
    Unit = c("MPN/100ml", "MPN/100ml", "mg/l"),
    "Qualifier Code" = c("U", NA, NA),
    "Detection Limit" = c(1, 1, 0.015),
    "Detection Limit Unit" = c("MPN/100ml", "MPN/100ml", "mg/l"),
    "Quantitation Level" = c(1, 1, 0.015),
    "Quantitation Level Unit" = c("MPN/100", "MPN/100", "mg/L"),
    "Lab Name" = "URIWW",
    "Analytical Method Number" = c(
      NA, NA, "4500-NO3(F): Nitrate in Water- Automated Cadmium Reduction"
    ),
    "Sediment Particle Size" = NA,
    "Particle Size Unit" = NA,
    "Fish Sample Type" = NA,
    "Fish Taxa" = NA,
    Comments = c(NA, NA, "Trilogy"),
    check.names = FALSE
  )

  df_ww2 <- df_ww
  df_ww2[["Date of Sample"]] <- as.Date("2021-05-07")
  df_ww2$Unit <- c("MPN/100ml", "MPN/100ml", "mg/l")
  df_ww2[["Detection Limit Unit"]] <- c("MPN/100ml", "MPN/100ml", "mg/l")
  df_ww2[["Monitoring location"]] <- NA
  df_ww2[["Watershed"]] <- NA
  df_ww2[["Watershed Code"]] <- NA
  df_ww2[["MONITOR 1"]] <- NA
  df_ww2[["MONITOR 2"]] <- NA

  # Test Watershed Watch to RI DEM
  expect_equal(
    suppressMessages(
      format_results(df_ww, "RI_WW", "RI_DEM", date_format = "m/d/Y")
    ),
    df_dem
  )

  # Test RI DEM back to Watershed Watch
  expect_equal(
    suppressMessages(
      format_results(df_dem, "RI_DEM", "RI_WW")
    ),
    df_ww2
  )
})

test_that("format_results converts RI_WW to MassWateR", {
  df_ww <- data.frame(
    "WW ID" = "WW066",
    "Date of Sample" = "5/7/2021",
    Time = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    Depth = c(0.5, 0.5, 1),
    Parameter = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    Concentration = c(1.25, 4.725, 1),
    Unit = c("MPN/100", "MPN/100", "mg/L"),
    "Qualifier Code" = c("U", NA, NA),
    "Detection Limit" = c(1, 1, 0.015),
    "Detection Limit Unit" = c("MPN/100", "MPN/100", "mg/L"),
    "Quantitation Level" = c(1, 1, 0.015),
    "Quantitation Level Unit" = c("MPN/100", "MPN/100", "mg/L"),
    "Lab Name" = "URIWW",
    "Analytical Method Number" = c(
      NA, NA, "4500-NO3(F): Nitrate in Water- Automated Cadmium Reduction"
    ),
    "Sediment Particle Size" = NA,
    "Particle Size Unit" = NA,
    "Fish Sample Type" = NA,
    "Fish Taxa" = NA,
    Comments = c(NA, NA, "Trilogy"),
    "Monitoring location" = "Worden Pond",
    "Watershed" = "Upper Pawcatuck River",
    "Watershed code" = "WD",
    "MONITOR 1" = NA,
    "MONITOR 2" = NA,
    check.names = FALSE
  )

  df_mwr <- data.frame(
    "Monitoring Location ID" = "WW066",
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = as.Date("2021-05-07"),
    "Activity Start Time" = "12:28:00 PM",
    "Activity Depth/Height Measure" = c(0.5, 1),
    "Activity Depth/Height Unit" = "m",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c("Enterococcus", "Nitrate + Nitrite"),
    "Result Value" = c(4.725, 1),
    "Result Unit" = c("MPN/100ml", "mg/l"),
    "Quantitation Limit" = c(1, 0.015),
    "QC Reference Value" = c("BDL", NA),
    "Result Measure Qualifier" = NA_character_,
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = NA,
    "Local Record ID" = NA_character_,
    "Result Comment" = c(NA, "Trilogy"),
    check.names = FALSE
  )

  # Test
  expect_equal(
    suppressMessages(
      format_results(df_ww, "RI_WW", "MassWateR", date_format = "m/d/Y")
    ),
    df_mwr
  )
})

test_that("format_results converts RI_WW to WQdashboard", {
  df_ww <- data.frame(
    "WW ID" = "WW066",
    "Date of Sample" = "5/7/2021",
    Time = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    Depth = c(0.5, 0.5, 1),
    Parameter = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    Concentration = c(1.25, 4.725, 1),
    Unit = c("MPN/100", "MPN/100", "mg/L"),
    "Qualifier Code" = c("U", NA, NA),
    "Detection Limit" = c(1, 1, 0.015),
    "Detection Limit Unit" = c("MPN/100", "MPN/100", "mg/L"),
    "Quantitation Level" = c(1, 1, 0.015),
    "Quantitation Level Unit" = c("MPN/100", "MPN/100", "mg/L"),
    "Lab Name" = "URIWW",
    "Analytical Method Number" = c(
      NA, NA, "4500-NO3(F): Nitrate in Water- Automated Cadmium Reduction"
    ),
    "Sediment Particle Size" = NA,
    "Particle Size Unit" = NA,
    "Fish Sample Type" = NA,
    "Fish Taxa" = NA,
    Comments = c(NA, NA, "Trilogy"),
    "Monitoring location" = "Worden Pond",
    Watershed = "Upper Pawcatuck River",
    "Watershed code" = "WD",
    "MONITOR 1" = NA,
    "MONITOR 2" = NA,
    check.names = FALSE
  )

  df_wqd <- data.frame(
    Site_ID = "WW066",
    Activity_Type = c(
      "Quality Control Field Replicate Msr/Obs", "Field Msr/Obs",
      "Field Msr/Obs"
    ),
    Date = as.Date("2021-05-07"),
    Depth = c(0.5, 0.5, 1),
    Depth_Unit = "m",
    Depth_Category = NA,
    Parameter = c("Enterococcus", "Enterococcus", "Nitrate + Nitrite"),
    Result = c(1.25, 4.725, 1),
    Result_Unit = c("MPN/100mL", "MPN/100mL", "mg/L"),
    Lower_Detection_Limit = c(1, 1, 0.015),
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = c("MPN/100mL", "MPN/100mL", "mg/L"),
    Qualifier = c("DL", NA, NA)
  )

  # Test
  expect_equal(
    suppressMessages(
      format_results(df_ww, "RI_WW", "WQdashboard", date_format = "m/d/Y")
    ),
    df_wqd
  )
})

# Test OTHER ----
test_that("format_results converts MassWateR to WQdashboard", {
  df_wqd <- data.frame(
    Site_ID = c("HBS-016", "HBS-016", "HBS-016", NA, NA, NA, NA),
    Activity_Type = c(
      "Field Msr/Obs", "Quality Control Field Replicate Msr/Obs",
      "Sample-Routine", "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Duplicate 2",
      "Quality Control-Calibration Check",
      "Quality Control-Calibration Check Buffer"
    ),
    Date = c(
      "2021-06-13", "2021-06-13", "2021-08-15", "2021-05-16", "2021-05-16",
      "2021-09-12", "2021-09-12"
    ),
    Depth = c(0.3048, 0.3048, 0.2286, NA, NA, NA, NA),
    Depth_Unit = c("m", "m", "m", NA, NA, NA, NA),
    Depth_Category = NA,
    Parameter = c(
      "Dissolved oxygen saturation", "Dissolved oxygen saturation",
      "Total suspended solids", "Nitrate", "Nitrate", "Specific conductance",
      "Specific conductance"
    ),
    Result = c(46.8, 7, 5, 0.45, 0.46, 980, 1000),
    Result_Unit = c("%", "%", "mg/L", "mg/L", "mg/L", "uS/cm", "uS/cm"),
    Lower_Detection_Limit = NA,
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = c(
      "%", "%", "mg/L", "mg/L", "mg/L", "uS/cm", "uS/cm"
    ),
    Qualifier = c(NA, NA, "Q", NA, NA, NA, NA),
    "Activity Start Time" = c("8:00", "8:00", "7:40", NA, NA, NA, NA),
    "Result Attribute" = c(NA, NA, NA, "K16452-MB3", "K16452-MB3", NA, NA),
    "Sample Collection Method ID" = c(NA, NA, "Grab-MassWateR", NA, NA, NA, NA),
    "Project ID" = "Water Quality",
    "Local Record ID" = NA,
    "Result Comment" = c(NA, NA, "River was very full", NA, NA, NA, NA),
    check.names = FALSE
  )
  df_wqd$Date <- as.Date(df_wqd$Date)

  # Test
  expect_equal(
    suppressMessages(
      format_results(
        tst$mwr_data,
        "MassWateR", "WQdashboard",
        drop_extra_col = FALSE,
        date_format = "m/d/Y"
      )
    ),
    df_wqd
  )
})

test_that("format_results converts WQX to WQdashboard", {
  df_wqx <- head(tst$wqx_data)

  df_wqd <- data.frame(
    Site_ID = "ML-06",
    Activity_Type = c(
      "Sample-Routine", "Sample-Routine", "Sample-Routine", "Sample-Routine",
      "Field Msr/Obs", "Sample-Routine"
    ),
    Date = as.Date("2017-03-01"),
    Depth = NA_integer_,
    Depth_Unit = NA_character_,
    Depth_Category = NA,
    Parameter = c(
      "Orthophosphate", "Total Kjeldahl nitrogen",
      "Nitrogen/Phosphorus molar ratio", "pH", "Conductivity", "Turbidity"
    ),
    Result = c(NA, NA, NA, 7.1, 4.3, NA),
    Result_Unit = c(NA, NA, NA, "None", "mg/L", NA),
    Lower_Detection_Limit = c(NA, 1.1, 1.1, NA, NA, 12),
    Upper_Detection_Limit = c(0.058, NA, NA, NA, NA, NA),
    Detection_Limit_Unit = c("mg/L", "mg/L", "mg/L", NA, NA, "NTU"),
    Qualifier = c("DL", "DL", "DL", NA, NA, "DL"),
    Detection_Limit_Type = c(
      "Upper Quantitation Limit", "Method Detection Level",
      "Method Detection Level", NA, NA, "Lower Reporting Limit"
    ),
    "Project ID" = "TEMPLATE_PCHEM",
    "Activity ID (CHILD-subset)" = c(
      "ML-06:20170301:1433:SR:WB:", "ML-06:20170301:1433:SR:WB:",
      "ML-06:20170301:1433:SR:WB:", "ML-06:20170301:1433:SR:WB:",
      "ML-06:20170301:1433:FM:WB:", "ML-06:20170301:1433:SR:WB:"
    ),
    "Activity ID User Supplied (PARENTs)" = NA,
    "Activity Media Name" = "Water",
    "Activity Start Time" = "14:33",
    "Activity Start Time Zone" = "MST",
    "Sample Collection Method ID" = "Grab Sample Method",
    "Sample Collection Method Context" = NA,
    "Sample Collection Equipment Name" = "Water Bottle",
    "Sample Collection Equipment Comment" = NA_character_,
    "Characteristic Name User Supplied" = NA,
    "Method Speciation" = c("as P", "as N", NA, NA, NA, NA),
    "Result Detection Condition" = c(
      "Not Detected", "Not Detected", "Not Detected", NA, NA, "Not Detected"
    ),
    "Result Sample Fraction" = c(
      "Filtered, lab", "Filtered, lab", NA, NA, NA, NA
    ),
    "Result Status ID" = "Final",
    ResultTemperatureBasis = NA_character_,
    "Statistical Base Code" = NA_character_,
    ResultTimeBasis = NA_character_,
    "Result Value Type" = "Actual",
    "Result Analytical Method ID" = 120.1,
    "Result Analytical Method Context" = "USEPA",
    "Analysis Start Date" = as.Date("2017-03-02"),
    "Result Comment" = NA,
    check.names = FALSE
  )

  expect_equal(
    suppressMessages(
      format_results(
        df_wqx,
        "WQX", "WQdashboard",
        drop_extra_col = FALSE,
        date_format = "m/d/Y"
      )
    ),
    df_wqd
  )
})

# Test ERROR ----
test_that("format_results error messages", {
  expect_error(
    suppressMessages(
      format_results(tst$mwr_data, "foobar", "foofy")
    ),
    regexp = "Invalid format"
  )
})

# Test format_mwr_results ----
test_that("format_mwr_results works", {
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
    suppressMessages(
      format_mwr_results(df_in)
    ),
    df_out
  )

  # Test edge case - BDL, AQL handling, no name repair
  colnames(df_in) <- make.names(colnames(df_in))
  df_in$Result.Value <- c(NA, 7, 5, NA, 0.46, 980, NA)
  df_in$Result.Measure.Qualifier <- c("DL", NA, "Q", "GT", NA, NA, "DL")

  df_out <- df_out
  df_out[["Result Value"]] <- c("AQL", "BDL", 5, 980)
  df_out[["QC Reference Value"]] <- c(0.46, 7, NA, "BDL")

  expect_equal(
    suppressMessages(
      format_mwr_results(df_in)
    ),
    df_out
  )
})

test_that("format_mwr_results errors", {
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

  df_nocol <- df_in
  df_nocol[["Local Record ID"]] <- NULL
  expect_warning(
    suppressMessages(format_mwr_results(df_nocol)),
    regexp = "Missing suggested columns: Local Record ID"
  )

  df_nocol[["Activity Type"]] <- NULL
  expect_error(
    suppressMessages(format_mwr_results(df_nocol)),
    regexp = "Missing mandatory columns: Activity Type"
  )

  df_extra <- df_in
  df_extra$foo <- "superb owl"
  chk <- suppressMessages(format_mwr_results(df_extra))
  expect_false("foo" %in% colnames(chk))
})

# Test format_wqd_results ----
test_that("format_wqd_results works", {
  # Test example data
  df_in <- data.frame(
    Site_ID = "foo",
    Activity_Type = "Field Msr/Obs",
    Date = as.Date("2025-11-12"),
    Depth = c(1, 2, 3, "foo"),
    Depth_Unit = c("m", "ft", "m", "ft"),
    Parameter = "Dissolved oxygen (DO)",
    Result = 8,
    Result_Unit = "mg/l"
  )

  df_out <- data.frame(
    Site_ID = "foo",
    Activity_Type = "Field Msr/Obs",
    Date = as.Date("2025-11-12"),
    Depth = c(1, 0.6096, 3, "foo"),
    Depth_Unit = c("m", "m", "m", "ft"),
    Parameter = "Dissolved oxygen (DO)",
    Result = 8,
    Result_Unit = "mg/L",
    Depth_Category = NA,
    Lower_Detection_Limit = NA,
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = NA,
    Qualifier = NA
  )

  expect_equal(
    suppressMessages(
      format_wqd_results(df_in)
    ),
    df_out
  )
})

test_that("format_wqd_results errors", {
  df_in <- data.frame(
    Site_ID = "foo",
    Activity_Type = "Field Msr/Obs",
    Date = as.Date("2025-11-12"),
    Depth = c(1, 2, 3, "foo"),
    Depth_Unit = c("m", "ft", "m", "ft"),
    Depth_Category = NA,
    Parameter = "Dissolved oxygen (DO)",
    Result = 8,
    Result_Unit = "mg/l",
    Qualifier = NA
  )

  expect_no_error(
    suppressMessages(format_wqd_results(df_in))
  )

  df_in$Site_ID <- NULL
  df_in$Date <- NULL

  expect_error(
    suppressMessages(format_wqd_results(df_in)),
    regexp = "Missing mandatory columns: Site_ID, Date"
  )
})
