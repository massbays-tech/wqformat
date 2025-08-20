# Test MAINE ----
test_that("format_results converts ME_FOCB to ME_DEP", {
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

  # Expected output - ME_DEP
  dep_col_order <- c(
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

  df_wide1_DEP <- data.frame(
    "PROJECT/SITE" = "FRIENDS OF CASCO BAY ALL SITES",
    "SAMPLE_POINT_NAME" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18", "HR2", "HR2", "HR2"
    ),
    "SAMPLE_DATE" = c(
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-24", "2023-05-24", "2023-05-24"
    ),
    "SAMPLE_TIME" = c(
      "12:32", "12:32", "12:32", "12:32", "12:32", "12:45", "12:45", "12:45",
      "12:45", "12:45", "10:22", "10:22", "10:22"
    ),
    "QC_TYPE" = "NA",
    "PARAMETER_NAME" = c(
      "CLOUD COVER", "WSPD", "WDIR", "DEPTH", "SECCHI", "CLOUD COVER", "WSPD",
      "WDIR", "DEPTH", "SECCHI", "CLOUD COVER", "WSPD", "WDIR"
    ),
    "CONCENTRATION" = c(
      "50", "3", "120", "10.7", "1.9", "50", "3", "150", "3.2", "BSV", "50",
      "2", "180"
    ),
    "LAB_QUALIFIER" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "G", NA, NA, NA),
    "PARAMETER_UNITS" = c(
      "%", "BFT", "DEG TRUE", "M", "M", "%", "BFT", "DEG TRUE", "M", "M", "%",
      "BFT", "DEG TRUE"
    ),
    "ANALYSIS_DATE" = NA,
    "SAMPLED_BY" = "FRIENDS OF CASCO BAY",
    check.names = FALSE
  )
  df_wide1_DEP$SAMPLE_DATE <- as.Date(df_wide1_DEP$SAMPLE_DATE)
  df_wide1_DEP$ANALYSIS_DATE <- as.Date(df_wide1_DEP$ANALYSIS_DATE)
  missing_col <- setdiff(dep_col_order, colnames(df_wide1_DEP))
  df_wide1_DEP[missing_col] <- NA
  df_wide1_DEP <- df_wide1_DEP[, dep_col_order]

  df_wide2_DEP <- data.frame(
    "SAMPLE_POINT_NAME" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18",
      "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "HR2", "HR2", "HR2",
      "HR2", "HR2", "HR2", "HR2"
    ),
    "SAMPLE_DATE" = c(
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-24",
      "2023-05-24", "2023-05-24", "2023-05-24", "2023-05-24", "2023-05-24",
      "2023-05-24"
    ),
    "SAMPLE_TIME" = c(
      "12:32", "12:32", "12:32", "12:32", "12:32", "12:32", "12:32", "12:45",
      "12:45", "12:45", "12:45", "12:45", "12:45", "12:45", "10:22", "10:22",
      "10:22", "10:22", "10:22", "10:22", "10:22"
    ),
    "QC_TYPE" = "NA",
    "SAMPLE_DEPTH" = c(
      0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0,
      0, 0, 0, 0, 0, 0
    ),
    "PARAMETER_NAME" = c(
      "TEMP", "SALINITY", "DO", "DO SAT", "PH", "CHLOROPHYLL", "TUR", "TEMP",
      "SALINITY", "DO", "DO SAT", "PH", "CHLOROPHYLL", "TUR", "TEMP",
      "SALINITY", "DO", "DO SAT", "PH", "CHLOROPHYLL", "TUR"
    ),
    "CONCENTRATION" = c(
      11.3, 27.5, 9.3, 100.7, 7.93, 1.2, 2.7, 11, 28, 9.3, 100.7, 7.93, 1.2,
      1.4, 14, 28, 8.2, 94.9, 7.82, 1.4, 2.4
    ),
    "PARAMETER_UNITS" = c(
      "DEG C", "PSU", "MG/L", "%", "STU", "UG/L", "FNU", "DEG C", "PSU", "MG/L",
      "%", "STU", "UG/L", "FNU", "DEG C", "PSU", "MG/L", "%", "STU", "UG/L",
      "FNU"
    ),
    "LAB_QUALIFIER" = c(
      NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA,
      NA, "J", NA
    ),
    "ANALYSIS_DATE" = NA,
    "SAMPLED_BY" = "FRIENDS OF CASCO BAY",
    check.names = FALSE
  )
  df_wide2_DEP$SAMPLE_DATE <- as.Date(df_wide2_DEP$SAMPLE_DATE)
  df_wide2_DEP$ANALYSIS_DATE <- as.Date(df_wide2_DEP$ANALYSIS_DATE)
  df_wide2_DEP["SAMPLE_DEPTH_UNIT"] <- "M"
  df_wide2_DEP[["PROJECT/SITE"]] <- "FRIENDS OF CASCO BAY ALL SITES"
  missing_col <- setdiff(dep_col_order, colnames(df_wide2_DEP))
  df_wide2_DEP[missing_col] <- NA
  df_wide2_DEP <- df_wide2_DEP[, dep_col_order]

  df_long_DEP <- data.frame(
    "SAMPLE_POINT_NAME" = c("BMR02", "EEB18", "HR2"),
    "SAMPLE_DATE" = c("2023-05-23", "2023-05-23", "2023-05-24"),
    "QC_TYPE" = "NA",
    "ANALYSIS_LAB" = "UMWL",
    "ANALYSIS_DATE" = c("2023-07-06", "2023-07-06", "2023-06-07"),
    "PARAMETER_NAME" = "TN AS N",
    "CONCENTRATION" = c(0.22, 0.18, 0.28),
    "PARAMETER_UNITS" = "MG/L",
    "REPORTING_LIMIT" = 0.1,
    "MDL" = 0.73,
    "TEST" = "SM4500NE_2021",
    "SAMPLE_DEPTH" = 0.2,
    "SAMPLE_DEPTH_UNIT" = "M",
    "LAB_QUALIFIER" = c("J", "J", NA),
    check.names = FALSE
  )
  df_long_DEP$SAMPLE_DATE <- as.Date(df_long_DEP$SAMPLE_DATE)
  df_long_DEP$ANALYSIS_DATE <- as.Date(df_long_DEP$ANALYSIS_DATE)
  df_long_DEP[["PROJECT/SITE"]] <- "FRIENDS OF CASCO BAY ALL SITES"
  df_long_DEP$SAMPLED_BY <- "FRIENDS OF CASCO BAY"
  missing_col <- setdiff(dep_col_order, colnames(df_long_DEP))
  df_long_DEP[missing_col] <- NA
  df_long_DEP <- df_long_DEP[, dep_col_order]

  # test ME_FOCB to ME_DEP
  expect_equal(
    suppressMessages(
      format_results(df_wide1, "ME_FOCB", "ME_DEP", date_format = "m/d/y")
    ),
    df_wide1_DEP
  )
  expect_equal(
    suppressMessages(
      format_results(df_wide2, "ME_FOCB", "ME_DEP", date_format = "m/d/y")
    ),
    df_wide2_DEP
  )
  expect_equal(
    suppressMessages(
      format_results(df_long, "ME_FOCB", "ME_DEP", date_format = "m/d/y")
    ),
    df_long_DEP
  )
})

test_that("format_results converts ME_FOCB to MassWateR", {
  # Input - ME_FOCB in 3 formats
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

  # Expected output - MassWateR
  df_wide1_mwr <- data.frame(
    "Monitoring Location ID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18", "HR2", "HR2", "HR2"
    ),
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = c(
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-24", "2023-05-24", "2023-05-24"
    ),
    "Activity Start Time" = c(
      "12:32", "12:32", "12:32", "12:32", "12:32", "12:45", "12:45", "12:45",
      "12:45", "12:45", "10:22", "10:22", "10:22"
    ),
    "Activity Depth/Height Measure" = NA,
    "Activity Depth/Height Unit" = NA,
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "Cloud Cover", "Depth", "Secchi Depth", "Wind Direction", "Wind Speed",
      "Cloud Cover", "Depth", "Secchi Depth", "Wind Direction", "Wind Speed",
      "Cloud Cover", "Wind Direction", "Wind Speed"
    ),
    "Result Value" = c(
      "50", "10.7", "1.9", "120", "3", "50", "3.2", "AQL", "150", "3", "50",
      "180", "2"
    ),
    "Result Unit" = c(
      "%", "m", "m", "DEG True", "BFT", "%", "m", "m", "DEG True", "BFT", "%",
      "DEG True", "BFT"
    ),
    "Quantitation Limit" = NA,
    "QC Reference Value" = NA_integer_,
    "Result Measure Qualifier" = NA_character_,
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA_character_,
    "Result Comment" = NA_character_,
    check.names = FALSE
  )
  df_wide1_mwr[["Activity Start Date"]] <- as.Date(
    df_wide1_mwr[["Activity Start Date"]]
  )

  df_wide2_mwr <- data.frame(
    "Monitoring Location ID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02",
      "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18",
      "HR2", "HR2", "HR2", "HR2", "HR2", "HR2", "HR2"
    ),
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = c(
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-24",
      "2023-05-24", "2023-05-24", "2023-05-24", "2023-05-24", "2023-05-24",
      "2023-05-24"
    ),
    "Activity Start Time" = c(
      "12:32", "12:32", "12:32", "12:32", "12:32", "12:32", "12:32",
      "12:45", "12:45", "12:45", "12:45", "12:45", "12:45", "12:45",
      "10:22", "10:22", "10:22", "10:22", "10:22", "10:22", "10:22"
    ),
    "Activity Depth/Height Measure" = c(
      0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
      0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
      0, 0, 0, 0, 0, 0, 0
    ),
    "Activity Depth/Height Unit" = "m",
    "Activity Relative Depth Name" = NA,
    # Characteristic Name pattern repeats 3 times
    "Characteristic Name" = c(
      "Chl a", "DO", "DO saturation", "Salinity", "Turbidity", "Water Temp",
      "pH"
    ),
    "Result Value" = c(
      1.2, 9.3, 100.7, 27.5, 2.7, 11.3, 7.93,
      1.2, 9.3, 100.7, 28, 1.4, 11, 7.93,
      1.4, 8.2, 94.9, 28, 2.4, 14, 7.82
    ),
    # Result Unit pattern repeats 3 times
    "Result Unit" = c("ug/l", "mg/l", "%", "PSU", "FNU", "deg C", "None"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = NA_integer_,
    # Result Measure Qualifier Pattern repeats 3 times
    "Result Measure Qualifier" = c("J", NA, NA, NA, NA, NA, NA),
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA_character_,
    "Result Comment" = NA_character_,
    check.names = FALSE
  )
  df_wide2_mwr[["Activity Start Date"]] <- as.Date(
    df_wide2_mwr[["Activity Start Date"]]
  )

  df_long_mwr <- data.frame(
    "Monitoring Location ID" = c("BMR02", "EEB18", "HR2"),
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = c("2023-05-23", "2023-05-23", "2023-05-24"),
    "Activity Start Time" = NA,
    "Activity Depth/Height Measure" = 0.2,
    "Activity Depth/Height Unit" = "m",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = "TN",
    "Result Value" = c(0.22, 0.18, 0.28),
    "Result Unit" = "mg/l",
    "Quantitation Limit" = 0.73,
    "QC Reference Value" = NA_integer_,
    "Result Measure Qualifier" = c("J", "J", NA),
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA_character_,
    "Result Comment" = NA_character_,
    check.names = FALSE
  )
  df_long_mwr[["Activity Start Date"]] <- as.Date(
    df_long_mwr[["Activity Start Date"]]
  )

  # Test warnings
  expect_warning(
    expect_warning(
      suppressMessages(
        format_results(df_wide1, "ME_FOCB", "MassWateR", date_format = "m/d/y")
      ),
      "Invalid variables in Characteristic Name: Cloud Cover, Wind Speed, Wind Direction"
    ),
    "Invalid variables in Result Unit: BFT, DEG True"
  )

  # Test output
  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_results(df_wide1, "ME_FOCB", "MassWateR", date_format = "m/d/y")
      )
    ),
    df_wide1_mwr
  )
  expect_equal(
    suppressMessages(
      format_results(df_wide2, "ME_FOCB", "MassWateR", date_format = "m/d/y")
    ),
    df_wide2_mwr
  )
  expect_equal(
    suppressMessages(
      format_results(df_long, "ME_FOCB", "MassWateR", date_format = "m/d/y")
    ),
    df_long_mwr
  )
})

test_that("format_results converts ME_DEP to MassWateR", {
  # Input data - ME_DEP
  df_dep <- data.frame(
    "PROJECT/SITE" = "FRIENDS OF CASCO BAY ALL SITES",
    "SAMPLE_POINT_NAME" = c("BMR02", "NMM79", "OBY35"),
    "LAB_SAMPLE_ID" = c("BMR02 2023", "NMM79 2023", "OBY35 2023"),
    "SAMPLE_ID" = c("BMR02 2023", "NMM79 2023", "OBY35 2023"),
    "ANALYSIS_LAB" = "FT",
    "SAMPLE_DATE" = c("5/23/23", "5/24/23", "5/24/23"),
    "SAMPLE_TIME" = c("12:32:11 PM", "11:02:03 AM", "11:58:35 AM"),
    "SAMPLE_DEPTH" = c(0.244, 0.229, 0.037),
    "SAMPLE_TYPE" = "SW",
    "QC_TYPE" = "NA",
    "PARAMETER_NAME" = c("TEMPERATURE", "SALINITY", "CHLOROPHYLL"),
    "CONCENTRATION" = c(11.315, 27.12, 2.12),
    "LAB_QUALIFIER" = c(NA, NA, "J"),
    "REPORTING_LIMIT" = NA,
    "PARAMETER_UNITS" = c("DEG C", "PPTH", "UG/L"),
    "TEST" = "MM",
    "METER_ID" = c("YSI EXO2"),
    "RESULT_TYPE_CODE" = "PM",
    "LAB_COMMENT" = NA,
    "SAMPLE_DEPTH_UNIT" = "M",
    "SAMPLE_COLLECTION_METHOD" = "IS",
    "SAMPLE_LOCATION" = c("B", "SH", "SH"),
    "TREATMENT_STATUS" = "N",
    "PARAMETER_FITLERED" = c("NA", "NA", "U"),
    "SAMPLED_BY" = "FRIENDS OF CASCO BAY",
    "SAMPLE_COMMENTS" = NA,
    "PARAMETER_QUALIFIER" = c(NA, NA, "OPTICAL CHL"),
    check.names = FALSE
  )

  # Expected output - MassWateR
  df_mwr <- data.frame(
    "Monitoring Location ID" = c("BMR02", "NMM79", "OBY35"),
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = c("2023-05-23", "2023-05-24", "2023-05-24"),
    "Activity Start Time" = c("12:32:11 PM", "11:02:03 AM", "11:58:35 AM"),
    "Activity Depth/Height Measure" = c(0.244, 0.229, 0.037),
    "Activity Depth/Height Unit" = "m",
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c("Water Temp", "Salinity", "Chl a"),
    "Result Value" = c(11.315, 27.12, 2.12),
    "Result Unit" = c("deg C", "ppth", "ug/l"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = NA_integer_,
    "Result Measure Qualifier" = c(NA, NA, "J"),
    "Result Attribute" = NA,
    "Sample Collection Method ID" = "IS",
    "Project ID" = "FRIENDS OF CASCO BAY ALL SITES",
    "Local Record ID" = NA_character_,
    "Result Comment" = NA_character_,
    check.names = FALSE
  )
  df_mwr[["Activity Start Date"]] <- as.Date(df_mwr[["Activity Start Date"]])

  # Test - ME_DEP to MassWateR
  expect_equal(
    suppressMessages(
      format_results(df_dep, "ME_DEP", "MassWateR", date_format = "m/d/y")
    ),
    df_mwr
  )
})

# Test MASSACHUSETTS ----
test_that("format_results converts MA_BRC to MassWateR", {
  # Input, expected output
  df_brc <- data.frame(
    "ID" = c(74635, 74639, 74640),
    "SEID" = 8528,
    "SITE_BRC_CODE" = "B-02-01-020",
    "DATE_TIME" = "2018-11-10 01:50",
    "PARAMETER" = c(
      "Dissolved Oxy Saturation", "Orthophosphate", "Orthophosphate Replicate"
    ),
    "RESULT" = c(80, .2, .19),
    "UNITS" = c("%", "mg/L", "mg/L"),
    "UNIQUE_ID" = c(
      "B-02-01-020_2018-11-10 01:50_DOXY", "B-02-01-020_2018-11-10 01:50_NO3",
      "B-02-01-020_2018-11-10 01:50_NO3R"
    )
  )

  df_mwr <- data.frame(
    "Monitoring Location ID" = "B-02-01-020",
    "Activity Type" = "Field Msr/Obs",
    "Activity Start Date" = as.Date("2018-11-10"),
    "Activity Start Time" = "01:50",
    "Activity Depth/Height Measure" = NA,
    "Activity Depth/Height Unit" = NA,
    "Activity Relative Depth Name" = "Surface",
    "Characteristic Name" = c("DO saturation", "Ortho P"),
    "Result Value" = c(80, .2),
    "Result Unit" = c("%", "mg/l"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = c(NA, .19),
    "Result Measure Qualifier" = NA_character_,
    "Result Attribute" = NA,
    "Sample Collection Method ID" = NA,
    "Project ID" = "BRC",
    "Local Record ID" = c(
      "B-02-01-020_2018-11-10 01:50_DOXY",
      "B-02-01-020_2018-11-10 01:50_NO3; B-02-01-020_2018-11-10 01:50_NO3R"
    ),
    "Result Comment" = NA_character_,
    check.names = FALSE
  )

  # Test
  expect_equal(
    suppressMessages(
      format_results(df_brc, "MA_BRC", "MassWateR", date_format = "Y-m-d H:M")
    ),
    df_mwr
  )
})

test_that("format_results converts MA_BRC to RI_DEM", {
  df_brc <- data.frame(
    "ID" = c(74635, 74639, 74640),
    "SEID" = 8528,
    "SITE_BRC_CODE" = "B-02-01-020",
    "DATE_TIME" = "2018-11-10 01:50",
    "PARAMETER" = c(
      "Dissolved Oxy Saturation", "Orthophosphate", "Orthophosphate Replicate"
    ),
    "RESULT" = c(80, .2, .19),
    "UNITS" = c("%", "mg/L", "mg/L"),
    "UNIQUE_ID" = c(
      "B-02-01-020_2018-11-10 01:50_DOXY", "B-02-01-020_2018-11-10 01:50_NO3",
      "B-02-01-020_2018-11-10 01:50_NO3R"
    )
  )

  df_dem <- data.frame(
    "Station Name" = "B-02-01-020",
    "Date" = as.Date("2018-11-10"),
    "Time" = "01:50",
    "Sample Type" = c("Grab", "Grab", "Replicate"),
    "Sample Media" = NA,
    "Depth" = NA,
    "Parameter" = c(
      "Dissolved Oxygen Percent Saturation - 00301",
      "Orthophosphate, Dissolved - 00671", "Orthophosphate, Dissolved - 00671"
    ),
    "Concentration" = c(80, .2, .19),
    "Unit" = c("%", "mg/l", "mg/l"),
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
    suppressMessages(
      format_results(df_brc, "MA_BRC", "RI_DEM", date_format = "Y-m-d H:M")
    ),
    df_dem
  )
})

test_that("format_results converts MA_BRC to WQdashboard", {
  df_brc <- data.frame(
    "ID" = c(74635, 74639, 74640),
    "SEID" = 8528,
    "SITE_BRC_CODE" = "B-02-01-020",
    "DATE_TIME" = "2018-11-10 01:50",
    "PARAMETER" = c(
      "Dissolved Oxy Saturation", "Orthophosphate", "Orthophosphate Replicate"
    ),
    "RESULT" = c(80, .2, .19),
    "UNITS" = c("%", "mg/L", "mg/L"),
    "UNIQUE_ID" = c(
      "B-02-01-020_2018-11-10 01:50_DOXY", "B-02-01-020_2018-11-10 01:50_NO3",
      "B-02-01-020_2018-11-10 01:50_NO3R"
    )
  )

  df_wqd <- data.frame(
    "Site_ID" = "B-02-01-020",
    "Activity_Type" = c(
      "Field Msr/Obs", "Field Msr/Obs",
      "Quality Control Field Replicate Msr/Obs"
    ),
    "Date" = as.Date("2018-11-10"),
    "Depth" = NA,
    "Depth_Unit" = NA,
    "Depth_Category" = "Surface",
    "Parameter" = c(
      "Dissolved oxygen saturation", "Orthophosphate", "Orthophosphate"
    ),
    "Result" = c(80, .2, .19),
    "Result_Unit" = c("%", "mg/L", "mg/L"),
    "Detection_Limit_Type" = NA,
    "Detection_Limit" = NA,
    "Detection_Limit_Unit" = NA,
    "Qualifier" = NA
  )

  # Test
  expect_equal(
    suppressMessages(
      format_results(df_brc, "MA_BRC", "WQdashboard", date_format = "Y-m-d H:M")
    ),
    df_wqd
  )
})

# Test RHODE ISLAND ----
test_that("format_results converts RI_WW to RI_DEM and vice versa", {
  df_ww <- data.frame(
    "WW ID" = "WW066",
    "Date of Sample" = "5/7/2021",
    "Time" = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    "Depth" = c(0.5, 0.5, 1),
    "Parameter" = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    "Concentration" = c(1.25, 4.725, 1),
    "Unit" = c("MPN/100", "MPN/100", "mg/L"),
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
    "Comments" = c(NA, NA, "Trilogy"),
    "Monitoring location" = "Worden Pond",
    "Watershed" = "Upper Pawcatuck River",
    "Watershed Code" = "WD",
    "MONITOR 1" = NA,
    "MONITOR 2" = NA,
    check.names = FALSE
  )

  df_dem <- data.frame(
    "Station Name" = "WW066",
    "Date" = as.Date("2021-05-07"),
    "Time" = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    "Depth" = c(0.5, 0.5, 1),
    "Parameter" = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    "Concentration" = c(1.25, 4.725, 1),
    "Unit" = c("MPN/100", "MPN/100", "mg/L"),
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
    "Comments" = c(NA, NA, "Trilogy"),
    check.names = FALSE
  )

  df_ww2 <- df_ww
  df_ww2[["Date of Sample"]] <- as.Date("2021-05-07")
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
    "Time" = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    "Depth" = c(0.5, 0.5, 1),
    "Parameter" = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    "Concentration" = c(1.25, 4.725, 1),
    "Unit" = c("MPN/100", "MPN/100", "mg/L"),
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
    "Comments" = c(NA, NA, "Trilogy"),
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
    "Activity Depth/Height Unit" = NA,
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
    "Time" = "12:28:00 PM",
    "Sample Type" = c("Replicate", "Grab", "Grab"),
    "Sample Media" = "Water",
    "Depth" = c(0.5, 0.5, 1),
    "Parameter" = c(
      "Enterococci - 31639", "Enterococci - 31639",
      "Nitrate + Nitrite, Dissolved - 00631"
    ),
    "Concentration" = c(1.25, 4.725, 1),
    "Unit" = c("MPN/100", "MPN/100", "mg/L"),
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
    "Comments" = c(NA, NA, "Trilogy"),
    "Monitoring location" = "Worden Pond",
    "Watershed" = "Upper Pawcatuck River",
    "Watershed code" = "WD",
    "MONITOR 1" = NA,
    "MONITOR 2" = NA,
    check.names = FALSE
  )

  df_wqd <- data.frame(
    "Site_ID" = "WW066",
    "Activity_Type" = c(
      "Quality Control Field Replicate Msr/Obs", "Field Msr/Obs",
      "Field Msr/Obs"
    ),
    "Date" = as.Date("2021-05-07"),
    "Depth" = c(0.5, 0.5, 1),
    "Depth_Unit" = NA,
    "Depth_Category" = NA,
    "Parameter" = c("Enterococcus", "Enterococcus", "Nitrate + Nitrite"),
    "Result" = c(1.25, 4.725, 1),
    "Result_Unit" = c("MPN/100mL", "MPN/100mL", "mg/L"),
    "Detection_Limit_Type" = NA,
    "Detection_Limit" = c(1, 1, 0.015),
    "Detection_Limit_Unit" = c("MPN/100mL", "MPN/100mL", "mg/L"),
    "Qualifier" = c("DL", NA, NA)
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
  df_mwr <- data.frame(
    "Monitoring Location ID"	= c("HBS-016", "HBS-016", NA, NA),
    "Activity Type" = c(
      "Field Msr/Obs", "Sample-Routine", "Quality Control Sample-Lab Duplicate",
      "Quality Control-Calibration Check"
    ),
    "Activity Start Date" = c(
      "6/13/2021", "8/15/2021", "5/16/2021", "9/12/2021"
    ),
    "Activity Start Time" = c("8:00", "7:40", NA, NA),
    "Activity Depth/Height Measure"	= c(1, 0.75, NA, NA),
    "Activity Depth/Height Unit" = c("ft", "ft", NA, NA),
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "DO saturation", "TSS", "Nitrate", "Sp Conductance"
    ),
    "Result Value" = c(46.8, 5, 0.45, 980),
    "Result Unit" = c("%", "mg/l", "mg/l", "uS/cm"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = c(7, NA, 0.46, 1000),
    "Result Measure Qualifier" = c(NA, "Q", NA, NA),
    "Result Attribute" = c(NA, NA, "K16452-MB3", NA),
    "Sample Collection Method ID" = c(NA, "Grab-MassWateR", NA, NA),
    "Project ID" = "Water Quality",
    "Local Record ID" = NA,
    "Result Comment" = c(NA, "River was very full", NA, NA),
    check.names = FALSE
  )

  df_wqd <- data.frame(
    "Site_ID"	= c("HBS-016", "HBS-016", "HBS-016", NA, NA, NA, NA),
    "Activity_Type" = c(
      "Field Msr/Obs", "Quality Control Field Replicate Msr/Obs",
      "Sample-Routine", "Quality Control Sample-Lab Duplicate",
      "Quality Control Sample-Lab Duplicate 2",
      "Quality Control-Calibration Check",
      "Quality Control-Calibration Check Buffer"
    ),
    "Date" = c(
      "2021-06-13", "2021-06-13", "2021-08-15", "2021-05-16", "2021-05-16",
      "2021-09-12", "2021-09-12"
    ),
    "Depth"	= c(1, 1, 0.75, NA, NA, NA, NA),
    "Depth_Unit" = c("ft", "ft", "ft", NA, NA, NA, NA),
    "Depth_Category" = NA,
    "Parameter" = c(
      "Dissolved oxygen saturation", "Dissolved oxygen saturation",
      "Total suspended solids", "Nitrate", "Nitrate", "Specific conductance",
      "Specific conductance"
    ),
    "Result" = c(46.8, 7, 5, 0.45, 0.46, 980, 1000),
    "Result_Unit" = c("%", "%", "mg/L", "mg/L", "mg/L", "uS/cm", "uS/cm"),
    "Detection_Limit_Type" = NA,
    "Detection_Limit" = NA,
    "Detection_Limit_Unit" = NA,
    "Qualifier" = c(NA, NA, "Q", NA, NA, NA, NA),
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
    suppressWarnings(
      suppressMessages(
        format_results(
          df_mwr,
          "MassWateR", "WQdashboard",
          drop_extra_col = FALSE,
          date_format = "m/d/Y"
        )
      )
    ),
    df_wqd
  )

  expect_warning(
    suppressMessages(
      format_results(
        df_mwr,
        "MassWateR", "WQdashboard",
        drop_extra_col = FALSE,
        date_format = "m/d/Y"
      )
    ),
    regexp = "Unable to rename 6 columns"
  )
})

test_that("format_results converts WQX to WQdashboard and vice versa", {
  df_wqx <- data.frame(
    "Project ID" = "TEMPLATE_PCHEM",
    "Monitoring Location ID" = "ML-06",
    "Activity ID (CHILD-subset)" = c(
      "ML-06:20170301:1433:SR:WB:", "ML-06:20170301:1433:SR:WB:",
      "ML-06:20170301:1433:FM:WB:"
    ),
    "Activity ID User Supplied (PARENTs)" = NA,
    "Activity Type" = c("Sample-Routine", "Sample-Routine", "Field Msr/Obs"),
    "Activity Media Name" = "Water",
    "Activity Start Date" = "3/1/2017",
    "Activity Start Time" = "14:33",
    "Activity Start Time Zone" = "MST",
    "Activity Depth/Height Measure" = NA,
    "Activity Depth/Height Unit" = NA,
    "Sample Collection Method ID" = "Grab Sample Method",
    "Sample Collection Method Context" = NA,
    "Sample Collection Equipment Name" = "Water Bottle",
    "Sample Collection Equipment Comment" = NA,
    "Characteristic Name" = c(
      "Kjeldahl nitrogen", "Total Nitrogen/Total Phosphorus Ratio (TN:TP)",
      "Conductivity"
    ),
    "Characteristic Name User Supplied" = NA,
    "Method Speciation" = c("as N", NA, NA),
    "Result Detection Condition" = c("Not Detected", "Not Detected", NA),
    "Result Value" = c(NA, NA, 4.3),
    "Result Unit" = c(NA, NA, "mg/l"),
    "Result Measure Qualifier" = NA,
    "Result Sample Fraction" = c("Filtered, lab", NA, NA),
    "Result Status ID" = "Final",
    "ResultTemperatureBasis" = NA,
    "Statistical Base Code" = NA,
    "ResultTimeBasis" = NA,
    "Result Value Type" = "Actual",
    "Result Analytical Method ID" = 120.1,
    "Result Analytical Method Context" = "USEPA",
    "Analysis Start Date" = "3/2/2017",
    "Result Detection/Quantitation Limit Type" = c(
      "Method Detection Level", "Method Detection Level", NA
    ),
    "Result Detection/Quantitation Limit Measure" = c(1.1, 1.1, NA),
    "Result Detection/Quantitation Limit Unit" = c("mg/l", "mg/l", NA),
    "Result Comment" = NA,
    check.names = FALSE
  )

  df_wqd = data.frame(
    "Site_ID" = "ML-06",
    "Activity_Type" = c("Sample-Routine", "Sample-Routine", "Field Msr/Obs"),
    "Date" = as.Date("2017-03-01"),
    "Depth" = NA,
    "Depth_Unit" = NA,
    "Depth_Category" = NA,
    "Parameter" = c(
      "Kjeldahl nitrogen", "Total Nitrogen/Total Phosphorus Ratio (TN:TP)",
      "Conductivity"
    ),
    "Result" = c(NA, NA, 4.3),
    "Result_Unit" = c(NA, NA, "mg/l"),
    "Detection_Limit_Type" = c(
      "Method Detection Level", "Method Detection Level", NA
    ),
    "Detection_Limit" = c(1.1, 1.1, NA),
    "Detection_Limit_Unit" = c("mg/l", "mg/l", NA),
    "Qualifier" = NA,
    "Project ID" = "TEMPLATE_PCHEM",
    "Activity ID (CHILD-subset)" = c(
      "ML-06:20170301:1433:SR:WB:", "ML-06:20170301:1433:SR:WB:",
      "ML-06:20170301:1433:FM:WB:"
    ),
    "Activity ID User Supplied (PARENTs)" = NA,
    "Activity Media Name" = "Water",
    "Activity Start Time" = "14:33",
    "Activity Start Time Zone" = "MST",
    "Sample Collection Method ID" = "Grab Sample Method",
    "Sample Collection Method Context" = NA,
    "Sample Collection Equipment Name" = "Water Bottle",
    "Sample Collection Equipment Comment" = NA,
    "Characteristic Name User Supplied" = NA,
    "Method Speciation" = c("as N", NA, NA),
    "Result Detection Condition" = c("Not Detected", "Not Detected", NA),
    "Result Sample Fraction" = c("Filtered, lab", NA, NA),
    "Result Status ID" = "Final",
    "ResultTemperatureBasis" = NA,
    "Statistical Base Code" = NA,
    "ResultTimeBasis" = NA,
    "Result Value Type" = "Actual",
    "Result Analytical Method ID" = 120.1,
    "Result Analytical Method Context" = "USEPA",
    "Analysis Start Date" = as.Date("2017-03-02"),
    "Result Comment" = NA,
    check.names = FALSE
  )

  df_wqx2 <- df_wqx
  df_wqx2$Depth_Category <- NA
  df_wqx2[["Activity Start Date"]] = as.Date("2017-03-01")
  df_wqx2[["Analysis Start Date"]] = as.Date("2017-03-02")

  # Test WQX to wqdashboard
  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_results(
          df_wqx,
          "WQX", "WQdashboard",
          drop_extra_col = FALSE,
          date_format = "m/d/Y"
        )
      )
    ),
    df_wqd
  )

  expect_warning(
    suppressMessages(
      format_results(
        df_wqx,
        "WQX", "WQdashboard",
        drop_extra_col = FALSE,
        date_format = "m/d/Y"
      )
    ),
    regexp = "Unable to rename 23 columns"
  )

  # Test wqdashboard back to WQX
  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_results(
          df_wqd,
          "WQdashboard", "WQX",
          drop_extra_col = FALSE
        )
      )
    ),
    df_wqx2
  )

  expect_warning(
    suppressMessages(
      format_results(
        df_wqd,
        "WQdashboard", "WQX",
        drop_extra_col = FALSE
      )
    ),
    regexp = "Unable to rename 1 columns"
  )
})

# Test ERROR ----
test_that("format_results error messages", {
  df_test <- data.frame(
    "numbers" = c(1,2,3),
    "alphabet" = c("A", "B", "C")
  )

  expect_error(
    suppressMessages(
      format_results(df_test, "foobar", "foofy")
    ),
    regexp = "Invalid format"
  )
})
