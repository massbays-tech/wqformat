# Test MAINE ----
test_that("format_results converts ME_FOCB to ME_DEP", {
  # Input formats - test data from ME_FOCB in 3 formats
  df_wide1 <- data.frame(
    "SiteID" = c("BMR02", "EEB18", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Time" = c("12:32", "12:45", "10:22"),
    "Cloud Cover" = c(50, 50, 50),
    "Wind Speed" = c(3, 3, 2),
    "Wind Direction" = c(120, 150, 180),
    "Water Depth" = c(10.7, 3.2, NA),
    "Secchi Depth" = c(1.9, "BSV", NA),
    check.names = FALSE
  )

  df_wide2 <- data.frame(
    "SiteID" = c("BMR02", "EEB18", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Time" = c("12:32", "12:45", "10:22"),
    "Sample Depth m" = c(0.2, 0.2,0),
    "Temp °C" = c(11.3,11,14),
    "Sal psu" = c(27.5, 28, 28),
    "ODO mg/L" = c(9.3,9.3, 8.2),
    "ODO % sat" = c(100.7, 100.7, 94.9),
    "pH" = c(7.93, 7.93, 7.82),
    "Chlorophyll ug/L" = c(1.2, 1.2, 1.4),
    "Turbidity FNU" = c(2.7, 1.4, 2.4),
    check.names = FALSE
  )

  df_long <- data.frame(
    "Site ID" = c("BMR02", "EEB18", "HR2"),
    "Sample Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Lab" = c("UMWL", "UMWL", "UMWL"),
    "Analysis Date" = c("07/06/23", "07/06/23", "06/07/23"),
    "Parameter" = c(
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN"
    ),
    "Result" = c(0.22, 0.18, 0.28),
    "Unit" = c("MG/L", "MG/L", "MG/L"),
    "RL" = c(0.1, 0.1, 0.1),
    "MDL" = c(0.73, 0.73, 0.73),
    "Method" = c("SM4500NE_2021", "SM4500NE_2021", "SM4500NE_2021"),
    "Sample Depth m" = c(0.2, 0.2, 0.2),
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
    "SAMPLE_POINT_NAME" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18","HR2", "HR2", "HR2"
    ),
    "SAMPLE_DATE" = c(
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-24", "2023-05-24", "2023-05-24"
    ),
    "SAMPLE_TIME" = c(
      "12:32","12:32","12:32","12:32","12:32","12:45","12:45","12:45", "12:45",
      "12:45","10:22","10:22","10:22"
    ),
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
      "%", "BFT", "DEG TRUE", "M", "M", "%", "BFT", "DEG TRUE", "M", "M","%",
      "BFT", "DEG TRUE"
    ),
    check.names = FALSE
  )
  df_wide1_DEP[["SAMPLE_DATE"]] <- as.Date(df_wide1_DEP[["SAMPLE_DATE"]])
  df_wide1_DEP["PROJECT/SITE"] <- "FRIENDS OF CASCO BAY ALL SITES"
  df_wide1_DEP["SAMPLED_BY"] <- "FRIENDS OF CASCO BAY"
  missing_col <- setdiff(dep_col_order, colnames(df_wide1_DEP))
  df_wide1_DEP[missing_col] <- NA
  df_wide1_DEP <- df_wide1_DEP[,dep_col_order]

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
      "12:32","12:32","12:32","12:32","12:32","12:32","12:32","12:45", "12:45",
      "12:45","12:45","12:45","12:45","12:45","10:22","10:22","10:22", "10:22",
      "10:22","10:22","10:22"
    ),
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
    check.names = FALSE
  )
  df_wide2_DEP[["SAMPLE_DATE"]] <- as.Date(df_wide2_DEP[["SAMPLE_DATE"]])
  df_wide2_DEP["SAMPLE_DEPTH_UNIT"] <- "M"
  df_wide2_DEP["PROJECT/SITE"] <- "FRIENDS OF CASCO BAY ALL SITES"
  df_wide2_DEP["SAMPLED_BY"] <- "FRIENDS OF CASCO BAY"
  missing_col <- setdiff(dep_col_order, colnames(df_wide2_DEP))
  df_wide2_DEP[missing_col] <- NA
  df_wide2_DEP <- df_wide2_DEP[,dep_col_order]

  df_long_DEP <- data.frame(
    "SAMPLE_POINT_NAME" = c("BMR02", "EEB18", "HR2"),
    "SAMPLE_DATE" = c("2023-05-23", "2023-05-23", "2023-05-24"),
    "ANALYSIS_LAB" = c("UMWL", "UMWL", "UMWL"),
    "ANALYSIS_DATE" = c("2023-07-06", "2023-07-06", "2023-06-07"),
    "PARAMETER_NAME" = c("TN AS N", "TN AS N", "TN AS N"),
    "CONCENTRATION" = c(0.22, 0.18, 0.28),
    "PARAMETER_UNITS" = c("MG/L", "MG/L", "MG/L"),
    "REPORTING_LIMIT" = c(0.1, 0.1, 0.1),
    "MDL" = c(0.73, 0.73, 0.73),
    "TEST" = c("SM4500NE_2021", "SM4500NE_2021", "SM4500NE_2021"),
    "SAMPLE_DEPTH" = c(0.2, 0.2, 0.2),
    "SAMPLE_DEPTH_UNIT" = c("M", "M", "M"),
    "LAB_QUALIFIER" = c("J", "J", NA),
    check.names = FALSE
  )
  df_long_DEP[["SAMPLE_DATE"]] <- as.Date(df_long_DEP[["SAMPLE_DATE"]])
  df_long_DEP[["ANALYSIS_DATE"]] <- as.Date(df_long_DEP[["ANALYSIS_DATE"]])
  df_long_DEP["PROJECT/SITE"] <- "FRIENDS OF CASCO BAY ALL SITES"
  df_long_DEP["SAMPLED_BY"] <- "FRIENDS OF CASCO BAY"
  missing_col <- setdiff(dep_col_order, colnames(df_long_DEP))
  df_long_DEP[missing_col] <- NA
  df_long_DEP <- df_long_DEP[,dep_col_order]

  # test ME_FOCB to ME_DEP
  expect_equal(
    suppressMessages(
      format_results(df_wide1, "ME_FOCB", "ME_DEP", date_format="m/d/y")
    ),
    df_wide1_DEP
  )
  expect_equal(
    suppressMessages(
      format_results(df_wide2, "ME_FOCB", "ME_DEP", date_format="m/d/y")
    ),
    df_wide2_DEP
  )
  expect_equal(
    suppressMessages(
      format_results(df_long, "ME_FOCB", "ME_DEP", date_format="m/d/y")
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
    "Cloud Cover" = c(50, 50, 50),
    "Wind Speed" = c(3, 3, 2),
    "Wind Direction" = c(120, 150, 180),
    "Water Depth" = c(10.7, 3.2, NA),
    "Secchi Depth" = c(1.9, "BSV", NA),
    check.names = FALSE
  )

  df_wide2 <- data.frame(
    "SiteID" = c("BMR02", "EEB18", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Time" = c("12:32", "12:45", "10:22"),
    "Sample Depth m" = c(0.2, 0.2,0),
    "Temp °C" = c(11.3,11,14),
    "Sal psu" = c(27.5, 28, 28),
    "ODO mg/L" = c(9.3,9.3, 8.2),
    "ODO % sat" = c(100.7, 100.7, 94.9),
    "pH" = c(7.93, 7.93, 7.82),
    "Chlorophyll ug/L" = c(1.2, 1.2, 1.4),
    "Turbidity FNU" = c(2.7, 1.4, 2.4),
    check.names = FALSE
  )

  df_long <- data.frame(
    "Site ID" = c("BMR02", "EEB18", "HR2"),
    "Sample Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Lab" = c("UMWL", "UMWL", "UMWL"),
    "Analysis Date" = c("07/06/23", "07/06/23", "06/07/23"),
    "Parameter" = c(
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN"
    ),
    "Result" = c(0.22, 0.18, 0.28),
    "Unit" = c("MG/L", "MG/L", "MG/L"),
    "RL" = c(0.1, 0.1, 0.1),
    "MDL" = c(0.73, 0.73, 0.73),
    "Method" = c("SM4500NE_2021", "SM4500NE_2021", "SM4500NE_2021"),
    "Sample Depth m" = c(0.2, 0.2, 0.2),
    check.names = FALSE
  )

  # Expected output - MassWateR
  mwr_col_order <- c(
    "Monitoring Location ID", "Activity Type", "Activity Start Date",
    "Activity Start Time", "Activity Depth/Height Measure",
    "Activity Depth/Height Unit", "Activity Relative Depth Name",
    "Characteristic Name", "Result Value", "Result Unit", "Quantitation Limit",
    "QC Reference Value", "Result Measure Qualifier", "Result Attribute",
    "Sample Collection Method ID", "Project ID", "Local Record ID",
    "Result Comment"
  )

  df_wide1_mwr <- data.frame(
    "Monitoring Location ID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18","HR2", "HR2", "HR2"
    ),
    "Activity Start Date" = c(
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-24", "2023-05-24", "2023-05-24"
    ),
    "Activity Start Time" = c(
      "12:32","12:32","12:32","12:32","12:32","12:45","12:45","12:45", "12:45",
      "12:45","10:22","10:22","10:22"
    ),
    "Characteristic Name" = c(
      "Cloud Cover", "Wind Speed", "Wind Direction", "Depth",
      "Depth, Secchi disk depth", "Cloud Cover", "Wind Speed", "Wind Direction",
      "Depth", "Depth, Secchi disk depth", "Cloud Cover", "Wind Speed",
      "Wind Direction"
    ),
    "Result Value" = c(
      "50", "3", "120", "10.7", "1.9", "50", "3", "150", "3.2", "AQL", "50",
      "2", "180"
    ),
    "Result Unit" = c(
      "%", "BFT", "DEG TRUE", "m", "m", "%", "BFT", "DEG TRUE", "m", "m","%",
      "BFT", "DEG TRUE"
    ),
    check.names = FALSE
  )
  df_wide1_mwr[["Activity Start Date"]] <- as.Date(df_wide1_mwr[["Activity Start Date"]])
  df_wide1_mwr["Project ID"] <- "FRIENDS OF CASCO BAY ALL SITES"
  df_wide1_mwr["Result Measure Qualifier"] <- NA_character_
  df_wide1_mwr[["QC Reference Value"]] <- NA_integer_
  missing_col <- setdiff(mwr_col_order, colnames(df_wide1_mwr))
  df_wide1_mwr[missing_col] <- NA
  df_wide1_mwr <- df_wide1_mwr[,mwr_col_order]

  df_wide2_mwr <- data.frame(
    "Monitoring Location ID" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18",
      "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "HR2", "HR2", "HR2",
      "HR2", "HR2", "HR2", "HR2"
    ),
    "Activity Start Date" = c(
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23",
      "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-23", "2023-05-24",
      "2023-05-24", "2023-05-24", "2023-05-24", "2023-05-24", "2023-05-24",
      "2023-05-24"
    ),
    "Activity Start Time" = c(
      "12:32","12:32","12:32","12:32","12:32","12:32","12:32","12:45", "12:45",
      "12:45","12:45","12:45","12:45","12:45","10:22","10:22","10:22", "10:22",
      "10:22","10:22","10:22"
    ),
    "Activity Depth/Height Measure" = c(
      0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0,
      0, 0, 0, 0, 0, 0
    ),
    "Characteristic Name" = c(
      "Temperature, water", "Salinity", "Dissolved oxygen (DO)",
      "Dissolved oxygen saturation", "pH", "Chlorophyll a", "Turbidity",
      "Temperature, water", "Salinity", "Dissolved oxygen (DO)",
      "Dissolved oxygen saturation", "pH", "Chlorophyll a", "Turbidity",
      "Temperature, water", "Salinity", "Dissolved oxygen (DO)",
      "Dissolved oxygen saturation", "pH", "Chlorophyll a", "Turbidity"
    ),
    "Result Value" = c(
      11.3, 27.5, 9.3, 100.7, 7.93, 1.2, 2.7, 11, 28, 9.3, 100.7, 7.93, 1.2,
      1.4, 14, 28, 8.2, 94.9, 7.82, 1.4, 2.4
    ),
    "Result Unit" = c(
      "deg C", "PSU", "mg/l", "%", "None", "ug/l", "FNU", "deg C", "PSU",
      "mg/l", "%", "None", "ug/l", "FNU", "deg C", "PSU", "mg/l", "%", "None",
      "ug/l", "FNU"
    ),
    check.names = FALSE
  )
  df_wide2_mwr[["Activity Start Date"]] <- as.Date(df_wide2_mwr[["Activity Start Date"]])
  df_wide2_mwr["Project ID"] <- "FRIENDS OF CASCO BAY ALL SITES"
  df_wide2_mwr["Activity Depth/Height Unit"] <- "m"
  df_wide2_mwr["Result Measure Qualifier"] <- NA_character_
  df_wide2_mwr[["QC Reference Value"]] <- NA_integer_
  missing_col <- setdiff(mwr_col_order, colnames(df_wide2_mwr))
  df_wide2_mwr[missing_col] <- NA
  df_wide2_mwr <- df_wide2_mwr[,mwr_col_order]

  df_long_mwr <- data.frame(
    "Monitoring Location ID" = c("BMR02", "EEB18", "HR2"),
    "Activity Start Date" = c("2023-05-23", "2023-05-23", "2023-05-24"),
    "Characteristic Name" = c(
      "Total Nitrogen, mixed forms", "Total Nitrogen, mixed forms",
      "Total Nitrogen, mixed forms"
    ),
    "Result Value" = c(0.22, 0.18, 0.28),
    "Result Unit" = c("mg/l", "mg/l", "mg/l"),
    "Activity Depth/Height Measure" = c(0.2, 0.2, 0.2),
    "Activity Depth/Height Unit" = c("m", "m", "m"),
    "Quantitation Limit" = c(0.73, 0.73, 0.73),
    check.names = FALSE
  )
  df_long_mwr[["Activity Start Date"]] <- as.Date(df_long_mwr[["Activity Start Date"]])
  df_long_mwr["Project ID"] <- "FRIENDS OF CASCO BAY ALL SITES"
  df_long_mwr["Result Measure Qualifier"] <- NA_character_
  df_long_mwr[["QC Reference Value"]] <- NA_integer_
  missing_col <- setdiff(mwr_col_order, colnames(df_long_mwr))
  df_long_mwr[missing_col] <- NA
  df_long_mwr <- df_long_mwr[,mwr_col_order]

  # Test --- ME_FOCB to MassWateR
  # Test warnings
  expect_warning(
    expect_warning(
      suppressMessages(
        format_results(df_wide1, "ME_FOCB", "MassWateR", date_format="m/d/y")
      ),
      "Invalid variables in Characteristic Name: Cloud Cover, Wind Speed, Wind Direction"
    ),
    "Invalid variables in Result Unit: BFT, DEG TRUE"
  )

  # Test output
  expect_equal(
    suppressWarnings(
      suppressMessages(
        format_results(df_wide1, "ME_FOCB", "MassWateR", date_format="m/d/y")
      )
    ),
    df_wide1_mwr
  )
  expect_equal(
    suppressMessages(
      format_results(df_wide2, "ME_FOCB", "MassWateR", date_format="m/d/y")
    ),
    df_wide2_mwr
  )
  expect_equal(
    suppressMessages(
      format_results(df_long, "ME_FOCB", "MassWateR", date_format="m/d/y")
    ),
    df_long_mwr
  )
})

# test_that("format_results converts ME_DEP to MassWateR", {
#   # Input data - ME_DEP
#   dep_col_order <- c(
#     "PROJECT/SITE", "SAMPLE_POINT_NAME", "LAB_SAMPLE_ID", "SAMPLE_ID",
#     "ANALYSIS_LAB", "SAMPLE_DATE", "SAMPLE_TIME", "SAMPLE_TYPE", "QC_TYPE",
#     "PARAMETER_NAME", "CONCENTRATION", "LAB_QUALIFIER", "REPORTING_LIMIT",
#     "PARAMETER_UNITS", "TEST", "METER_ID", "ANALYSIS_DATE", "ANALYSIS_TIME",
#     "MDL", "RESULT_TYPE_CODE", "LAB_COMMENT", "SAMPLE_DEPTH",
#     "SAMPLE_DEPTH_UNIT", "SAMPLE_COLLECTION_METHOD", "SAMPLE_LOCATION",
#     "TREATMENT_STATUS", "PARAMETER_FILTERED", "SAMPLED_BY", "SAMPLE_COMMENTS",
#     "BATCH_ID", "PREP_DATE", "SAMPLE_DELIVERY_GROUP", "PARAMETER_QUALIFIER",
#     "VALIDATION_QUALIFIER", "VALIDATION_LEVEL", "VALIDATION_COMMENT",
#     "VALIDATION_COMMENT_TYPE"
#   )
#   df_DEP <- data.frame(
#     "SAMPLE_POINT_NAME" = c("BMR02", "EEB18", "HR2"),
#     "SAMPLE_DATE" = c("2023-05-23", "2023-05-23", "2023-05-24"),
#     "ANALYSIS_LAB" = c("UMWL", "UMWL", "UMWL"),
#     "ANALYSIS_DATE" = c("2023-07-06", "2023-07-06", "2023-06-07"),
#     "PARAMETER_NAME" = c("TN AS N", "TN AS N", "TN AS N"),
#     "CONCENTRATION" = c(0.22, 0.18, 0.28),
#     "PARAMETER_UNITS" = c("MG/L", "MG/L", "MG/L"),
#     "REPORTING_LIMIT" = c(0.1, 0.1, 0.1),
#     "MDL" = c(0.73, 0.73, 0.73),
#     "TEST" = c("SM4500NE_2021", "SM4500NE_2021", "SM4500NE_2021"),
#     "SAMPLE_DEPTH" = c(0.2, 0.2, 0.2),
#     "SAMPLE_DEPTH_UNIT" = c("M", "M", "M"),
#     "LAB_QUALIFIER" = c("J", "J", NA),
#     check.names = FALSE
#   )
#   df_DEP[["SAMPLE_DATE"]] <- as.Date(df_DEP[["SAMPLE_DATE"]])
#   df_DEP[["ANALYSIS_DATE"]] <- as.Date(df_DEP[["ANALYSIS_DATE"]])
#   df_DEP["PROJECT/SITE"] <- "FRIENDS OF CASCO BAY ALL SITES"
#   df_DEP["SAMPLED_BY"] <- "FRIENDS OF CASCO BAY"
#   missing_col <- setdiff(dep_col_order, colnames(df_DEP))
#   df_DEP[missing_col] <- NA
#   df_DEP <- df_DEP[,dep_col_order]
#
#   # Expected output - MassWateR
#   mwr_col_order <- c(
#     "Monitoring Location ID", "Activity Type", "Activity Start Date",
#     "Activity Start Time", "Activity Depth/Height Measure",
#     "Activity Depth/Height Unit", "Activity Relative Depth Name",
#     "Characteristic Name", "Result Value", "Result Unit", "Quantitation Limit",
#     "QC Reference Value", "Result Measure Qualifier", "Result Attribute",
#     "Sample Collection Method ID", "Project ID", "Local Record ID",
#     "Result Comment"
#   )
#   df_mwr <- data.frame(
#     "Monitoring Location ID" = c("BMR02", "EEB18", "HR2"),
#     "Activity Start Date" = c("2023-05-23", "2023-05-23", "2023-05-24"),
#     "Characteristic Name" = c(
#       "Total Nitrogen, mixed forms", "Total Nitrogen, mixed forms",
#       "Total Nitrogen, mixed forms"
#     ),
#     "Result Value" = c(0.22, 0.18, 0.28),
#     "Result Unit" = c("mg/l", "mg/l", "mg/l"),
#     "Activity Depth/Height Measure" = c(0.2, 0.2, 0.2),
#     "Activity Depth/Height Unit" = c("m", "m", "m"),
#     "Quantitation Limit" = c(0.73, 0.73, 0.73),
#     check.names = FALSE
#   )
#   df_mwr[["Activity Start Date"]] <- as.Date(df_mwr[["Activity Start Date"]])
#   df_mwr["Project ID"] <- "FRIENDS OF CASCO BAY ALL SITES"
#   df_mwr["Result Measure Qualifier"] <- NA_character_
#   df_mwr[["QC Reference Value"]] <- NA_integer_
#   missing_col <- setdiff(mwr_col_order, colnames(df_mwr))
#   df_mwr[missing_col] <- NA
#   df_mwr <- df_mwr[,mwr_col_order]
#
#   # Test - ME_DEP to MassWateR
#   expect_equal(
#     suppressWarnings(
#       format_results(
#         df_DEP, "ME_DEP", "MassWateR", show_messages = FALSE
#       )
#     ),
#     df_mwr
#   )
# })

# Test MASSACHUSETTS ----

# Test RHODE ISLAND ----

# Test WQDASHBOARD ----
