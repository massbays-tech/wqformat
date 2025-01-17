test_that("format_results works for Maine data formats", {
  # Input formats - test data from ME_FOCB in 3 formats
  dat_wide1 <- data.frame(
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

  dat_wide2 <- data.frame(
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

  dat_long <- data.frame(
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
  dat_wide1_DEP <- data.frame(
    "PROJECT/SITE" = c(
      "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
      "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
      "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
      "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
      "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
      "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
      "FRIENDS OF CASCO BAY ALL SITES"
    ),
    "SAMPLE_POINT_NAME" = c(
      "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18", "EEB18",
      "EEB18", "EEB18","HR2", "HR2", "HR2"
    ),
    "LAB_SAMPLE_ID" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLE_ID" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "ANALYSIS_LAB" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLE_DATE" = c(
      as.Date("2023-05-23"), as.Date("2023-05-23"), as.Date("2023-05-23"),
      as.Date("2023-05-23"), as.Date("2023-05-23"), as.Date("2023-05-23"),
      as.Date("2023-05-23"), as.Date("2023-05-23"), as.Date("2023-05-23"),
      as.Date("2023-05-23"), as.Date("2023-05-24"), as.Date("2023-05-24"),
      as.Date("2023-05-24")
    ),
    "SAMPLE_TIME" = c(
      "12:32","12:32","12:32","12:32","12:32","12:45","12:45","12:45", "12:45",
      "12:45","10:22","10:22","10:22"
    ),
    "SAMPLE_TYPE" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "QC_TYPE" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "PARAMETER_NAME" = c(
      "CLOUD COVER", "WSPD", "WDIR", "DEPTH", "SECCHI", "CLOUD COVER", "WSPD",
      "WDIR", "DEPTH", "SECCHI", "CLOUD COVER", "WSPD", "WDIR"
    ),
    "CONCENTRATION" = c(
      '50', '3', '120', '10.7', '1.9', '50', '3', '150', '3.2', 'BSV', '50',
      '2', '180'
    ),
    "LAB_QUALIFIER" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "G", NA, NA, NA),
    "REPORTING_LIMIT" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "PARAMETER_UNITS" = c(
      "%", "BFT", "DEG TRUE", "M", "M", "%", "BFT", "DEG TRUE", "M", "M","%",
      "BFT", "DEG TRUE"
    ),
    "TEST" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "METER_ID" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "ANALYSIS_DATE" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "ANALYSIS_TIME" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "MDL" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "RESULT_TYPE_CODE" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "LAB_COMMENT" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLE_DEPTH" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLE_DEPTH_UNIT" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLE_COLLECTION_METHOD" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLE_LOCATION" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "TREATMENT_STATUS" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "PARAMETER_FILTERED" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLED_BY" = c(
      "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
      "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
      "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
      "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
      "FRIENDS OF CASCO BAY"
    ),
    "SAMPLE_COMMENTS" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "BATCH_ID" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "PREP_DATE" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "SAMPLE_DELIVERY_GROUP" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "PARAMETER_QUALIFIER" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "VALIDATION_QUALIFIER" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "VALIDATION_LEVEL" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "VALIDATION_COMMENT" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    "VALIDATION_COMMENT_TYPE" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    check.names = FALSE
  )

  dat_wide2_DEP <- data.frame(
    "SiteID" = c("BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02",
                 "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "HR2",
                 "HR2", "HR2", "HR2", "HR2", "HR2", "HR2"),
    "Sample Date" = c("05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23",
                      "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23",
                      "05/23/23", "05/23/23", "05/23/23", "05/24/23", "05/24/23","05/24/23",
                      "05/24/23","05/24/23", "05/24/23","05/24/23"),
    "Time" = c("12:32","12:32","12:32","12:32","12:32","12:32","12:32","12:45",
               "12:45","12:45","12:45","12:45","12:45","12:45","10:22","10:22","10:22",
               "10:22","10:22","10:22","10:22"),
    "Sample Depth m" = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                         0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0, 0),
    "Sample Depth Unit" = c("m", "m", "m", "m", "m", "m", "m", "m", "m", "m",
                            "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m"),
    "Project" = c("FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES"),
    "Sampled By" = c("FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY"),
    "Parameter" = c("Temp", "Sal", "ODO", "ODO % sat", "pH", "Chlorophyll",
                    "Turbidity", "Temp", "Sal", "ODO", "ODO % sat", "pH", "Chlorophyll",
                    "Turbidity", "Temp", "Sal", "ODO", "ODO % sat", "pH", "Chlorophyll",
                    "Turbidity"),
    "Result" = c(11.3, 27.5, 9.3, 100.7, 7.93, 1.2, 2.7, 11, 28, 9.3, 100.7,
                 7.93, 1.2, 1.4, 14, 28, 8.2, 94.9, 7.82, 1.4, 2.4),
    "Unit" = c("deg C", "psu", "mg/L", "%", "STU", "ug/L", "FNU", "deg C",
               "psu", "mg/L", "%", "STU", "ug/L", "FNU", "deg C", "psu", "mg/L", "%",
               "STU", "ug/L", "FNU"),
    "Qualifier" = c(NA, NA, NA, NA, NA, "J", NA, NA, NA, NA, NA, NA, "J", NA,
                    NA, NA, NA, NA, NA, "J", NA),
    check.names = FALSE
  )

  dat_long_DEP <- data.frame(
    "Site ID" = c("BMR02", "EEB18", "HR2"),
    "Sample Date" = c(lubridate::ymd("20230523"), lubridate::ymd("20230523"),
                      lubridate::ymd("20230524")),
    "Lab" = c("UMWL", "UMWL", "UMWL"),
    "Analysis Date" = c(lubridate::ymd("20230706"), lubridate::ymd("20230706"),
                        lubridate::ymd("20230607")),
    "Parameter" = c("TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
                    "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
                    "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN"),
    "Result" = c(0.22, 0.18, 0.28),
    "Unit" = c("MG/L", "MG/L", "MG/L"),
    "RL" = c(0.1, 0.1, 0.1),
    "MDL" = c(0.73, 0.73, 0.73),
    "Method" = c("SM4500NE_2021", "SM4500NE_2021", "SM4500NE_2021"),
    "Sample Depth m" = c(0.2, 0.2, 0.2),
    "Sample Depth Unit" = c('m', 'm', 'm'),
    "Project" = c("FRIENDS OF CASCO BAY ALL SITES",
                  "FRIENDS OF CASCO BAY ALL SITES", "FRIENDS OF CASCO BAY ALL SITES"),
    "Sampled By" = c("FRIENDS OF CASCO BAY", "FRIENDS OF CASCO BAY",
                     "FRIENDS OF CASCO BAY"),
    'Qualifier' = c('J', 'J', NA),
    check.names = FALSE
  )

  # test ME_FOCB to ME_DEP
  expect_equal(
    suppressWarnings(
      format_results(
        dat_wide1, "ME_FOCB", "ME_DEP", date_format="m/d/y",
        show_messages = FALSE
      )
    ),
    dat_wide1_DEP
  )

  # expect_equal(
  #   suppressWarnings(
  #     format_results(
  #       dat_wide2, "ME_FOCB", "ME_DEP", date_format="m/d/y",
  #       show_messages = FALSE
  #     )
  #   ),
  #   dat_wide2_DEP
  # )
  #
  # expect_equal(
  #   suppressWarnings(
  #     format_results(
  #       dat_long, "ME_FOCB", "ME_DEP", date_format="m/d/y",
  #       show_messages = FALSE
  #     )
  #   ),
  #   dat_long_DEP
  # )

  # Expected output - ME_FOCB to MassWateR

  # test ME_FOCB to MassWateR

  # test ME_DEP to MassWateR
})
