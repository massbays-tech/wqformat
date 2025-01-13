test_that("prep_ME_FOCB works", {
  # Input formats - test data from ME_FOCB in 3 formats
  df_wide1 <- data.frame(
    "SiteID" = c("BMR02", "EEB18", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/24/23"),
    "Time" = c("12:32", "12:45", "10:22"),
    "Cloud Cover" = c(50, 50, 50),
    "Wind Speed" = c(3, 3, 2),
    "Wind Direction" = c(120, 150, 180),
    "Water Depth" = c(10.7, 3.2, NA),
    "Secchi Depth" = c(1.9, 1.8, NA),
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
    "Analysis Date" = c("07/06/23", "07/06/23", "07/07/23"),
    "Parameter" = c("TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN",
      "TOTAL NITROGEN MIXED FORMS (NH3, NH4, ORGANIC, NO2, AND NO3) AS NITROGEN"),
    "Result" = c(0.22, 0.18, 0.28),
    "Unit" = c("MG/L", "MG/L", "MG/L"),
    "RL" = c(0.1, 0.1, 0.1),
    "MDL" = c(0.73, 0.73, 0.73),
    "Method" = c("SM4500NE_2021", "SM4500NE_2021", "SM4500NE_2021"),
    "Sample Depth m" = c(0.2, 0.2, 0.2),
    check.names = FALSE
  )

  # Expected outputs
  df_wide1_b <- data.frame(
    "SiteID" = c("BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "EEB18", "EEB18",
      "EEB18", "EEB18", "EEB18","HR2", "HR2", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23",
      "05/23/23","05/23/23", "05/23/23","05/23/23", "05/23/23","05/24/23",
      "05/24/23","05/24/23"),
    "Time" = c("12:32","12:32","12:32","12:32","12:32","12:45","12:45","12:45",
      "12:45","12:45","10:22","10:22","10:22"),
    "Parameter" = c("Cloud Cover", "Wind Speed", "Wind Direction",
      "Water Depth", "Secchi Depth", "Cloud Cover", "Wind Speed",
      "Wind Direction", "Water Depth", "Secchi Depth","Cloud Cover",
      "Wind Speed", "Wind Direction"),
    "Result" = c(50, 3, 120, 10.7, 1.9, 50, 3, 150, 3.2, 1.8, 50, 2, 180),
    "Unit" = c("%", "BFT", "DEG TRUE", "m", "m", "%", "BFT", "DEG TRUE", "m",
      "m","%", "BFT", "DEG TRUE"),
    check.names = FALSE
  )

  df_wide2_b <- data.frame(
    "SiteID" = c("BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02", "BMR02",
      "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "EEB18", "HR2",
      "HR2", "HR2", "HR2", "HR2", "HR2", "HR2"),
    "Date" = c("05/23/23", "05/23/23", "05/23/23", "05/23/23", "05/23/23",
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
    "Parameter" = c("Temp", "Sal", "ODO", "ODO % sat", "pH", "Chlorophyll",
      "Turbidity", "Temp", "Sal", "ODO", "ODO % sat", "pH", "Chlorophyll",
      "Turbidity", "Temp", "Sal", "ODO", "ODO % sat", "pH", "Chlorophyll",
      "Turbidity"),
    "Result" = c(11.3, 27.5, 9.3, 100.7, 7.93, 1.2, 2.7, 11, 28, 9.3, 100.7,
      7.93, 1.2, 1.4, 14, 28, 8.2, 94.9, 7.82, 1.4, 2.4),
    "Unit" = c("deg C", "psu", "mg/L", "%", "STU", "ug/L", "FNU", "deg C",
      "psu", "mg/L", "%", "STU", "ug/L", "FNU", "deg C", "psu", "mg/L", "%",
      "STU", "ug/L", "FNU"),
    check.names = FALSE
  )

  df_long2 <- df_long %>%
    dplyr::mutate("Sample Depth Unit" = "m")

  # Test
  expect_equal(prep_ME_FOCB(df_wide1), df_wide1_b)
  expect_equal(prep_ME_FOCB(df_wide2), df_wide2_b)
  expect_equal(prep_ME_FOCB(df_long), df_long2)
})
