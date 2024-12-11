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
    "Secchi Depth" = c(1.9, 1.8, NA)
  )

  # df_wide2 <- data.frame()
  #
  # df_long <- data.frame()

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
      "m","%", "BFT", "DEG TRUE")
  )

  # Test
  expect_equal(prep_ME_FOCB(df_wide1), df_wide1_b)
})
