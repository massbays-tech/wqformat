test_that("results_from_MA_BRC works", {
  df_in <- data.frame(
    "DATE_TIME" = c(
      "2024-02-04 12:56", "2024-02-05 15:25", "2024-02-06 7:24",
      "2024-02-07 17:30", "2024-03-06 18:20"),
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
    "PARAMETER" = c(
      "Nitrate", "Nitrate Replicate", "E. coli", "E. coli Field Blank",
      "E. coli Lab Blank"
    ),
    "DATE" = c(
      "2024-02-04", "2024-02-05", "2024-02-06", "2024-02-07", "2024-03-06"
    ),
    "TIME" = c("12:56", "15:25", "07:24", "17:30", "18:20"),
    "SAMPLE_TYPE" = c("Grab", "Replicate", "Grab", "Field Blank", "Lab Blank")
  )
  df_out[["DATE_TIME"]] <- as.POSIXct(
    df_out[["DATE_TIME"]], tz="America/New_York"
  )
  df_out[["DATE"]] <- as.Date(df_out[["DATE"]])

  expect_equal(results_from_MA_BRC(df_in), df_out)
})

test_that("results_to_MA_BRC works", {
  df <- data.frame(
    "SITE_BRC_CODE" = c(1,2,3,4,5),
    "PARAMETER" = c("Nitrate", "Nitrate", "E. coli", "E. coli", "E. coli"),
    "DATE" = c(
      "2024-02-04", "2024-02-05", "2024-02-06", "2024-02-07", "2024-03-06"
    ),
    "TIME" = c("12:56", "15:25", "07:24", "17:30", "18:20"),
    "SAMPLE_TYPE" = c("Grab", "Replicate", "Grab", "Field Blank", "Lab Blank")
  )

  # EXPAND TEST TO CHECK EVERY PARAMETER/CODE COMBO!!

  df_out <- data.frame(
    "SITE_BRC_CODE" = c(1,2,3,4,5),
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
