#' Prep ME_FOCB Results
#'
#' @description Pre-formats result data for Maine Friends of Casco Bay (ME_FOCB).
#'   Adds "Sample Depth Unit" column and pivots table from wide to long.
#'
#' @param df Dataframe.
#' @param date_format String. Date format, uses lubridate. (word better)
#'
#' @returns Updated dataframe.
prep_ME_FOCB <- function(df, date_format = "m/d/y"){

  # Add columns
  if ("Sample Depth m" %in% colnames(df)) {
    df <- dplyr::mutate(df, "Sample Depth Unit" = "m")
  }

  df <- df %>%
    dplyr::mutate("Project" = "FRIENDS OF CASCO BAY ALL SITES") %>%
    dplyr::mutate("Sampled By" = "FRIENDS OF CASCO BAY")

  # Check if table is long, else make long
  if (!"Parameter" %in% colnames(df)) {
    # Pivot table longer, update parameter & unit names
    keep_col <- c("SiteID", "Site ID", "Sample ID", "Date", "Time",
                  "Sample Depth", "Sample Depth m", "Sample Depth Unit",
                  "Project", "Sampled By")

    # Set table to numeric before pivot to avoid errors from "BSV" score
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::pivot_longer(
        !dplyr::any_of(keep_col),
        names_to = "Parameter",
        values_to = "Result",
        values_drop_na = TRUE
      )

    # Return columns to numeric...
    for (field in colnames(df)) {
      df <- col_to_numeric(df, field)
    }

    # Add parameters, units
    df <- as.data.frame(df)  %>%
      dplyr::mutate(
        "Unit" = dplyr::case_when(
          grepl("mg/L", .data$Parameter) ~ "mg/L",
          grepl("ug/L", .data$Parameter) ~ "ug/L",
          grepl("FNU", .data$Parameter) ~ "FNU",
          grepl("psu", .data$Parameter) ~ "psu",
          grepl("%", .data$Parameter) | .data$Parameter == "Cloud Cover" ~ "%",
          .data$Parameter == "Wind Speed" ~ "BFT",
          .data$Parameter == "Wind Direction" ~ "DEG TRUE",
          .data$Parameter %in% c("Water Depth", "Secchi Depth") ~ "m",
          .data$Parameter == "pH" ~ "STU",
          grepl("Temp", .data$Parameter) &
            grepl("C", .data$Parameter) ~ "deg C",
          TRUE ~ NA
        )
      ) %>%
      dplyr::mutate(
        "Parameter" = dplyr::case_when(
          .data$Parameter == "ODO mg/L" ~ "ODO",
          .data$Parameter == "Sal psu" ~ "Sal",
          grepl("Chlorophyll", .data$Parameter) ~ "Chlorophyll",
          grepl("Turbidity", .data$Parameter) ~ "Turbidity",
          grepl("Temp", .data$Parameter) &
            grepl("C", .data$Parameter) ~ "Temp",
          TRUE ~ .data$Parameter
        )
      )
  }

  # Calc gap between Sample Date & Analysis Date
  if ("Date" %in% colnames(df)) {
    df <- dplyr::rename(df, "Sample Date" = "Date")
  }

  chk <- "Sample Date" %in% colnames(df) & "Analysis Date" %in% colnames(df)
  if (chk) {
    df <- col_to_date(df, "Sample Date", date_format)
    df <- col_to_date(df, "Analysis Date", date_format)

    df <- df %>%
      dplyr::rename(
        c("Sample_Date" = "Sample Date",
          "Analysis_Date" = "Analysis Date")
      ) %>%
      dplyr::mutate(
        "temp_gap" = as.numeric(.data$Analysis_Date - .data$Sample_Date)
      ) %>%
      dplyr::rename(
        c("Sample Date" = "Sample_Date",
          "Analysis Date" = "Analysis_Date")
      )
  } else {
    df$temp_gap <- 0
  }

  # Add qualifiers
  df <- df %>%
    dplyr::mutate(
      "Qualifier" = dplyr::case_when(
        .data$Parameter == "Chlorophyll" ~ "J",
        .data$Parameter == "Secchi Depth" & .data$Result == "BSV" ~ "G",
        grepl("NITROGEN", .data$Parameter, fixed=TRUE) &
          .data$temp_gap > 28 ~ "J",
        TRUE ~ NA
      )
    ) %>%
    dplyr::select(!"temp_gap")

  return(df)
}
