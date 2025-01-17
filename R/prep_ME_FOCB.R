#' Prep ME_FOCB Results
#'
#' @description Pre-formats result data for Maine Friends of Casco Bay (ME_FOCB).
#'   Adds "Sample Depth Unit" column and pivots table from wide to long.
#'
#' @param df Dataframe.
#'
#' @returns Updated dataframe.
prep_ME_FOCB <- function(df, date_format = 'm/d/y'){

  # Add columns
  if ("Sample Depth m" %in% colnames(df)) {
    df <- dplyr::mutate(df, "Sample Depth Unit" = "m")
  }

  df <- df %>%
    dplyr::mutate(Project = "FRIENDS OF CASCO BAY ALL SITES") %>%
    dplyr::mutate(`Sampled By` = "FRIENDS OF CASCO BAY")

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
        Unit = dplyr::case_when(
          grepl("mg/L", Parameter) ~ "mg/L",
          grepl("ug/L", Parameter) ~ "ug/L",
          grepl("FNU", Parameter) ~ "FNU",
          grepl("psu", Parameter) ~ "psu",
          grepl("%", Parameter) | Parameter == "Cloud Cover" ~ "%",
          Parameter == "Wind Speed" ~ "BFT",
          Parameter == "Wind Direction" ~ "DEG TRUE",
          Parameter %in% c("Water Depth", "Secchi Depth") ~ "m",
          Parameter == "pH" ~ "STU",
          grepl("Temp", Parameter) & grepl("C", Parameter) ~ "deg C",
          TRUE ~ NA
        )
      ) %>%
      dplyr::mutate(
        Parameter = dplyr::case_when(
          Parameter == "ODO mg/L" ~ "ODO",
          Parameter == "Sal psu" ~ "Sal",
          grepl("Chlorophyll", Parameter) ~ "Chlorophyll",
          grepl("Turbidity", Parameter) ~ "Turbidity",
          grepl("Temp", Parameter) & grepl("C", Parameter) ~ "Temp",
          TRUE ~ Parameter
        )
      )
  }

  # Calc gap between Sample Date & Analysis Date
  if ("Date" %in% colnames(df)) {
    df <- dplyr::rename(df, "Sample Date" = Date)
  }

  chk <- "Sample Date" %in% colnames(df) & "Analysis Date" %in% colnames(df)
  if (chk) {
    df <- format_date(df, "Sample Date", date_format)
    df <- format_date(df, "Analysis Date", date_format)

    df <- df %>%
      dplyr::rename(
        c(Sample_Date = 'Sample Date',
          Analysis_Date = "Analysis Date")
      ) %>%
      dplyr::mutate(temp_gap = as.numeric(Analysis_Date - Sample_Date)) %>%
      dplyr::rename(
        c('Sample Date' = Sample_Date,
          'Analysis Date' = Analysis_Date)
      )
  } else {
    df$temp_gap <- 0
  }

  # Add qualifiers
  df <- df %>%
    dplyr::mutate(
      Qualifier = dplyr::case_when(
        Parameter == "Chlorophyll" ~ "J",
        Parameter == "Secchi Depth" & Result == "BSV" ~ "G",
        grepl("NITROGEN", Parameter, fixed=TRUE) &
          temp_gap > 28 ~ "J",
        TRUE ~ NA
      )
    ) %>%
    dplyr::select(!temp_gap)

  return(df)
}
