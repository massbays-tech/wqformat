#' Prep ME_FOCB Results
#'
#' @description Pre-formats result data for Maine Friends of Casco Bay (ME_FOCB).
#'   Adds "Sample Depth Unit" column and pivots table from wide to long.
#'
#' @param df Dataframe.
#'
#' @returns Updated dataframe.
prep_ME_FOCB <- function(df){

  # Add column for sample depth units
  if ("Sample Depth m" %in% colnames(df)) {
    df <- dplyr::mutate(df, "Sample Depth Unit" = "m")
  }

  # Check if already long
  if ("Parameter" %in% colnames(df)) {
    return(df)
  }

  # Pivot table longer, update parameter & unit names
  keep_col <- c("SiteID", "Site ID", "Sample ID", "Date", "Time",
    "Sample Depth", "Sample Depth m", "Sample Depth Unit")

  df <- df %>%
    tidyr::pivot_longer(
      !dplyr::any_of(keep_col),
      names_to = "Parameter",
      values_to = "Result",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(
      Parameter = gsub(".", " ", Parameter, fixed = TRUE)
    )

  # Update parameters, units
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

  return(df)
}
