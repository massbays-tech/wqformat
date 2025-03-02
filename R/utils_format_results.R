#' Results from Massachusetts Blackstone River Coalition
#'
#' @description Helper function for `format_results` that preformats result
#'  data from the Blackstone River Coalition (MA_BRC).
#'    * Adds columns for DATE, TIME, SAMPLE_TYPE
#'
#' @param df Input dataframe.
#' @param date_format String. Date format. Uses the same formatting as
#'  `lubridate`. Default value "m/d/Y".
#' @param tz String. Timezone. Default value "America/New York".
#'
#' @returns Updated dataframe.
results_from_MA_BRC <- function(
    df, date_format="Y-m-d H:M", tz="America/New_York") {
  df <- df %>%
    col_to_date("DATE_TIME", date_format=date_format, tz=tz) %>%
    dplyr::mutate("DATE" = as.Date(.data$DATE_TIME)) %>%
    dplyr::mutate("TIME" = format(.data$DATE_TIME,"%H:%M")) %>%
    dplyr::mutate(
      "SAMPLE_TYPE" = dplyr::case_when(
        grepl("Field Blank", .data$PARAMETER, fixed=TRUE) ~ "Field Blank",
        grepl("Lab Blank", .data$PARAMETER, fixed=TRUE) ~ "Lab Blank",
        grepl("Replicate", .data$PARAMETER, fixed=TRUE) ~ "Replicate",
        TRUE ~ "Grab"
      )
    )
  return(df)
}

#' Results to Massachusetts Blackstone River Coalition
#'
#' @description Helper function for `format_results` that formats result data for
#'    the Blackstone River Coalition (MA_BRC).
#'    * Uses DATE, TIME columns to fill DATE_TIME column
#'    * uses SAMPLE_TYPE column to update PARAMETER column
#'    * Adds columns UNIQUE_ID
#'    * Removes DATE, TIME, and SAMPLE_TYPE columns
#'
#' @param df Input dataframe.
#'
#' @returns Updated dataframe.
results_to_MA_BRC <- function(df) {

  df <- df %>%
    dplyr::mutate("DATE_TIME" = paste(.data$DATE, .data$TIME)) %>%
    dplyr::mutate(
      "PARAMETER" = dplyr::if_else(
        .data$SAMPLE_TYPE %in% c("Field Blank", "Lab Blank", "Replicate"),
        paste(.data$PARAMETER, .data$SAMPLE_TYPE),
        .data$PARAMETER
      )
    ) %>%
    dplyr::mutate(
      "UNIQUE_ID" = dplyr::case_when(
        .data$PARAMETER == "Air Temperature" ~ "TAC",
        .data$PARAMETER == "Conductivity" ~ "COND",
        .data$PARAMETER == "Conductivity Replicate" ~ "CONDR",
        .data$PARAMETER == "Dissolved Oxy Saturation" ~ "OXYSAT",
        .data$PARAMETER == "Dissolved Oxygen" ~ "DOXY",
        .data$PARAMETER == "E. coli" ~ "ECOL",
        .data$PARAMETER == "E. coli Field Blank" ~ "ECOLFB",
        .data$PARAMETER == "E. coli Lab Blank" ~ "ECOLB",
        .data$PARAMETER == "E. coli Replicate" ~ "ECOLR",
        .data$PARAMETER == "Nitrate" ~ "NO3",
        .data$PARAMETER == "Nitrate Replicate" ~ "NO3R",
        .data$PARAMETER == "Orthophosphate" ~ "PO4",
        .data$PARAMETER == "Orthophosphate Replicate" ~ "PO4R",
        .data$PARAMETER == "Turbidity" ~ "TURB1",
        .data$PARAMETER == "Turbidity Replicate" ~ "TURBR",
        .data$PARAMETER == "Water Temperature" ~ "TWC",
        TRUE ~ ""
      )
    ) %>%
    dplyr::mutate(
      "UNIQUE_ID" = paste(
        .data$SITE_BRC_CODE, .data$DATE_TIME, .data$UNIQUE_ID,
        sep = "_"
      )
    ) %>%
    dplyr::relocate("DATE_TIME", .after = "SITE_BRC_CODE") %>%
    dplyr::select(!dplyr::any_of(c("DATE", "TIME", "SAMPLE_TYPE")))

  return(df)
}
