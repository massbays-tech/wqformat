#' Prepare MassWateR Results
#'
#' @description Helper function for `format_results`. Standardizes result data
#'    from MassWateR.
#'  * Transfers "BDL" and "AQL" values from `Result Value` to
#'    `Result Measure Qualifier`.
#'  * Data in `QC Reference Value` is copied to its own row.
#'
#' @param df Dataframe.
#'
#' @returns Updated dataframe.
prep_MassWateR_results <- function(df) {
  # Prep data
  df[["Result Value"]] <- as.character(df[["Result Value"]])
  df[["QC Reference Value"]] <- as.character(df[["QC Reference Value"]])

  # Transfer QC Reference Value to own row
  qc_duplicate <- c(
    "Quality Control Sample-Lab Duplicate",
    "Quality Control-Meter Lab Duplicate"
  )

  df <- df %>%
    dplyr::mutate(
      "QC Reference Value" = dplyr::if_else(
        .data[["Activity Type"]] %in% qc_duplicate &
          !is.na(.data[["QC Reference Value"]]),
        paste0("NA|", .data[["QC Reference Value"]]),
        .data[["QC Reference Value"]]
      )
    ) %>%
    tidyr::separate_longer_delim(dplyr::all_of("QC Reference Value"), "|") %>%
    dplyr::mutate(
      "QC Reference Value" = dplyr::if_else(
        .data[["QC Reference Value"]] == "NA",
        NA,
        .data[["QC Reference Value"]]
      )
    ) %>%
    dplyr::mutate(
      "Result Value" = dplyr::if_else(
        .data[["Activity Type"]] %in% qc_duplicate &
          !is.na(.data[["QC Reference Value"]]),
        .data[["QC Reference Value"]],
        .data[["Result Value"]]
      )
    ) %>%
    dplyr::mutate(
      "QC Reference Value" = dplyr::if_else(
        .data[["Activity Type"]] %in% qc_duplicate,
        NA,
        .data[["QC Reference Value"]]
      )
    )

  # Update qualifiers, result value
  df <- df %>%
    dplyr::mutate(
      "Result Measure Qualifier" = dplyr::if_else(
        .data[["Result Value"]] %in% c("BDL", "AQL"),
        .data[["Result Value"]],
        .data[["Result Measure Qualifier"]]
      )
    ) %>%
    dplyr::mutate(
      "Result Value" = dplyr::if_else(
        .data[["Result Value"]] %in% c("BDL", "AQL"),
        NA,
        .data[["Result Value"]]
      )
    ) %>%
    col_to_numeric("Result Value") %>%
    col_to_numeric("QC Reference Value")

  return(df)
}

#' Results to MassWateR
#'
#' @description Helper function for `format_results`. Formats result data for
#'    MassWateR.
#'  * Sets "Result Value" to "BDL" or "AQL" as needed
#'  * Sets "Result Measure Qualifier" to Q or NA
#'  * Transfers duplicate values to "QC Reference Value"
#'
#' @param df Dataframe.
#' @param in_format String. Input format.
#'
#' @returns Updated dataframe.
results_to_MassWateR <- function(df, in_format) {
  # Prep data
  df[["Result Value"]] <- as.character(df[["Result Value"]])
  df[["QC Reference Value"]] <- as.character(df[["QC Reference Value"]])

  df_colnames <- colnames(df)

  # Update qualifiers, result value
  qual <- find_var_names(varnames_qualifiers, in_format, "Flag")
  df <- rename_all_var(
    df,
    "Result Measure Qualifier",
    qual$old_names,
    qual$new_names
  )

  df <- df %>%
    dplyr::mutate(
      "Result Value" = dplyr::case_when(
        .data[["Result Measure Qualifier"]] == "Non-Detect" ~ "BDL",
        .data[["Result Measure Qualifier"]] == "Over-Detect" ~ "AQL",
        TRUE ~ .data[["Result Value"]]
      )
    ) %>%
    dplyr::mutate(
      "Result Measure Qualifier" = dplyr::case_when(
        .data[["Result Measure Qualifier"]] %in%
          c("Not Reviewed", "Suspect") ~ "Q",
        .data[["Result Measure Qualifier"]] %in%
          c("Over-Detect", "Non-Detect", "Pass") ~ NA,
        TRUE ~ .data[["Result Measure Qualifier"]]
      )
    )
  warn_invalid_var(df, "Result Measure Qualifier", "Q")

  # Transfer QC duplicates to QC Reference Value
  qc_duplicate <- c(
    "Quality Control Sample-Lab Duplicate",
    "Quality Control-Meter Lab Duplicate"
  )
  group_col <- setdiff(df_colnames, "Result Value")

  chk <- df[["Activity Type"]] %in% qc_duplicate &
    is.na(df[["QC Reference Value"]])
  df1 <- df[which(chk), ] # data to group
  df2 <- df[which(!chk), ] # data to leave as-is

  df1 <- df1 %>%
    dplyr::group_by_at(group_col) %>%
    dplyr::summarize(
      "Result Value" = stringr::str_c(.data[["Result Value"]], collapse = "|"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      "QC Reference Value" = dplyr::if_else(
        stringr::str_count(.data[["Result Value"]], "\\|") == 1,
        stringr::str_split_i(.data[["Result Value"]], "\\|", 2),
        .data[["QC Reference Value"]]
      )
    ) %>%
    dplyr::mutate(
      "Result Value" = dplyr::if_else(
        stringr::str_count(.data[["Result Value"]], "\\|") == 1,
        stringr::str_split_i(.data[["Result Value"]], "\\|", 1),
        .data[["Result Value"]]
      )
    ) %>%
    tidyr::separate_longer_delim(dplyr::all_of("Result Value"), "|")

  df <- rbind(df1, df2)
  df <- as.data.frame(df) %>%
    dplyr::arrange(
      .data[["Activity Start Date"]],
      .data[["Activity Start Time"]]
    ) %>%
    dplyr::select(dplyr::all_of(df_colnames)) %>%
    col_to_numeric("Result Value") %>%
    col_to_numeric("QC Reference Value")

  return(df)
}

#' Prepare Blackstone River Coalition Results
#'
#' @description Helper function for `format_results`. Standardizes result data
#'    from the Blackstone River Coalition (MA_BRC).
#'    * Adds columns for DATE, TIME, SAMPLE_TYPE
#'
#' @param df Input dataframe.
#' @param date_format String. Date format. Uses the same formatting as
#'  `lubridate`. Default value "m/d/Y".
#' @param tz String. Timezone. Default value "America/New York".
#'
#' @returns Updated dataframe.
prep_MA_BRC_results <- function(
    df, date_format = "Y-m-d H:M", tz = "America/New_York") {
  df <- df %>%
    col_to_date("DATE_TIME", date_format = date_format, tz = tz) %>%
    dplyr::mutate("DATE" = as.Date(.data$DATE_TIME)) %>%
    dplyr::mutate("TIME" = format(.data$DATE_TIME, "%H:%M")) %>%
    dplyr::mutate(
      "SAMPLE_TYPE" = dplyr::case_when(
        grepl("Field Blank", .data$PARAMETER, fixed = TRUE) ~ "Field Blank",
        grepl("Lab Blank", .data$PARAMETER, fixed = TRUE) ~ "Lab Blank",
        grepl("Replicate", .data$PARAMETER, fixed = TRUE) ~ "Replicate",
        TRUE ~ "Grab"
      )
    )
  return(df)
}

#' Results to Blackstone River Coalition
#'
#' @description Helper function for `format_results`. Formats result data for
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

#' Prep Friends of Casco Bay Results
#'
#' @description Helper function for `format_results`. Standardizes result data
#'    from the Friends of Casco Bay (ME_FOCB).
#'
#'   * Adds "Sample Depth Unit" column
#'   * Pivots table from wide to long.
#'
#' @param df Dataframe.
#' @param date_format String. Date format, uses lubridate. (word better)
#'
#' @returns Updated dataframe.
prep_ME_FOCB_results <- function(df, date_format = "m/d/y") {
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
    keep_col <- c(
      "SiteID", "Site ID", "Sample ID", "Date", "Time", "Sample Depth",
      "Sample Depth m", "Sample Depth Unit", "Project", "Sampled By"
    )

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
    df <- as.data.frame(df) %>%
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
  if ("Date" %in% colnames(df) && !"Sample Date" %in% colnames(df)) {
    df <- dplyr::rename(df, "Sample Date" = "Date")
  }

  if (all(c("Sample Date", "Analysis Date") %in% colnames(df))) {
    df <- df %>%
      col_to_date("Sample Date", date_format = date_format) %>%
      col_to_date("Analysis Date", date_format = date_format) %>%
      dplyr::mutate(
        "temp_gap" = as.numeric(
          .data[["Analysis Date"]] - .data[["Sample Date"]]
        )
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
        grepl("NITROGEN", .data$Parameter, fixed = TRUE) &
          .data$temp_gap > 28 ~ "J",
        TRUE ~ NA
      )
    ) %>%
    dplyr::select(!"temp_gap")

  return(df)
}
