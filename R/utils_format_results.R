#' Preformat result data from MassWateR
#'
#' @description
#' `prep_MassWateR_results()` is a helper function for [format_results()] that
#' preformats result data from MassWateR.
#' * Transfers "BDL" and "AQL" values from "Result Value" to "Result Measure
#' Qualifier".
#' * Data in "QC Reference Value" is copied to its own row.
#'
#' @param .data Dataframe
#'
#' @returns Dataframe matching the standard format used by [format_results()]
#'
#' @noRd
prep_MassWateR_results <- function(.data) {
  # Prep data
  .data[["Result Value"]] <- as.character(.data[["Result Value"]])
  .data[["QC Reference Value"]] <- as.character(.data[["QC Reference Value"]])

  # Transfer QC Reference Value to own row
  qc_duplicate <- c(
    "Quality Control Sample-Lab Duplicate",
    "Quality Control-Meter Lab Duplicate"
  )

  dat <- .data %>%
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
  dat <- dat %>%
    dplyr::mutate(
      "Result Measure Qualifier" = dplyr::case_when(
        .data[["Result Value"]] == "BDL" ~ "DL",
        .data[["Result Value"]] == "AQL" ~ "GT",
        TRUE ~ .data[["Result Measure Qualifier"]]
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

  return(dat)
}

#' Results to MassWateR
#'
#' @description
#' `results_to_MassWateR()` is a helper function for [format_results()] that
#' formats result data for MassWateR.
#' * Updates "Result Value" and "Result Measure Qualifier" for over-detects and
#' under-detects
#' * Transfers duplicate values to "QC Reference Value"
#'
#' @param .data Dataframe
#'
#' @inheritParams format_results
#'
#' @returns Dataframe matching the standard format used by MassWateR
#'
#' @noRd
results_to_MassWateR <- function(.data) {
  # Prep data
  .data[["Result Value"]] <- as.character(.data[["Result Value"]])
  .data[["QC Reference Value"]] <- as.character(.data[["QC Reference Value"]])

  dat_colnames <- colnames(.data)

  # Update qualifiers, result value
  q_under <- rename_var(
    "Non-Detect",
    varnames_qualifiers$Flag,
    varnames_qualifiers$WQX,
    multiple = TRUE
  )
  q_over <- rename_var(
    "Over-Detect",
    varnames_qualifiers$Flag,
    varnames_qualifiers$WQX,
    multiple = TRUE
  )
  q_pass <- rename_var(
    "Pass",
    varnames_qualifiers$Flag,
    varnames_qualifiers$WQX,
    multiple = TRUE
  )

  dat <- .data %>%
    dplyr::mutate(
      "Result Value" = dplyr::case_when(
        .data[["Result Measure Qualifier"]] %in% q_under ~ "BDL",
        .data[["Result Measure Qualifier"]] %in% q_over ~ "AQL",
        TRUE ~ .data[["Result Value"]]
      )
    ) %>%
    dplyr::mutate(
      "Result Measure Qualifier" = dplyr::if_else(
        .data[["Result Measure Qualifier"]] %in% c(q_under, q_over, q_pass),
        NA,
        .data[["Result Measure Qualifier"]]
      )
    )

  # Transfer QC duplicates to QC Reference Value
  qc_duplicate <- c(
    "Quality Control Sample-Lab Duplicate",
    "Quality Control-Meter Lab Duplicate"
  )
  group_col <- setdiff(dat_colnames, "Result Value")

  chk <- dat[["Activity Type"]] %in% qc_duplicate &
    is.na(dat[["QC Reference Value"]])
  dat1 <- dat[which(chk), ] # data to group
  dat2 <- dat[which(!chk), ] # data to leave as-is

  dat1 <- dat1 %>%
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

  dat <- rbind(dat1, dat2)
  dat <- as.data.frame(dat) %>%
    dplyr::arrange(
      .data[["Activity Start Date"]],
      .data[["Activity Start Time"]]
    ) %>%
    dplyr::select(dplyr::all_of(dat_colnames)) %>%
    col_to_numeric("Result Value") %>%
    col_to_numeric("QC Reference Value")

  return(dat)
}

#' Preformat result data from the Blackstone River Coalition
#'
#' @description
#' `prep_MA_BRC_results()` is a helper function for [format_results()] that
#' preformats result data from the Blackstone River Coalition (MA_BRC).
#' * Adds columns "DATE", "TIME", "SAMPLE_TYPE"
#'
#' @param .data Input Dataframe
#' @param date_format String. Date format. Uses the same formatting as
#' [lubridate::parse_date_time()]. Default value is "Y-m-d H:M".
#' @param tz String. Timezone. Default value is "America/New York".
#'
#' @returns Dataframe matching the standard format used by [format_results()]
#'
#' @noRd
prep_MA_BRC_results <- function(.data, date_format = "Y-m-d H:M",
                                tz = "America/New_York") {
  dat <- .data %>%
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
    ) %>%
    dplyr::mutate("DEPTH_CATEGORY" = "Surface")
  return(dat)
}

#' Results to Blackstone River Coalition
#'
#' @description
#' `results_to_MA_BRC()` is a helper function for [format_results()] that
#' formats result data for the Blackstone River Coalition (MA_BRC).
#' * Uses "DATE", "TIME" columns to fill "DATE_TIME" column
#' * uses "SAMPLE_TYPE" column to update "PARAMETER" column
#' * Adds column "UNIQUE_ID"
#' * Removes "DATE", "TIME", and "SAMPLE_TYPE" columns
#'
#' @param .data Dataframe
#'
#' @returns
#' Dataframe matching the standard format used by the Blackstone River
#' Coalition.
#'
#' @noRd
results_to_MA_BRC <- function(.data) {
  dat <- .data %>%
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
    dplyr::select(
      !dplyr::any_of(
        c("DATE", "TIME", "SAMPLE_TYPE", "DEPTH_CATEGORY")
      )
    )

  return(dat)
}

#' Preformat result data from Friends of Casco Bay
#'
#' @description
#' `prep_ME_FOCB_results()` is a helper function for [format_results()] that
#' preformats result data from Friends of Casco Bay (ME_FOCB).
#' * Adds column "Sample Depth Unit"
#' * Pivots table from wide to long
#'
#' @param .data Dataframe
#'
#' @inheritParams col_to_date
#'
#' @returns Dataframe matching the standard format used by [format_results()]
#'
#' @noRd
prep_ME_FOCB_results <- function(.data, date_format = "m/d/y") {
  # Add columns
  if (any(c("Sample Depth", "Sample Depth m") %in% colnames(.data))) {
    .data <- dplyr::mutate(.data, "Sample Depth Unit" = "m")
  }

  dat <- .data %>%
    dplyr::mutate("Project" = "FRIENDS OF CASCO BAY ALL SITES") %>%
    dplyr::mutate("Sampled By" = "FRIENDS OF CASCO BAY")

  # Check if table is long, else make long
  if (!"Parameter" %in% colnames(dat)) {
    # Pivot table longer, update parameter & unit names
    keep_col <- c(
      "SiteID", "Site ID", "Sample ID", "Date", "Time", "Sample Depth",
      "Sample Depth m", "Sample Depth Unit", "Project", "Sampled By"
    )

    # Set table to numeric before pivot to avoid errors from "BSV" score
    dat <- dat %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::pivot_longer(
        !dplyr::any_of(keep_col),
        names_to = "Parameter",
        values_to = "Result",
        values_drop_na = TRUE
      )

    # Return columns to numeric...
    for (field in colnames(dat)) {
      dat <- col_to_numeric(dat, field)
    }

    # Add parameters, units
    dat <- as.data.frame(dat) %>%
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
  if ("Date" %in% colnames(dat) && !"Sample Date" %in% colnames(dat)) {
    dat <- dplyr::rename(dat, "Sample Date" = "Date")
  }

  if (all(c("Sample Date", "Analysis Date") %in% colnames(dat))) {
    dat <- dat %>%
      col_to_date("Sample Date", date_format = date_format) %>%
      col_to_date("Analysis Date", date_format = date_format) %>%
      dplyr::mutate(
        "temp_gap" = as.numeric(
          .data[["Analysis Date"]] - .data[["Sample Date"]]
        )
      )
  } else {
    dat$temp_gap <- 0
  }

  # Add qualifiers
  dat <- dat %>%
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

  return(dat)
}
