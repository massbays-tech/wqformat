#' Prepare result data from MassWateR
#'
#' @description
#' `prep_mwr_results()` is a helper function for [format_results()] that
#' standardizes MassWateR result data.
#' * Transfers "BDL" and "AQL" values from "Result Value" to "Result Measure
#' Qualifier".
#' * Data in "QC Reference Value" is copied to its own row.
#'
#' @param .data Dataframe
#' @param name_repair Set to TRUE if column names have undergone name repair
#' (eg all spaces replaces with periods). Default FALSE.
#'
#' @returns Dataframe matching the standard format used by [format_results()]
#'
#' @noRd
prep_mwr_results <- function(.data, name_repair = FALSE) {
  # Prep data
  if (!name_repair) {
    colnames(.data) <- make.names(colnames(.data))
  }
  .data$Result.Value <- as.character(.data$Result.Value)
  .data$QC.Reference.Value <- as.character(.data$QC.Reference.Value)

  # Set variables
  qc_ref <- c(
    "Quality Control Sample-Field Replicate",
    "Quality Control Sample-Lab Duplicate 2",
    "Quality Control Sample-Lab Spike Target",
    "Quality Control Field Replicate Msr/Obs",
    "Quality Control-Meter Lab Duplicate 2",
    "Quality Control-Calibration Check Buffer"
  )
  names(qc_ref) <- c(
    "Sample-Routine",
    "Quality Control Sample-Lab Duplicate",
    "Quality Control Sample-Lab Spike",
    "Field Msr/Obs",
    "Quality Control-Meter Lab Duplicate",
    "Quality Control-Calibration Check"
  )

  dat <- .data %>%
    # Separate QC Reference Value to own row
    dplyr::mutate(
      "QC.Reference.Value" = dplyr::if_else(
        is.na(.data$QC.Reference.Value),
        NA,
        paste0("NA|", .data$QC.Reference.Value)
      )
    ) %>%
    tidyr::separate_longer_delim(dplyr::all_of("QC.Reference.Value"), "|") %>%
    # Update new rows
    dplyr::mutate(
      "QC.Reference.Value" = dplyr::if_else(
        .data$QC.Reference.Value == "NA",
        NA,
        .data$QC.Reference.Value
      )
    ) %>%
    dplyr::mutate(
      "Result.Value" = dplyr::if_else(
        is.na(.data$QC.Reference.Value),
        .data$Result.Value,
        .data$QC.Reference.Value
      )
    ) %>%
    dplyr::mutate(
      "Activity.Type" = dplyr::if_else(
        .data$Activity.Type %in% names(qc_ref) &
          !is.na(.data$QC.Reference.Value),
        unname(qc_ref[.data$Activity.Type]),
        .data$Activity.Type
      )
    ) %>%
    # Drop QC Reference Value columns
    dplyr::select(!"QC.Reference.Value")

  # Update qualifiers, result value
  dat <- dat %>%
    dplyr::mutate(
      "Result.Measure.Qualifier" = dplyr::case_when(
        .data$Result.Value == "BDL" ~ "DL",
        .data$Result.Value == "AQL" ~ "GT",
        TRUE ~ .data$Result.Measure.Qualifier
      )
    ) %>%
    dplyr::mutate(
      "Result.Value" = dplyr::if_else(
        .data$Result.Value %in% c("BDL", "AQL"),
        NA,
        .data$Result.Value
      )
    ) %>%
    col_to_numeric("Result.Value")

  if (!name_repair) {
    colnames(dat) <- gsub("\\.", " ", colnames(dat))
    colnames(dat) <- gsub("Depth Height", "Depth/Height", colnames(dat))
  }

  dat
}

#' Add QC Reference Value to MassWateR result data
#'
#' @description
#' `add_qc_ref()` is a helper function for [results_to_mwr()] that
#' transfers duplicate values to "QC Reference Value"
#'
#' @param .data Dataframe
#'
#' @returns Updated dataframe where rows with duplicate samples have been
#' removed, and their values transferred to the "QC Reference Value" column in
#' the matching row.
#'
#' @noRd
add_qc_ref <- function(.data) {
  # Set variables
  dat <- .data
  qc_ref <- c(
    "Sample-Routine",
    "Quality Control Sample-Lab Duplicate",
    "Quality Control Sample-Lab Spike",
    "Field Msr/Obs",
    "Quality Control-Meter Lab Duplicate",
    "Quality Control-Calibration Check"
  )
  names(qc_ref) <- c(
    "Quality Control Sample-Field Replicate",
    "Quality Control Sample-Lab Duplicate 2",
    "Quality Control Sample-Lab Spike Target",
    "Quality Control Field Replicate Msr/Obs",
    "Quality Control-Meter Lab Duplicate 2",
    "Quality Control-Calibration Check Buffer"
  )

  # Check - any rows with duplicate/replicate data?
  chk <- dat[["Activity Type"]] %in% c(qc_ref, names(qc_ref)) &
    is.na(dat[["QC Reference Value"]])

  if (all(!chk)) {
    return(.data) # no duplicates, return original data
  }

  # Split data in two categories: possible duplicates, leave as-is
  dat1 <- dat[which(chk), ] # possible duplicate/replicate
  dat2 <- dat[which(!chk), ] # leave as-is

  # Prep duplicate data
  # - Add activity type category
  # - Sort rows with replicate samples AFTER initial samples
  dat1 <- dat1 %>%
    dplyr::mutate(
      "temp_activity" = dplyr::if_else(
        .data[["Activity Type"]] %in% names(qc_ref),
        qc_ref[.data[["Activity Type"]]],
        .data[["Activity Type"]]
      )
    ) %>%
    dplyr::mutate(
      "temp_rank" = dplyr::if_else(
        .data[["Activity Type"]] %in% names(qc_ref), 2, 1
      )
    ) %>%
    dplyr::arrange(.data$temp_rank) %>%
    dplyr::select(!"temp_rank")

  # Group data, check for paired data
  group_col <- setdiff(
    colnames(dat1),
    c(
      "Activity Type", "Result Value", "Result Measure Qualifier",
      "Local Record ID", "Result Comment"
    )
  )

  dat_temp <- dat1 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) %>%
    dplyr::add_count()

  chk <- dat_temp$n == 2
  chk2 <- dat_temp$n > 2

  if (any(chk2)) {
    warning(
      "More than two matching samples found. Check rows ",
      paste(which(chk2), collapse=", ")
    )
  }

  if (all(!chk)) {
    return(.data) # no pairs found, return original data
  } else if (any(!chk)) {
    # send non-pairs to dat2
    dat_temp <- dat1[which(!chk), ] %>%
      dplyr::select(!"temp_activity")
    dat2 <- rbind(dat2, dat_temp)

    # set dat1 to true pairs
    dat1 <- dat1[which(chk), ]
  }

  # Combine duplicate rows
  dat1 <- dat1 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) %>%
    dplyr::summarize(
      "Activity Type" = dplyr::first(.data[["Activity Type"]]),
      "Result Value" = stringr::str_flatten(.data[["Result Value"]], "|"),
      "Result Measure Qualifier" = stringr::str_flatten(
        .data[["Result Measure Qualifier"]],
        collapse = ",",
        na.rm = TRUE
      ),
      "Local Record ID" = stringr::str_flatten(
        .data[["Local Record ID"]],
        collapse = "; ",
        na.rm = TRUE
      ),
      "Result Comment" = stringr::str_flatten(
        .data[["Result Comment"]],
        collapse = "; ",
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    # Edit concatenated columns to remove duplicate strings
    dplyr::mutate(
      "Result Measure Qualifier" = dplyr::if_else(
        .data[["Result Measure Qualifier"]] %in% c(NA, ""),
        NA,
        sapply(
          .data[["Result Measure Qualifier"]],
          function(x) str_unique(x),
          USE.NAMES = FALSE
        )
      )
    ) %>%
    dplyr::mutate(
      "Local Record ID" = dplyr::if_else(
        .data[["Local Record ID"]] %in% c(NA, ""),
        NA,
        sapply(
          .data[["Local Record ID"]],
          function(x) str_unique(x, "; "),
          USE.NAMES = FALSE
        )
      )
    ) %>%
    dplyr::mutate(
      "Result Comment" = dplyr::if_else(
        .data[["Result Comment"]] %in% c(NA, ""),
        NA,
        sapply(
          .data[["Result Comment"]],
          function(x) str_unique(x, "; "),
          USE.NAMES = FALSE
        )
      )
    ) %>%
    # Set QC Reference Value, Result Value
    dplyr::mutate(
      "QC Reference Value" = dplyr::if_else(
        grepl("\\|", .data[["Result Value"]]),
        stringr::str_split_i(.data[["Result Value"]], "\\|", 2),
        .data[["QC Reference Value"]]
      )
    ) %>%
    dplyr::mutate(
      "Result Value" = dplyr::if_else(
        grepl("\\|", .data[["Result Value"]]),
        stringr::str_split_i(.data[["Result Value"]], "\\|", 1),
        .data[["Result Value"]]
      )
    ) %>%
    # Drop extra column
    dplyr::select(!"temp_activity")

  # Join data, fix formatting
  dat <- rbind(dat1, dat2)

  # Adjust formatting
  dat <- as.data.frame(dat) %>%
    dplyr::arrange(
      .data[["Activity Start Date"]],
      .data[["Activity Start Time"]]
    )
}

#' Results to MassWateR
#'
#' @description
#' `results_to_mwr()` is a helper function for [format_results()] that
#' formats result data for MassWateR.
#' * Updates "Result Value" and "Result Measure Qualifier" for over-detects and
#' under-detects
#' * Transfers duplicate values to "QC Reference Value"
#'
#' @param .data Dataframe
#'
#' @returns Dataframe matching the standard format used by MassWateR
#'
#' @noRd
results_to_mwr <- function(.data) {
  # Prep data
  .data[["Result Value"]] <- as.character(.data[["Result Value"]])
  .data[["QC Reference Value"]] <- as.character(.data[["QC Reference Value"]])

  column_order <- colnames(.data)

  # Update qualifiers, result value
  q_under <- c(
    "<2B", "2-5B", "BQL", "BRL", "D>T", "DL", "IDL", "K", "LTGTE", "U"
  )

  q_over <- c("GT", "E", "EE")

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
        .data[["Result Measure Qualifier"]] %in% c(q_under, q_over),
        NA,
        .data[["Result Measure Qualifier"]]
      )
    )

  # Transfer duplicate samples to QC Reference Value
  dat <- add_qc_ref(dat)

  # Adjust formatting
  dat <- dat %>%
    dplyr::select(dplyr::all_of(column_order)) %>%
    col_to_numeric("Result Value") %>%
    col_to_numeric("QC Reference Value")
}

#' Prepare result data from WQX
#'
#' @description
#' `prep_wqx_results()` is a helper function for [format_results()] that
#' standardizes WQX result data.
#' * Uses "Result Detection Condition" to update "Result Measure Qualifier"
#'
#' @param .data Dataframe
#' @param name_repair Set to TRUE if column names have undergone name repair
#' (eg all spaces replaces with periods). Default FALSE.
#'
#' @returns Dataframe matching the standard format used by [format_results()]
#'
#' @noRd
prep_wqx_results <- function(.data, name_repair = FALSE) {
  # Prep data
  if (!name_repair) {
    .data <- .data %>%
      dplyr::rename(
        "Result.Detection.Condition" = "Result Detection Condition",
        "Result.Measure.Qualifier" = "Result Measure Qualifier",
        "Result.Value" = "Result Value"
      )
  }

  # Update qualifiers
  dat <- .data %>%
    dplyr::mutate(
      "Result.Measure.Qualifier" = dplyr::case_when(
        !is.na(.data$Result.Measure.Qualifier) ~ .data$Result.Measure.Qualifier,
        .data$Result.Detection.Condition %in%
          c("Detected Not Quantified", "Present Below Quantification Limit") ~
          "BQL",
        .data$Result.Detection.Condition == "Not Reported" ~ "NRR",
        .data$Result.Detection.Condition ==
          "Present Above Quantification Limit" ~ "GT",
        .data$Result.Detection.Condition == "Not Detected" ~ "DL",
        .data$Result.Detection.Condition == "Trace" ~ "TR",
        TRUE ~ NA
      )
    )

  if (!name_repair) {
    dat <- dat %>%
      dplyr::rename(
        "Result Detection Condition" = "Result.Detection.Condition",
        "Result Measure Qualifier" = "Result.Measure.Qualifier",
        "Result Value" = "Result.Value"
      )
  }

  dat
}

#' Results to WQX
#'
#' @description
#' `results_to_wqx()` is a helper function for [format_results()] that
#' formats result data for WQX
#' * Uses "Result Measure Qualifier" to update "Result Detection Condition"
#'
#' @param .data Dataframe
#'
#' @inheritParams format_results
#'
#' @returns Dataframe matching the standard format used by WQX
#'
#' @noRd
results_to_wqx <- function(.data) {
  # Update Result Detection Condition
  .data %>%
    dplyr::mutate(
      "Result Detection Condition" = dplyr::case_when(
        !is.na(.data[["Result Detection Condition"]]) ~
          .data[["Result Detection Condition"]],
        .data[["Result Measure Qualifier"]] %in% c("2-5B", "D>T") ~
          "Detected Not Quantified",
        .data[["Result Measure Qualifier"]] == "NRR" ~ "Not Reported",
        .data[["Result Measure Qualifier"]] == "GT" ~
          "Present Above Quantification Limit",
        .data[["Result Measure Qualifier"]] == "DL" ~ "Not Detected",
        .data[["Result Measure Qualifier"]] == "BQL" ~
          "Present Below Quantification Limit",
        .data[["Result Measure Qualifier"]] == "TR" ~ "Trace",
        TRUE ~ NA
      )
    )
}

#' Preformat result data from the Blackstone River Coalition
#'
#' @description
#' `prep_brc_results()` is a helper function for [format_results()] that
#' standardizes result data from the Blackstone River Coalition (MA_BRC).
#' * Adds columns "DATE", "TIME", "ACTIVITY_TYPE", "DEPTH_CATEGORY", "
#' PROJECT_ID"
#'
#' @param .data Input Dataframe
#' @param date_format String. Date format. Uses the same formatting as
#' [lubridate::parse_date_time()]. Default value is "Y-m-d H:M".
#' @param tz String. Timezone. Default value is "America/New York".
#'
#' @returns Dataframe matching the standard format used by [format_results()]
#'
#' @noRd
prep_brc_results <- function(.data, date_format = "Y-m-d H:M",
                             tz = "America/New_York") {
  dat <- .data %>%
    col_to_date(
      "DATE_TIME",
      date_format = date_format,
      tz = tz,
      datetime = TRUE
    ) %>%
    dplyr::mutate("DATE" = as.Date(.data$DATE_TIME, tz = tz)) %>%
    dplyr::mutate("TIME" = format(.data$DATE_TIME, "%H:%M")) %>%
    dplyr::mutate(
      "ACTIVITY_TYPE" = dplyr::case_when(
        grepl("Field Blank", .data$PARAMETER, fixed = TRUE) ~ "Field Blank",
        grepl("Lab Blank", .data$PARAMETER, fixed = TRUE) ~ "Lab Blank",
        grepl("Replicate", .data$PARAMETER, fixed = TRUE) ~ "Replicate",
        TRUE ~ "Grab"
      )
    ) %>%
    dplyr::mutate(
      "PARAMETER" = dplyr::case_when(
        .data$ACTIVITY_TYPE == "Field Blank" ~
          gsub(" Field Blank", "", .data$PARAMETER),
        .data$ACTIVITY_TYPE == "Lab Blank" ~
          gsub(" Lab Blank", "", .data$PARAMETER),
        .data$ACTIVITY_TYPE == "Replicate" ~
          gsub(" Replicate", "", .data$PARAMETER),
        TRUE ~ .data$PARAMETER
      )
    ) %>%
    dplyr::mutate("DEPTH_CATEGORY" = "Surface") %>%
    dplyr::mutate("PROJECT_ID" = "BRC")
  return(dat)
}

#' Results to Blackstone River Coalition
#'
#' @description
#' `results_to_brc()` is a helper function for [format_results()] that
#' formats result data for the Blackstone River Coalition (MA_BRC).
#' * Uses "DATE", "TIME" columns to fill "DATE_TIME" column
#' * uses "ACTIVITY_TYPE" column to update "PARAMETER" column
#' * Adds column "UNIQUE_ID"
#' * Removes columns "DATE", "TIME", "ACTIVITY_TYPE", "DEPTH_CATEGORY",
#' "PROJECT_ID"
#'
#' @param .data Dataframe
#'
#' @returns
#' Dataframe matching the standard format used by the Blackstone River
#' Coalition.
#'
#' @noRd
results_to_brc <- function(.data) {
  .data %>%
    dplyr::mutate("DATE_TIME" = paste(.data$DATE, .data$TIME)) %>%
    dplyr::mutate(
      "PARAMETER" = dplyr::if_else(
        .data$ACTIVITY_TYPE %in% c("Field Blank", "Lab Blank", "Replicate"),
        paste(.data$PARAMETER, .data$ACTIVITY_TYPE),
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
        c("DATE", "TIME", "ACTIVITY_TYPE", "DEPTH_CATEGORY", "PROJECT_ID")
      )
    )
}

#' Preformat result data from Friends of Casco Bay
#'
#' @description
#' `prep_focb_results()` is a helper function for [format_results()] that
#' standardizes result data from Friends of Casco Bay (ME_FOCB).
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
prep_focb_results <- function(.data, date_format = "m/d/y") {
  # Add columns
  dat <- .data %>%
    dplyr::mutate("Project" = "FRIENDS OF CASCO BAY ALL SITES") %>%
    dplyr::mutate("Sampled By" = "FRIENDS OF CASCO BAY")

  colnames(dat) <- gsub("\\.", " ", colnames(dat))

  chk <- grepl("Sample Depth", colnames(dat))
  if (any(chk) && !"Sample Depth Unit" %in% colnames(dat)) {
    dat <- dplyr::mutate(dat, "Sample Depth Unit" = "m")
  }

  if ("QC Type" %in% colnames(dat)) {
    dat <- dat %>%
      dplyr::mutate(
        "QC Type" = dplyr::if_else(
          is.na(.data[["QC Type"]]),
          "Field Measurement",
          .data[["QC Type"]]
        )
      )
  } else {
    dat <- dplyr::mutate(dat, "QC Type" = "Field Measurement")
  }

  # Check if table is long, else make long
  if (!"Parameter" %in% colnames(dat)) {
    # Pivot table longer, update parameter & unit names
    keep_col <- c(
      "SiteID", "Site ID", "Sample ID", "Date", "Time", "Sample Depth_m",
      "Sample Depth m", "Sample Depth Unit", "Project", "Sampled By",
      "Sample Type", "QC Type"
    )

    # Set table to character before pivot to avoid errors from "BSV" score
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
        "Unit" = dplyr::if_else(
          tolower(.data$Parameter) == "ph",
          "STU",
          stringr::str_split_i(.data$Parameter, "_", 2)
        )
      ) %>%
      dplyr::mutate("Parameter" = stringr::str_split_i(.data$Parameter, "_", 1))
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
      "Parameter" = dplyr::if_else(
        grepl("total nitrogen mixed forms", tolower(.data$Parameter)),
        "TN as N",
        .data$Parameter
      )
    ) %>%
    dplyr::mutate(
      "Qualifier" = dplyr::case_when(
        tolower(.data$Parameter) == "chlorophyll" ~ "J",
        tolower(.data$Result) == "bsv" ~ "G",
        tolower(.data$Parameter) == "tn as n" & .data$temp_gap > 28 ~ "J",
        TRUE ~ NA
      )
    ) %>%
    dplyr::select(!"temp_gap")
}
