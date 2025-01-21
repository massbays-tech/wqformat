#' Prep MassWateR
#'
#' @description Converts MassWateR data to standard format. Helper function for
#'   `format_results`.
#'  * Transfers "BDL" and "AQL" values from `Result Value` to
#'    `Result Measure Qualifier`.
#'  * Data in `QC Reference Value` is copied to its own row.
#'
#' @param df Dataframe.
#'
#' @returns Updated dataframe.
prep_MassWateR <- function(df){
  # Prep data
  df$`Result Value` <- as.character(df$`Result Value`)
  df$`QC Reference Value` <- as.character(df$`QC Reference Value`)

  # Transfer QC Reference Value to own row
  qc_duplicate <- c("Quality Control Sample-Lab Duplicate",
                    "Quality Control-Meter Lab Duplicate")

  df <- df %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        `Activity Type` %in% qc_duplicate & !is.na(`QC Reference Value`),
        paste0("NA|", `QC Reference Value`),
        `QC Reference Value`
      )
    ) %>%
    tidyr::separate_longer_delim(`QC Reference Value`, "|") %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        `QC Reference Value` == "NA",
        NA,
        `QC Reference Value`
      )
    ) %>%
    dplyr::mutate(
      `Result Value` = dplyr::if_else(
        `Activity Type` %in% qc_duplicate & !is.na(`QC Reference Value`),
        `QC Reference Value`,
        `Result Value`
      )
    ) %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        `Activity Type` %in% qc_duplicate,
        NA,
        `QC Reference Value`
      )
    )

  # Update qualifiers, result value
  df <- df %>%
    dplyr::mutate(
      `Result Measure Qualifier` = dplyr::if_else(
        `Result Value` %in% c("BDL", "AQL"),
        `Result Value`,
        `Result Measure Qualifier`
      )
    ) %>%
    dplyr::mutate(
      `Result Value` = dplyr::if_else(
        `Result Value` %in% c("BDL", "AQL"),
        NA,
        `Result Value`
      )
    )

  # Reset columns to numeric
  df <- col_to_numeric(df, "Result Value")
  df <- col_to_numeric(df, "QC Reference Value")

  return(df)
}

#' To MassWateR
#'
#' @description Helper function for `format_results` that formats data for
#'  `MassWateR`.
#'  * Sets `Result Value` to "BDL" or "AQL" as needed
#'  * Sets `Result Measure Qualifier` to Q or NA
#'  * Transfers duplicate values to `QC Reference Value`
#'
#' @param df Dataframe.
#'
#' @returns Updated dataframe.
to_MassWateR <- function(df, in_format){
  # Prep data
  df$`Result Value` <- as.character(df$`Result Value`)
  df$`QC Reference Value` <- as.character(df$`QC Reference Value`)

  df_colnames <- colnames(df)

  # Update qualifiers, result value
  qual <- find_var_names(varnames_qualifiers, in_format, "Flag")
  df <- rename_all_var(df, "Result Measure Qualifier", qual$old_names, qual$new_names)

  df <- df %>%
    dplyr::mutate(
      `Result Value` = dplyr::case_when(
        `Result Measure Qualifier` == "Non-Detect" ~ "BDL",
        `Result Measure Qualifier` == "Over-Detect" ~ "AQL",
        TRUE ~ `Result Value`
      )
    ) %>%
    dplyr::mutate(
      `Result Measure Qualifier` = dplyr::if_else(
        `Result Measure Qualifier` %in% c("Not Reviewed", "Suspect"),
        "Q",
        NA
      )
    )

  # Transfer QC duplicates to QC Reference Value
  qc_duplicate <- c("Quality Control Sample-Lab Duplicate",
                    "Quality Control-Meter Lab Duplicate")
  group_col <- setdiff(df_colnames, "Result Value")

  chk <- df[["Activity Type"]] %in% qc_duplicate &
    is.na(df[["QC Reference Value"]])
  df1 <- df[which(chk),]  # data to group
  df2 <- df[which(!chk),]  # data to leave as-is

  df1 <- df1 %>%
    dplyr::group_by_at(group_col) %>%
    dplyr::summarize(
      `Result Value` = stringr::str_c(`Result Value`, collapse = "|"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        stringr::str_count(`Result Value`, "\\|") == 1,
        stringr::str_split_i(`Result Value`, "\\|", 2),
        `QC Reference Value`
      )
    ) %>%
    dplyr::mutate(
      `Result Value` = dplyr::if_else(
        stringr::str_count(`Result Value`, "\\|") == 1,
        stringr::str_split_i(`Result Value`, "\\|", 1),
        `Result Value`
      )
    ) %>%
    tidyr::separate_longer_delim(`Result Value`, "|")

  df <- rbind(df1, df2)
  df <- as.data.frame(df) %>%
    dplyr::arrange(`Activity Start Date`, `Activity Start Time`) %>%  # Sort data by Date, Time
    dplyr::select(dplyr::all_of(df_colnames))  # Reorder columns, else "Result Value" sent to end
  df <- col_to_numeric(df, "Result Value")
  df <- col_to_numeric(df, "QC Reference Value")

  return(df)
}
