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
  df[["Result Value"]] <- as.character(df[["Result Value"]])
  df[["QC Reference Value"]] <- as.character(df[["QC Reference Value"]])

  # Transfer QC Reference Value to own row
  qc_duplicate <- c("Quality Control Sample-Lab Duplicate",
                    "Quality Control-Meter Lab Duplicate")

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
#' @param in_format String. Input format.
#'
#' @returns Updated dataframe.
to_MassWateR <- function(df, in_format){
  # Prep data
  df[["Result Value"]] <- as.character(df[["Result Value"]])
  df[["QC Reference Value"]] <- as.character(df[["QC Reference Value"]])

  df_colnames <- colnames(df)

  # Update qualifiers, result value
  qual <- find_var_names(varnames_qualifiers, in_format, "Flag")
  df <- rename_all_var(df, "Result Measure Qualifier", qual$old_names, qual$new_names)

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
        .data[["Result Measure Qualifier"]] %in% c("Not Reviewed", "Suspect") ~ "Q",
        .data[["Result Measure Qualifier"]] %in% c("Over-Detect", "Non-Detect", "Pass") ~ NA,
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
  df1 <- df[which(chk),]  # data to group
  df2 <- df[which(!chk),]  # data to leave as-is

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
    dplyr::select(dplyr::all_of(df_colnames))  # Reorder columns, else "Result Value" sent to end
  df <- col_to_numeric(df, "Result Value")
  df <- col_to_numeric(df, "QC Reference Value")

  return(df)
}
