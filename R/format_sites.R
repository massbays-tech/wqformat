#' Format site data
#'
#' @description Converts water quality site data between formats. (List formats)
#'
#' @param df Input dataframe.
#' @param in_format,out_format String. Name of input & output formats
#'
#' @inheritParams format_results
#'
#' @returns Updated dataframe.
format_sites <- function(df, in_format, out_format, drop_extra_col = TRUE) {
  message("Reformatting data...")

  # Prep data with nonstandard formats ----
  if (in_format == "MA_BRC") {
    df <- prep_MA_BRC_sites(df)
  }

  # Update columns ----
  var_names <- fetch_var(colnames_sites, in_format, out_format)
  df <- rename_col(df, var_names$old_names, var_names$new_names)

  # Add missing columns
  missing_col <- setdiff(var_names$keep_var, colnames(df))
  if (length(missing_col) > 0) {
    df[missing_col] <- NA
    message(
      "\tAdded ", toString(length(missing_col)), " new columns: ",
      paste(missing_col, collapse = ", ")
    )
  }

  # Sort columns, drop surplus if drop_extra_col is TRUE
  keep_col <- var_names$keep_var
  drop_col <- setdiff(colnames(df), keep_col)
  if (length(drop_col) == 0) {
    df <- dplyr::select(df, dplyr::all_of(keep_col))
  } else if (drop_extra_col) {
    df <- dplyr::select(df, dplyr::all_of(keep_col))
    message("\tDropped ", toString(length(drop_col)), " columns")
  } else {
    df <- dplyr::select(df, dplyr::all_of(c(keep_col, drop_col)))
    warning(
      "\tUnable to rename ", toString(length(drop_col)), " columns: ",
      paste(drop_col, collapse = ", "),
      call. = FALSE
    )
  }

  # Update variables ----
  if (out_format == "WQX" && "State Code" %in% colnames(df)) {
    df <- col_to_state(df, "State Code")
  } else if (out_format == "WQdashboard" && "State" %in% colnames(df)) {
    df <- col_to_state(df, "State")
  }

  # Format data with nonstandard formats ----
  if (out_format == "MA_BRC") {
    df <- sites_to_MA_BRC(df)
  }

  message("Done")
  return(df)
}
