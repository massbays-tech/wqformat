#' Format site data
#'
#' @description Converts water quality site data between different formats.
#'
#' @param df Input dataframe.
#' @param in_format,out_format String. Desired input and output formats. Not
#' case sensitive. Accepted formats:
#' * WQX
#' * MassWateR
#' * wqdashboard
#' * RI_WW (Rhode Island Watershed Watch)
#' * MA_BRC (Blackstone River Coalition)
#' * ME_FOCB (Friends of Casco Bay)
#'
#' @inheritParams format_results
#'
#' @returns Updated dataframe.
format_sites <- function(df, in_format, out_format, drop_extra_col = TRUE) {
  message("Reformatting data...")

  # Check inputs ----
  in_format <- tolower(in_format)
  out_format <- tolower(out_format)

  target_formats <- c(
    "wqx", "masswater", "wqdashboard", "ri_ww", "ma_brc", "me_focb"
  )
  chk <- c(in_format, out_format) %in% target_formats
  if (any(!chk)) {
    stop(
      "Invalid format. Acceptable options: ",
      paste(target_formats, collapse = ", ")
    )
  }

  # Check - repaired column names?
  chk <- grepl("\\.", colnames(df))
  chk2 <- grepl(" ", colnames(df))
  if (any(chk) && !any(chk2)) {
    name_repair <- TRUE
  } else {
    name_repair <- FALSE
  }

  # Prep data with nonstandard formats ----
  if (in_format == "ma_brc") {
    df <- prep_brc_sites(df)
  }

  # Update columns ----
  var_names <- fetch_var(colnames_sites, in_format, out_format, name_repair)
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
  if (out_format == "wqx") {
    df <- state_to_abb(df, "State Code")
  } else if (out_format == "wqdashboard") {
    df <- state_to_abb(df, "State")
  }

  # Format data with nonstandard formats ----
  if (out_format == "ma_brc") {
    df <- sites_to_brc(df)
  }

  message("Done")
  return(df)
}
