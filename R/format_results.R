#' Format result data
#'
#' @description boop
#'
#' @param df Input dataframe.
#' @param in_format boop
#' @param out_format boop
#' @param date_format boop
#' @param drop_extra_col boop boop
#' @param warn_missing_col boop boop
#'
#' @returns Updated dataframe.
#'
#' @noRd
format_results <- function(df, in_format, out_format,
    date_format="m/d/Y", drop_extra_col = FALSE, warn_missing_col = TRUE){

  message("Reformatting data...")

  # Preformat data ----
  if (in_format == "ME_FOCB") {
    df <- prep_ME_FOCB(df)
  # } else if (in_format == "MassWater") {
  #
  }

  if (out_format == "ME_DEP") {
    multiple_out_var <- TRUE
  } else {
    multiple_out_var <- FALSE
  }

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_results,
    in_format = in_format,
    out_format = out_format,
    multiple_out_var = multiple_out_var)
  df <- rename_col(
    df = df,
    old_colnames = var_names$old_names,
    new_colnames = var_names$new_names)
  if (drop_extra_col) {
    drop_col <- setdiff(colnames(df), var_names$keep_var)
    if (length(drop_col) > 0) {
      message("\tDropped ", toString(length(drop_col)), " columns")
      df <- dplyr::select(df, !dplyr::any_of(drop_col))
    }
  }
  if (warn_missing_col) {
    missing_col <- setdiff(var_names$keep_var, colnames(df))
    if (length(missing_col) > 0) {
      warning("\tMissing columns: ", paste(missing_col, collapse = ", "),
              call. = FALSE)
    }
  }

  # Update variables ----
  col_sub <- find_var_names(colnames_results, "WQX", out_format)

  # Format dates
  for (date_col in c("Activity Start Date", "Analysis Start Date")) {
    col_name <- rename_var(
      in_var = date_col,
      old_varname = col_sub$old_names,
      new_varname = col_sub$new_names)
    if (col_name %in% colnames(df)) {
      df <- format_date(df, col_name, date_format)
    }
  }

  # Check - in_format and out_format using same variables?
  if (in_format == "WQdashboard") { in_format <- "WQX" }
  if (out_format == "WQdashboard") { out_format <- "WQX" }

  if (in_format == out_format) {
    message("Done")
    return(df)
  }

  # Rename parameters
  col_name <- rename_var(
    in_var = "Characteristic Name",
    old_varname = col_sub$old_names,
    new_varname = col_sub$new_names)
  if (col_name %in% colnames(df)) {
    param <- find_var_names(varnames_parameters, in_format, out_format)
    df <- rename_all_var(df, col_name, param$old_names, param$new_names)
  }

  # Rename units
  unit_name <- find_var_names(varnames_units, in_format, out_format)
  for (unit_col in c("Result Unit", "Activity Depth/Height Unit",
                     "Result Detection/Quantitation Limit Unit")) {
    col_name <- rename_var(
      in_var = unit_col,
      old_varname = col_sub$old_names,
      new_varname = col_sub$new_names)
    if (col_name %in% colnames(df)) {
      df <- rename_all_var(df, col_name, unit_name$old_names, unit_name$new_names)
    }
  }

  # Rename qualifiers
  col_name <- rename_var(
    in_var = "Result Measure Qualifier",
    old_varname = col_sub$old_names,
    new_varname = col_sub$new_names)
  if (col_name %in% colnames(df)) {
    qual <- find_var_names(varnames_qualifiers, in_format, out_format)
    df <- rename_all_var(df, col_name, qual$old_names, qual$new_names)
  }

  message("Done")

  return(df)
}
