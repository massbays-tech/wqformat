#' Format result data
#'
#' @description boop
#'
#' @param df Input dataframe.
#' @param in_format String. Name of input format. (word better)
#' @param out_format String. Name of desired output format. (word better)
#' @param date_format String. Date format, uses lubridate. (word better)
#' @param drop_extra_col Boolean. If TRUE, removes any columns that can't be
#'    converted to `out_format`. Default value TRUE.
#'
#' @returns Updated dataframe.
#'
#' @noRd
format_results <- function(df, in_format, out_format,
    date_format="m/d/Y", drop_extra_col = TRUE){

  message("Reformatting data...")

  # Preformat data ----
  if (in_format == "MassWateR") {
    df <- prep_MassWateR(df)
  } else if (in_format == "ME_DEP") {
    df <- concat_columns(df,
      in_fields = c('LAB_QUALIFIER', 'PARAMETER_QUALIFIER',
                    'VALIDATION_QUALIFIER'),
      out_field = 'LAB_QUALIFIER')
  } else if (in_format == "ME_FOCB") {
    df <- prep_ME_FOCB(df, date_format)
  }

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_results,
    in_format = in_format,
    out_format = out_format)
  df <- rename_col(
    df = df,
    old_colnames = var_names$old_names,
    new_colnames = var_names$new_names)

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
    df <- dplyr::select(dplyr::all_of(keep_col))
  } else if (drop_extra_col) {
    df <- dplyr::select(dplyr::all_of(keep_col))
    message("\tDropped ", toString(length(drop_col)), " columns")
  } else {
    df <- dplyr::select(dplyr::all_of(c(keep_col, drop_col)))
    warning(
      "\tUnable to rename ", toString(length(drop_col)), " columns: ",
      paste(drop_col, collapse = ", "),
      call. = FALSE
    )
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
  if (out_format != "MassWater" & col_name %in% colnames(df)) {
    qual <- find_var_names(varnames_qualifiers, in_format, out_format)
    df <- rename_all_var(df, col_name, qual$old_names, qual$new_names)
  }

  # Rename activity type
  col_name <- rename_var(
    in_var = "Activity Type",
    old_varname = col_sub$old_names,
    new_varname = col_sub$new_names)
  if (col_name %in% colnames(df)) {
    atype <- find_var_names(varnames_activity, in_format, out_format)
    df <- rename_all_var(df, col_name, atype$old_names, atype$new_names)
  }

  # Custom format changes -----
  if (out_format == "MassWateR") {
    df <- to_MassWateR(df, in_format)
  }

  message("Done")

  return(df)
}
