#' Format result data
#'
#' @description Converts water quality result data between formats. (List
#'  formats)
#'
#' @param df Input dataframe.
#' @param in_format String. Name of input format. (word better)
#' @param out_format String. Name of desired output format. (word better)
#' @param date_format String. Date format, uses lubridate. (word better) Default
#'    value "m/d/Y".
#' @param tz Timezone. Default value "America/New_York".
#' @param drop_extra_col Boolean. If TRUE, removes any columns that can't be
#'    converted to `out_format`. Default value TRUE.
#'
#' @returns Updated dataframe.
format_results <- function(
    df, in_format, out_format, date_format = "m/d/Y", tz = "America/New_York",
    drop_extra_col = TRUE) {
  message("Reformatting data...")

  # Preformat data ----
  if (in_format == "MassWateR") {
    df <- prep_MassWateR_results(df)
  } else if (in_format == "MA_BRC") {
    df <- prep_MA_BRC_results(df, date_format, tz)
  } else if (in_format == "ME_DEP") {
    df <- concat_columns(
      df,
      in_fields = c(
        "LAB_QUALIFIER", "PARAMETER_QUALIFIER", "VALIDATION_QUALIFIER"
      ),
      out_field = "LAB_QUALIFIER"
    )
  } else if (in_format == "ME_FOCB") {
    df <- prep_ME_FOCB_results(df, date_format)
  }

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_results,
    in_format = in_format,
    out_format = out_format
  )
  df <- rename_col(
    df = df,
    old_colnames = var_names$old_names,
    new_colnames = var_names$new_names
  )

  # Add missing columns
  missing_col <- setdiff(var_names$keep_var, colnames(df))
  if (length(missing_col) > 0) {
    df[missing_col] <- NA
    message("\tAdded ", toString(length(missing_col)), " new columns")
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
  col_sub <- find_var_names(colnames_results, "WQX", out_format)

  # Format dates
  for (date_col in c("Activity Start Date", "Analysis Start Date")) {
    col_name <- rename_var(
      in_var = date_col,
      old_varname = col_sub$old_names,
      new_varname = col_sub$new_names
    )
    if (col_name %in% colnames(df)) {
      df <- col_to_date(df, col_name, date_format)
    }
  }

  # Check - in_format and out_format using same variables?
  if (in_format == "WQdashboard") {
    in_format <- "WQX"
  } else if (in_format == "RI_WW") {
    in_format <- "RI_DEM"
  }

  if (out_format == "WQdashboard") {
    out_format <- "WQX"
  } else if (out_format == "RI_WW") {
    out_format <- "RI_DEM"
  }

  if (in_format == out_format) {
    message("Done")
    return(df)
  }

  # Rename parameters
  col_name <- rename_var(
    in_var = "Characteristic Name",
    old_varname = col_sub$old_names,
    new_varname = col_sub$new_names
  )
  if (col_name %in% colnames(df)) {
    param <- find_var_names(varnames_parameters, in_format, out_format)
    df <- rename_all_var(df, col_name, param$old_names, param$new_names)
    warn_invalid_var(df, col_name, param$keep_var)
  }

  # Rename units
  unit_name <- find_var_names(varnames_units, in_format, out_format)
  for (unit_col in c(
    "Result Unit", "Activity Depth/Height Unit",
    "Result Detection/Quantitation Limit Unit"
  )) {
    col_name <- rename_var(
      in_var = unit_col,
      old_varname = col_sub$old_names,
      new_varname = col_sub$new_names
    )
    if (col_name %in% colnames(df)) {
      df <- rename_all_var(df, col_name, unit_name$old_names, unit_name$new_names)
      warn_invalid_var(df, col_name, unit_name$keep_var)
    }
  }

  # Rename qualifiers
  col_name <- rename_var(
    in_var = "Result Measure Qualifier",
    old_varname = col_sub$old_names,
    new_varname = col_sub$new_names
  )
  if (out_format != "MassWateR" && col_name %in% colnames(df)) {
    qual <- find_var_names(varnames_qualifiers, in_format, out_format)
    df <- rename_all_var(df, col_name, qual$old_names, qual$new_names)
    warn_invalid_var(df, col_name, qual$keep_var)
  }

  # Rename activity type
  col_name <- rename_var(
    in_var = "Activity Type",
    old_varname = col_sub$old_names,
    new_varname = col_sub$new_names
  )
  if (col_name %in% colnames(df)) {
    atype <- try(
      find_var_names(varnames_activity, in_format, out_format),
      silent = TRUE
    )
    if (!inherits(atype, "try-error")) {
      df <- rename_all_var(df, col_name, atype$old_names, atype$new_names)
      warn_invalid_var(df, col_name, atype$keep_var)
    }
  }

  # Custom format changes -----
  if (out_format == "MassWateR") {
    df <- results_to_MassWateR(df, in_format)
  } else if (out_format == "MA_BRC") {
    df <- results_to_MA_BRC(df)
  }

  message("Done")
  return(df)
}
