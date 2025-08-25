#' Format result data
#'
#' @description Converts water quality result data between different formats.
#'
#' @param df Input dataframe.
#' @param in_format,out_format String. Desired input and output formats.
#' Possible inputs:
#' * WQX
#' * MassWateR
#' * WQdashboard
#' * RI_WW (Rhode Island Watershed Watch)
#' * RI_DEM (Rhode Island DEM)
#' * MA_BRC (Blackstone River Coalition)
#' * ME_DEP (Maine DEP)
#' * ME_FOCB (Friends of Casco Bay)
#' @param drop_extra_col Boolean. If `TRUE`, removes any columns that can't be
#'    converted to `out_format`. Default `TRUE`.
#'
#' @inheritParams col_to_date
#'
#' @returns Updated dataframe.
format_results <- function(df, in_format, out_format, date_format = "m/d/Y",
                           tz = Sys.timezone(), drop_extra_col = TRUE) {
  message("Reformatting data...")

  # Check inputs ----
  target_formats <- c(
    "WQX", "MassWateR", "WQdashboard", "RI_WW", "RI_DEM", "MA_BRC", "ME_DEP",
    "ME_FOCB"
  )
  chk <- c(in_format, out_format) %in% target_formats
  if (any(!chk)) {
    stop(
      "Invalid format. Acceptable options: ",
      paste(target_formats, collapse = ", ")
    )
  }

  # Preformat data ----
  if (in_format == "MassWateR") {
    df <- prep_mwr_results(df)
  } else if (in_format == "MA_BRC") {
    df <- prep_brc_results(df, date_format, tz)
  } else if (in_format == "ME_DEP") {
    df <- df %>%
      concat_columns(
        c("LAB_QUALIFIER", "PARAMETER_QUALIFIER", "VALIDATION_QUALIFIER"),
        "LAB_QUALIFIER"
      ) %>%
      concat_columns(
        c("LAB_COMMENT", "SAMPLE_COMMENTS", "VALIDATION_COMMENT"),
        "SAMPLE_COMMENTS",
        concat = TRUE
      )
  } else if (in_format == "ME_FOCB") {
    df <- prep_focb_results(df, date_format)
  }

  # Update columns ----
  var_names <- fetch_var(colnames_results, in_format, out_format)
  df <- rename_col(df, var_names$old_names, var_names$new_names)

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
  col_sub <- fetch_var(colnames_results, "WQX", out_format)

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
    param <- fetch_var(varnames_parameters, in_format, out_format)
    df <- rename_all_var(df, col_name, param$old_names, param$new_names)
    warn_invalid_var(df, col_name, param$keep_var)
  }

  # Rename units
  unit_name <- fetch_var(varnames_units, in_format, out_format)
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
      df <- df %>%
        rename_all_var(col_name, unit_name$old_names, unit_name$new_names)
      warn_invalid_var(df, col_name, unit_name$keep_var)
    }
  }

  # Rename qualifiers
  col_name <- rename_var(
    in_var = "Result Measure Qualifier",
    old_varname = col_sub$old_names,
    new_varname = col_sub$new_names
  )
  if (col_name %in% colnames(df)) {
    in_qual <- in_format
    out_qual <- out_format

    if (in_format == "MassWateR") {
      in_qual <- "WQX"
    } else if (out_format == "MassWateR") {
      out_qual <- "WQX"
    }

    qual <- fetch_var(varnames_qualifiers, in_qual, out_qual)
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
      fetch_var(varnames_activity, in_format, out_format),
      silent = TRUE
    )
    if (!inherits(atype, "try-error")) {
      df <- rename_all_var(df, col_name, atype$old_names, atype$new_names)
      warn_invalid_var(df, col_name, atype$keep_var)
    }
  }

  # Custom format changes -----
  if (out_format == "MassWateR") {
    df <- results_to_mwr(df)
  } else if (out_format == "MA_BRC") {
    df <- results_to_brc(df)
  }

  message("Done")
  return(df)
}
