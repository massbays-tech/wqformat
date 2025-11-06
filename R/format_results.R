#' Format result data
#'
#' @description Converts water quality result data between different formats.
#'
#' @param df Input dataframe.
#' @param in_format,out_format String. Desired input and output formats.
#' Not case sensitive. Accepted formats:
#' * WQX
#' * MassWateR
#' * wqdashboard
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
#'
#' @export
format_results <- function(df, in_format, out_format, date_format = "m/d/Y",
                           tz = Sys.timezone(), drop_extra_col = TRUE) {
  message("Reformatting data...")

  # Check inputs ----
  in_format <- tolower(in_format)
  out_format <- tolower(out_format)

  target_formats <- c(
    "wqx", "masswater", "wqdashboard", "ri_ww", "ri_dem", "ma_brc", "me_dep",
    "me_focb"
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
    df <- unrepair_names(df, colnames_results[[in_format]])
  }

  # Preformat data ----
  if (in_format == "masswater") {
    df <- prep_mwr_results(df)
  } else if (in_format == "wqx") {
    df <- prep_wqx_results(df)
  } else if (in_format == "ma_brc") {
    df <- prep_brc_results(df, date_format, tz)
  } else if (in_format == "me_dep") {
    df <- prep_me_dep_results(df)
  } else if (in_format == "me_focb") {
    df <- prep_focb_results(df, date_format)
  }

  # Update columns ----
  var_names <- fetch_var(
    colnames_results,
    in_format,
    out_format,
    limit_var = TRUE
  )
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
  col_sub <- fetch_var(colnames_results, "wqx", out_format)

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
  in_var <- in_format
  if (in_format == "wqdashboard") {
    in_var <- "wqx"
  } else if (in_format == "ri_ww") {
    in_var <- "ri_dem"
  }

  out_var <- out_format
  if (out_format == "wqdashboard") {
    out_var <- "wqx"
  } else if (out_format == "ri_ww") {
    out_var <- "ri_dem"
  }

  if (in_var == out_var && out_format == "wqx") {
    df <- results_to_wqx(df)
    message("Done")
    return(df)
  } else if (in_var == out_var) {
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
    param <- fetch_var(varnames_parameters, in_var, out_var)
    df <- update_var(df, col_name, param$old_names, param$new_names)
    warn_invalid_var(df, col_name, param$keep_var)
  }

  # Rename units
  unit_name <- fetch_var(varnames_units, in_var, out_var)
  for (
    unit_col in c(
      "Result Unit", "Activity Depth/Height Unit",
      "Result Detection/Quantitation Limit Unit"
    )
  ) {
    col_name <- rename_var(
      in_var = unit_col,
      old_varname = col_sub$old_names,
      new_varname = col_sub$new_names
    )
    if (col_name %in% colnames(df)) {
      df <- df %>%
        update_var(col_name, unit_name$old_names, unit_name$new_names)
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
    in_qual <- in_var
    out_qual <- out_var

    if (in_format == "masswater") {
      in_qual <- "wqx"
    } else if (out_format == "masswater") {
      out_qual <- "wqx"
    }

    qual <- fetch_var(varnames_qualifiers, in_qual, out_qual)
    df <- update_var(df, col_name, qual$old_names, qual$new_names)
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
      fetch_var(varnames_activity, in_var, out_var),
      silent = TRUE
    )
    if (!inherits(atype, "try-error")) {
      df <- update_var(df, col_name, atype$old_names, atype$new_names)
      warn_invalid_var(df, col_name, atype$keep_var)
    }
  }

  # Custom format changes -----
  if (out_format == "masswater") {
    df <- results_to_mwr(df)
  } else if (out_format == "wqx") {
    df <- results_to_wqx(df)
  } else if (out_format == "ma_brc") {
    df <- results_to_brc(df)
  }

  message("Done")
  return(df)
}

#' Check and improve formatting for MassWateR result data
#'
#' @description `format_mwr_results()` reviews MassWateR result data and checks
#' for common formatting errors.
#' * Checks for missing columns and places columns in the correct order
#' * Updates "Result Value" and "Result Measure Qualifier" for over-detects and
#' under-detects
#' * Transfers duplicate values to "QC Reference Value"
#'
#' @param .data Input dataframe.
#'
#' @returns Updated dataframe.
#'
#' @export
format_mwr_results <- function(.data) {
  message("Formatting MassWateR result data...")

  # Check - repaired column names?
  chk <- grepl("\\.", colnames(.data))
  if (any(chk)) {
    colnames(.data) <- gsub("\\.", " ", colnames(.data))
    colnames(.data) <- gsub("Depth Height", "Depth/Height", colnames(.data))
  }

  # Check columns
  key_col <- c(
    "Monitoring Location ID", "Activity Type", "Activity Start Date",
    "Activity Start Time", "Activity Depth/Height Measure",
    "Activity Depth/Height Unit", "Activity Relative Depth Name",
    "Characteristic Name", "Result Value", "Result Unit", "Quantitation Limit",
    "QC Reference Value", "Result Measure Qualifier", "Result Attribute",
    "Sample Collection Method ID", "Project ID"
  )
  bonus_col <- c("Local Record ID", "Result Comment")

  chk <- key_col %in% colnames(.data)
  if (any(!chk)) {
    missing_col <- key_col[which(!chk)]
    stop(
      "Missing mandatory columns: ", paste(missing_col, collapse = ", ")
    )
  }

  chk <- bonus_col %in% colnames(.data)
  if (any(!chk)) {
    missing_col <- bonus_col[which(!chk)]
    .data[missing_col] <- NA
    warning(
      "Missing suggested columns: ", paste(missing_col, collapse = ", ")
    )
  }

  # Check variables
  activity_list <- unique_var(varnames_activity, "masswater")
  param_list <- unique_var(varnames_parameters, "masswater")
  unit_list <- unique_var(varnames_units, "masswater")
  qual_list <- unique_var(varnames_qualifiers, "wqx")

  warn_invalid_var(.data, "Activity Type", activity_list)
  warn_invalid_var(.data, "Characteristic Name", param_list)
  warn_invalid_var(.data, "Result Unit", unit_list)
  warn_invalid_var(.data, "Result Measure Qualifier", qual_list)

  # Improve formatting, arrange columns
  keep_col <- c(key_col, bonus_col)
  drop_col <- setdiff(colnames(.data), keep_col)

  if (length(drop_col) > 0) {
    message("\tRemoved ", toString(length(drop_col)), " invalid columns")
  }

  dat <- .data %>%
    results_to_mwr() %>% # improve formatting
    dplyr::select(dplyr::all_of(keep_col)) # reorder columns

  message("Done")

  dat
}
