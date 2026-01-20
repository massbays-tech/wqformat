#' Format result data
#'
#' @description Converts water quality result data between different formats.
#'
#' @param df Input dataframe.
#' @param in_format,out_format String. Desired input and output formats.
#' Not case sensitive. Accepted formats:
#'
#' * WQX
#'
#' * MassWateR
#'
#' * wqdashboard
#'
#' * RI_WW (Rhode Island Watershed Watch)
#'
#' * RI_DEM (Rhode Island DEM)
#'
#' * MA_BRC (Blackstone River Coalition)
#'
#' * ME_DEP (Maine DEP)
#'
#' * ME_FOCB (Friends of Casco Bay)
#' @param drop_extra_col Boolean. If `TRUE`, removes any columns that can't be
#'    converted to `out_format`. Default `TRUE`.
#'
#' @inheritParams col_to_date
#'
#' @returns Updated dataframe
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
  } else if (in_format %in% c("ri_dem", "ri_ww")) {
    df[["Depth Unit"]] <- "m"
  }

  # Update columns ----
  message("\tRenaming columns")
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
    message(
      "\tKept ", toString(length(drop_col)), " extra columns"
    )
  }

  # Update variables ----
  message("\tUpdating variable names")
  col_sub <- fetch_var(colnames_results, "wqx", out_format)

  # Format dates
  for (date_col in c("Activity Start Date", "Analysis Start Date")) {
    col_name <- rename_var(date_col, col_sub$old_names, col_sub$new_names)
    if (col_name %in% colnames(df)) {
      df <- col_to_date(df, col_name, date_format)
    }
  }

  # Rename parameters
  col_name <- rename_var(
    "Characteristic Name", col_sub$old_names, col_sub$new_names
  )
  if (col_name %in% colnames(df)) {
    df <- update_param(df, col_name, in_format, out_format)
  }

  # Rename units
  for (
    unit_col in c(
      "Result Unit", "Activity Depth/Height Unit",
      "Result Detection/Quantitation Limit Unit"
    )
  ) {
    col_name <- rename_var(unit_col, col_sub$old_names, col_sub$new_names)
    if (col_name %in% colnames(df)) {
      df <- update_unit(df, col_name, in_format, out_format)
    }
  }

  # Rename qualifiers
  col_name <- rename_var(
    "Result Measure Qualifier", col_sub$old_names, col_sub$new_names
  )
  if (col_name %in% colnames(df)) {
    qual <- fetch_var(varnames_qualifiers, in_format, out_format)
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
    atype <- fetch_var(varnames_activity, in_format, out_format)
    df <- update_var(df, col_name, atype$old_names, atype$new_names)
    warn_invalid_var(df, col_name, atype$keep_var)
  }

  # Custom format changes -----
  if (out_format == "masswater") {
    df <- results_to_mwr(df)
  } else if (out_format == "wqdashboard") {
    df <- results_to_wqd(df)
  } else if (out_format == "wqx") {
    df <- results_to_wqx(df)
  } else if (out_format == "ma_brc") {
    df <- results_to_brc(df)
  } else if (out_format == "me_dep") {
    df <- results_to_me_dep(df)
  } else if (out_format == "me_focb") {
    df <- results_to_focb(df)
  } else if (out_format %in% c("ri_dem", "ri_ww")) {
    df <- results_to_ridem(df)
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
#' @param .data Input dataframe
#'
#' @inheritParams format_results
#'
#' @returns Updated dataframe
#'
#' @export
format_mwr_results <- function(.data, date_format = "m/d/Y") {
  message("Formatting MassWateR result data...")

  # Check - repaired column names?
  chk <- grepl("\\.", colnames(.data))
  if (any(chk)) {
    colnames(.data) <- gsub("\\.", " ", colnames(.data))
    colnames(.data) <- gsub("Depth Height", "Depth/Height", colnames(.data))
  }

  # Check columns
  all_col <- c(
    "Monitoring Location ID", "Activity Type", "Activity Start Date",
    "Activity Start Time", "Activity Depth/Height Measure",
    "Activity Depth/Height Unit", "Activity Relative Depth Name",
    "Characteristic Name", "Result Value", "Result Unit", "Quantitation Limit",
    "QC Reference Value", "Result Measure Qualifier", "Result Attribute",
    "Sample Collection Method ID", "Project ID", "Local Record ID",
    "Result Comment"
  )
  bonus_col <- c(
    "QC Reference Value", "Result Attribute", "Local Record ID",
    "Result Comment"
  )
  key_col <- setdiff(all_col, bonus_col)

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
      "Missing suggested columns: ", paste(missing_col, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"Quantitation Limit Unit" %in% colnames(.data)) {
    .data[["Quantitation Limit Unit"]] <- .data[["Result Unit"]]
  }

  # Check variables
  activity_list <- unique_var(varnames_activity, "masswater")
  param_list <- unique_var(varnames_parameters, "masswater")
  unit_list <- unique_var(varnames_units, "masswater")
  qual_list <- unique_var(varnames_qualifiers, "masswater")

  warn_invalid_var(.data, "Activity Type", activity_list)
  warn_invalid_var(.data, "Characteristic Name", param_list)
  warn_invalid_var(.data, "Result Unit", unit_list)
  warn_invalid_var(.data, "Activity Depth/Height Unit", unit_list)
  warn_invalid_var(.data, "Result Measure Qualifier", qual_list)

  # Improve formatting, arrange columns
  drop_col <- setdiff(colnames(.data), all_col)

  if (length(drop_col) > 0) {
    message("\tRemoved ", toString(length(drop_col)), " invalid columns")
  }

  dat <- .data %>%
    col_to_date("Activity Start Date") %>%
    results_to_mwr() %>% # improve formatting
    dplyr::select(dplyr::all_of(all_col)) # reorder columns

  message("Done")

  dat
}

#' Check and improve formatting for wqdashboard result data
#'
#' @description `format_wqd_results()` reviews wqdashboard result data and
#' checks for common formatting errors.
#' * Checks for missing columns
#' * Checks for unknown parameters, units, qualifiers, and activity type
#' * Formats Date column as date
#'
#' @param .data Input dataframe
#' @param categorical Boolean. Set to `TRUE` for categorical data that may be
#' missing column "Result_Unit." Default `FALSE`.
#'
#' @inheritParams col_to_date
#'
#' @returns Updated dataframe
#'
#' @export
format_wqd_results <- function(.data, date_format, categorical = FALSE) {
  message("Formatting wqdashboard result data...")

  # Check columns
  key_col <- c("Site_ID", "Date", "Parameter", "Result", "Result_Unit")
  bonus_col <- c(
    "Activity_Type", "Depth", "Depth_Unit", "Depth_Category",
    "Detection_Limit_Type", "Lower_Detection_Limit", "Upper_Detection_Limit",
    "Detection_Limit_Unit", "Qualifier"
  )

  if (categorical) {
    key_col <- key_col[1:4]
    bonus_col <- c("Result_Unit", bonus_col)
  }

  chk <- key_col %in% colnames(.data)
  if (any(!chk)) {
    missing_col <- key_col[which(!chk)]
    stop(
      "Missing mandatory columns: ", paste(missing_col, collapse = ", ")
    )
  }

  missing_col <- setdiff(bonus_col, colnames(.data))
  .data[missing_col] <- NA

  # Improve formatting, standardize variables
  param <- fetch_var(varnames_parameters, "wqx", "wqx")
  par_unit <- fetch_var(varnames_units, "wqx", "wqx")
  old_unit <- par_unit$old_names
  new_unit <- par_unit$new_names

  dat <- .data %>%
    col_to_date("Date", date_format) %>%
    update_var("Parameter", param$old_names, param$new_names) %>%
    update_var("Result_Unit", old_unit, new_unit) %>%
    update_var("Depth_Unit", old_unit, new_unit) %>%
    update_var("Detection_Limit_Unit", old_unit, new_unit) %>%
    results_to_wqd()

  # Check variables
  activity_list <- unique_var(varnames_activity, "wqx")
  qual_list <- unique_var(varnames_qualifiers, "wqx")
  unit_list <- par_unit$keep_var

  warn_invalid_var(dat, "Activity_Type", activity_list)
  warn_invalid_var(dat, "Parameter", param$keep_var)
  warn_invalid_var(dat, "Result_Unit", unit_list)
  warn_invalid_var(dat, "Depth_Unit", unit_list)
  warn_invalid_var(dat, "Detection_Limit_Unit", unit_list)
  warn_invalid_var(dat, "Qualifier", qual_list)

  message("Done")

  dat
}
