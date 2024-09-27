#' Format site data for WQX
#'
#' @description boop
#'
#' @param df Input dataframe.
#' @param in_format boop
#' @param drop_extra_col boop boop
#'
#' @returns Updated dataframe.
#'
#' @noRd
sites_to_wqx <- function(df, in_format, drop_extra_col = TRUE,
                         warn_missing_col = TRUE){

  message("Reformatting data...")

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_sites,
    in_format = in_format,
    out_format = "WQX")
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
  if ("State Code" %in% colnames(df)) {
    chk <- df[["State Code"]] %in% state.name
    if (any(chk)) {
      df <- df %>%
        dplyr::mutate("State Code" = dplyr::if_else(
          "State Code" %in% state.name,
          state.abb[match("State Code", state.name)],
          "State Code"))
    }
  }

  message("Done")

  return(df)
}

#' Format result data for WQX
#'
#' @description boop
#'
#' @param df Input dataframe.
#' @param in_format boop
#' @param date_format boop
#' @param drop_extra_col boop boop
#'
#' @returns Updated dataframe.
#'
#' @noRd
results_to_wqx <- function(df, in_format, date_format="m/d/Y",
                           drop_extra_col = TRUE, warn_missing_col = TRUE){

  message("Reformatting data...")

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_results,
    in_format = in_format,
    out_format = "WQX")
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
  if (in_format == "WQdashboard") { in_format == "WQX" }

  if ("Characteristic Name" %in% colnames(df)) {
    param <- find_var_names(varnames_parameters, in_format, "WQX")
    df <- rename_all_var(df, "Characteristic Name", param$old_names, param$new_names)
  }

  unit_name <- find_var_names(varnames_units, in_format, "WQX")

  if ("Result Unit" %in% colnames(df)) {
    df <- rename_all_var(df, "Result Unit", unit_name$old_names,
                         unit_name$new_names)
  }

  if ("Activity Depth/Height Unit" %in% colnames(df)) {
    df <- rename_all_var(df, "Activity Depth/Height Unit", unit_name$old_names,
                         unit_name$new_names)
  }

  if ("Result Detection/Quantitation Limit Unit" %in% colnames(df)) {
    df <- rename_all_var(df, "Result Detection/Quantitation Limit Unit",
                         unit_name$old_names, unit_name$new_names)
  }

  if ("Result Measure Qualifier" %in% colnames(df)) {
    qual <- find_var_names(varnames_qualifiers, in_format, "WQX")
    df <- rename_all_var(df, "Result Measure Qualifier", qual$old_names, qual$new_names)
  }

  if ("Activity Start Date" %in% colnames(df)) {
    df <- format_date(df, "Activity Start Date", date_format)
  }

  if ("Analysis Start Date" %in% colnames(df)) {
    df <- format_date(df, "Analysis Start Date", date_format)
  }

  message("Done")

  return(df)
}
