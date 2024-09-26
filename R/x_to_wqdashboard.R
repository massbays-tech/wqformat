#' Format site data for WQdashboard
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
sites_to_wqdashboard <- function(df, in_format, drop_extra_col = FALSE,
                                 warn_missing_col = TRUE){

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_sites,
    in_format = in_format,
    out_format = "WQdashboard")
  df <- rename_col(
    df = df,
    old_colnames = var_names$old_names,
    new_colnames = var_names$new_names)
  if (drop_extra_col) {
    drop_col <- setdiff(colnames(df), var_names$keep_var)
    if (length(drop_col) > 0) {
      message("Dropped ", toString(length(drop_col)), " columns")
      df <- dplyr::select(df, !dplyr::any_of(drop_col))
    }
  }
  if (warn_missing_col) {
    mandatory_col <- c("Site_ID", "Site_Name", "Latitude", "Longitude")
    missing_col <- setdiff(mandatory_col, colnames(df))
    if (length(missing_col) > 0) {
      warning("Missing columns: ", paste(missing_col, collapse = ", "))
    }
  }

  # Update variables ----
  if ("State" %in% colnames(df)) {
    chk <- df$State %in% state.name
    if (any(chk)) {
      df <- df %>%
        dplyr::mutate(State = dplyr::case_when(
          State %in% state.name ~ state.abb[match(State, state.name)],
          TRUE ~ State))
    }
  }

  return(df)
}

#' Format result data for WQdashboard
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
results_to_wqdashboard <- function(df, in_format, date_format="m/d/Y",
    drop_extra_col = FALSE, warn_missing_col = TRUE){

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_results,
    in_format = in_format,
    out_format = "WQdashboard")
  df <- rename_col(
    df = df,
    old_colnames = var_names$old_names,
    new_colnames = var_names$new_names)
  if (drop_extra_col) {
    drop_col <- setdiff(colnames(df), var_names$keep_var)
    if (length(drop_col) > 0) {
      message("Dropped ", toString(length(drop_col)), " columns")
      df <- dplyr::select(df, !dplyr::any_of(drop_col))
    }
  }
  if (warn_missing_col) {
    mandatory_col <- c("Site_ID", "Date", "Parameter", "Result", "Result_Unit")
    missing_col <- setdiff(mandatory_col, colnames(df))
    if (length(missing_col) > 0) {
      warning("Missing columns: ", paste(missing_col, collapse = ", "))
    }
  }

  # Update variables ----
  if ("Parameter" %in% colnames(df)) {
    param <- find_var_names(varnames_parameters, in_format, "WQX")
    df <- rename_all_var(df, "Parameter", param$old_names, param$new_names)
  }

  if ("Result_Unit" %in% colnames(df)) {
    unit_name <- find_var_names(varnames_units, in_format, "WQX")
    df <- rename_all_var(df, "Result_Unit", unit_name$old_names,
                         unit_name$new_names)
  }

  if ("Qualifier" %in% colnames(df)) {
    qual <- find_var_names(varnames_qualifiers, in_format, "WQX")
    df <- rename_all_var(df, "Qualifier", qual$old_names, qual$new_names)
  }

  if ("Date" %in% colnames(df)) {
    df <- format_date(df, "Date", date_format)
  }

  return(df)
}
