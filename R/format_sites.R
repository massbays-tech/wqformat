#' Reformat Site Data
#'
#' @description boop
#'
#' @param df Input dataframe.
#' @param in_format boop
#' @param out_format boop
#' @param drop_extra_col boop boop
#' @param warn_missing_col boop boop
#'
#' @returns Updated dataframe.
#'
#' @noRd
format_sites <- function(df, in_format, out_format, drop_extra_col = FALSE,
    warn_missing_col = TRUE) {

  message("Reformatting data...")

  # Update columns ----
  var_names <- find_var_names(
    df = colnames_sites,
    in_format = in_format,
    out_format = out_format)
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
  if (out_format == "WQX" & "State Code" %in% colnames(df)) {
    df <- state_to_abb(df, "State Code")
  } else if (out_format == "WQdashboard" & "State" %in% colnames(df)) {
    df <- state_to_abb(df, "State")
  }

  message("Done")

  return(df)
}
