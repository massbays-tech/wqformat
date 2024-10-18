#' Rename variable
#'
#' @description Updates variable name. Helper function for `rename_all_var`.
#'
#' @param in_var Variable to update.
#' @param old_varname List of old variable names.
#' @param new_varname List of new variable names.
#'
#' @return New variable name.
#'
#' @noRd
rename_var <- function(in_var, old_varname, new_varname) {
  names(new_varname) <- old_varname

  if (in_var %in% old_varname) {
    in_var <- new_varname[[in_var]]
  }

  return(in_var)
}

#' Rename all variables in column
#'
#' @description Updates variable names in column.
#'
#' @param df Input dataframe.
#' @param col_name Column name.
#' @param old_varname List of old variable names.
#' @param new_varname List of new variable names.
#'
#' @return Updated dataframe.
#'
#' @noRd
rename_all_var <- function(df, col_name, old_varname, new_varname) {

  # Check inputs
  chk <- col_name %in% colnames(df)
  if (!chk) {
    stop("col_name not in dataframe")
  }
  chk <- all(is.na(old_varname))
  chk2 <- all(is.na(new_varname))
  if (chk & chk2) {
    return(df)
  } else if (chk | chk2) {
    stop("old_varname and new_varname are different lengths")
  } else if (all(old_varname == new_varname)) {
    return(df)
  } else if (length(old_varname) != length(new_varname)) {
    stop("old_varname and new_varname are different lengths")
  }

  # Update variable names
  df <- df %>%
    dplyr::mutate(
      {{col_name}} := sapply(
        .data[[col_name]],
        function(x) rename_var(x, old_varname, new_varname))) %>%
    dplyr::mutate({{col_name}} := unname(.data[[col_name]])) # remove names

  return(df)
}
