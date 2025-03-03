#' Rename variable
#'
#' @description Converts value from old variable to new variable name. If no
#'   match found, leaves old variable name. Helper function for
#'   `rename_all_var`.
#'
#' @param in_var String. Variable to update.
#' @param old_varname List. Old variable names; must be same length and in
#'   same order as `new_varname`.
#' @param new_varname List. New variable names; must be same length and in
#'   same order as `old_varname`.
#' @param allow_multiple Boolean. If \code{TRUE}, will return all matches for
#'  `in_var`. If \code{FALSE}, only returns the first match. Default
#'  \code{FALSE}.
#'
#' @return String. Updated variable name.
rename_var <- function(
    in_var, old_varname, new_varname, allow_multiple = FALSE) {
  if (!in_var %in% old_varname) {
    return(in_var)
  }

  chk <- c(is.na(old_varname), is.na(new_varname))
  if (any(chk)) {
    stop("old_varname and new_varname must not contain NA values")
  } else if (length(old_varname) != length(new_varname)) {
    stop("old_varname and new_varname must be the same length")
  }

  names(new_varname) <- old_varname
  x <- new_varname[names(new_varname) == in_var]
  names(x) <- NULL

  if (!allow_multiple && length(x) > 1) {
    x <- x[1]
  }

  return(x)
}

#' Rename all variables in column
#'
#' @description Converts values in column from old variable names to new
#'   new variable names. If no match found, leaves old variable name.
#'
#' @param df Input dataframe.
#' @param col_name String. Column name.
#' @param old_varname List. Old variable names; must be same length and in
#'   same order as `new_varname`.
#' @param new_varname List. New variable names; must be same length and in
#'   same order as `old_varname`.
#'
#' @return Updated dataframe.
rename_all_var <- function(df, col_name, old_varname, new_varname) {
  # Check inputs
  chk <- col_name %in% colnames(df)
  if (!chk) {
    stop("col_name not in dataframe")
  }
  chk <- c(is.na(old_varname), is.na(new_varname))
  if (all(chk)) {
    return(df)
  } else if (any(chk)) {
    stop("old_varname and new_varname must not contain NA values")
  } else if (all(old_varname == new_varname)) {
    return(df)
  } else if (length(old_varname) != length(new_varname)) {
    stop("old_varname and new_varname are different lengths")
  }

  # Update variable names
  df <- df %>%
    dplyr::mutate(
      {{ col_name }} := sapply(
        .data[[col_name]],
        function(x) rename_var(x, old_varname, new_varname)
      )
    ) %>%
    dplyr::mutate({{ col_name }} := unname(.data[[col_name]])) # remove names

  return(df)
}

#' Warn if Invalid Variables
#'
#' @description Checks if column contains variables that aren't included in the
#'  list of acceptable names.
#'
#' @param df Input dataframe.
#' @param col_name String. Column name.
#' @param varlist List. List of acceptable variable names.
#'
#' @return Updated dataframe.
warn_invalid_var <- function(df, col_name, varlist) {
  chk <- col_name %in% colnames(df)
  if (!chk) {
    stop("col_name not in dataframe")
  }

  x <- unique(df[[col_name]])
  y <- setdiff(x, varlist)
  y <- y[!is.na(y)]

  if (length(y) > 0) {
    warning(
      "Invalid variables in ", col_name, ": ", paste(y, collapse = ", "),
      call. = FALSE
    )
  }
}
