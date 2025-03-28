#' Rename variable
#'
#' @description
#' `rename_var()` uses a paired list (`old_varname`, `new_varname`) to generate
#' a list of name substitutions and update the input variable. If no match
#' found, leaves input variable as-is. Helper function for [rename_all_var()].
#'
#' @param in_var String. Variable to update.
#' @param old_varname,new_varname List. List old variable names in `old_varname`
#' and new variable names in `new_varname`. `old_varname` and `new_varname` are
#' a paired list, therefor they must be the same length and in the same order.
#' @param multiple Boolean. If `TRUE`, will return all matches for
#' `in_var`. If `FALSE`, only returns the first match. Default `FALSE`.
#'
#' @seealso [rename_all_var]
#'
#' @return String. If substitution found, provides updated variable name.
#' Otherwise returns input variable.
#'
rename_var <- function(in_var, old_varname, new_varname, multiple = FALSE) {
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

  if (!multiple && length(x) > 1) {
    x <- x[1]
  }

  return(x)
}

#' Rename all variables in column
#'
#' @description
#' `rename_all_var()` uses a paired list (`old_varname`, `new_varname`) to
#' generate a list of name substitutions and update all variables in the input
#' column. If no match found, leaves old variable name.
#'
#' @param .data Dataframe
#' @param col_name String. Column name.
#'
#' @inheritParams rename_var
#'
#' @seealso [rename_var], [col_to_numeric], [col_to_date], [col_to_state]
#'
#' @return Dataframe with updated variables in target column. If no
#' substitutions found, returns original dataframe.
rename_all_var <- function(.data, col_name, old_varname, new_varname) {
  # Check inputs
  chk <- col_name %in% colnames(.data)
  if (!chk) {
    stop("col_name not in dataframe")
  }
  chk <- c(is.na(old_varname), is.na(new_varname))
  if (all(chk)) {
    return(.data)
  } else if (any(chk)) {
    stop("old_varname and new_varname must not contain NA values")
  } else if (all(old_varname == new_varname)) {
    return(.data)
  } else if (length(old_varname) != length(new_varname)) {
    stop("old_varname and new_varname are different lengths")
  }

  # Update variable names
  dat <- .data %>%
    dplyr::mutate(
      {{ col_name }} := sapply(
        .data[[col_name]],
        function(x) rename_var(x, old_varname, new_varname),
        USE.NAMES = FALSE
      )
    )

  return(dat)
}

#' Check column for invalid variables
#'
#' @description
#' `warn_invalid_var()` checks if the input column contains any variables that
#' aren't included in `varlist`.
#'
#' @param dat Dataframe
#' @param col_name String. Column name.
#' @param varlist List. List of acceptable variable names.
#'
#' @seealso [rename_all_var]
#'
#' @return Updated dataframe.
warn_invalid_var <- function(dat, col_name, varlist) {
  chk <- col_name %in% colnames(dat)
  if (!chk) {
    stop("col_name not in dataframe")
  }

  x <- unique(dat[[col_name]])
  y <- setdiff(x, varlist)
  y <- y[!is.na(y)]

  if (length(y) > 0) {
    warning(
      "Invalid variables in ", col_name, ": ", paste(y, collapse = ", "),
      call. = FALSE
    )
  }
}
