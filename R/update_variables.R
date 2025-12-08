#' Rename all variables in column
#'
#' @description
#' `update_var()` uses a paired list (`old_varname`, `new_varname`) to
#' generate a list of name substitutions and update all variables in the input
#' column. If no match found, leaves old variable name.
#'
#' @param .data Dataframe
#' @param col_name String. Column name.
#' @param old_varname,new_varname List. List old variable names in `old_varname`
#' and new variable names in `new_varname`. `old_varname` and `new_varname` are
#' a paired list, therefor they must be the same length and in the same order.
#'
#' @seealso [update_param], [update_unit], [warn_invalid_var]
#'
#' @return Dataframe with updated variables in target column. If no
#' substitutions found, returns original dataframe.
#'
#' @export
update_var <- function(.data, col_name, old_varname, new_varname) {
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
  } else if (length(old_varname) != length(new_varname)) {
    stop("old_varname and new_varname are different lengths")
  } else if (all(old_varname == new_varname)) {
    return(.data)
  }

  # Update variable names
  .data %>%
    dplyr::mutate(
      {{ col_name }} := sapply(
        .data[[col_name]],
        function(x) rename_var(x, old_varname, new_varname),
        USE.NAMES = FALSE
      )
    )
}

#' Rename parameters
#'
#' @description
#' `update_param()` converts parameters in target column to new format.
#'
#' @param .data Dataframe
#'
#' @inheritParams update_var
#' @inheritParams format_results
#'
#' @seealso [update_var], [update_unit]
#'
#' @return Dataframe with updated parameters in target column
#'
#' @export
update_param <- function(.data, col_name, in_format, out_format) {
  # Check inputs
  chk <- col_name %in% colnames(.data)
  if (!chk) {
    stop("col_name not in dataframe")
  }

  # Update parameters
  param <- fetch_var(
    varnames_parameters,
    tolower(in_format),
    tolower(out_format)
  )
  dat <- update_var(.data, col_name, param$old_names, param$new_names)
  warn_invalid_var(dat, col_name, param$keep_var)

  dat
}

#' Rename units
#'
#' @description
#' `update_unit()` converts units in target column to new format.
#'
#' @param .data Dataframe
#'
#' @inheritParams update_var
#' @inheritParams format_results
#'
#' @seealso [update_var], [update_unit]
#'
#' @return Dataframe with updated units in target column
#'
#' @export
update_unit <- function(.data, col_name, in_format, out_format) {
  # Check inputs
  chk <- col_name %in% colnames(.data)
  if (!chk) {
    stop("col_name not in dataframe")
  }

  # Update units
  unit_list <- fetch_var(
    varnames_units,
    tolower(in_format),
    tolower(out_format)
  )
  dat <- update_var(.data, col_name, unit_list$old_names, unit_list$new_names)
  warn_invalid_var(dat, col_name, unit_list$keep_var)

  dat
}

#' Check column for invalid variables
#'
#' @description
#' `warn_invalid_var()` checks if the input column contains any variables that
#' aren't included in `varlist`.
#'
#' @param df Dataframe
#' @param col_name String. Column name.
#' @param varlist List. List of acceptable variable names.
#'
#' @seealso [update_var]
#'
#' @return Updated dataframe.
#'
#' @export
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

#' Convert state names to abbreviations
#'
#' @description `state_to_abb` changes all state names in selected column to
#' abbreviations.
#'
#' @param .data Dataframe.
#' @param state_col String. Name of state column.
#'
#' @seealso [abb_to_state]
#'
#' @returns
#' Updated dataframe where all state names in `state_col` have been converted
#' to abbreviations.
#'
#' @export
state_to_abb <- function(.data, state_col) {
  chk <- .data[[state_col]] %in% c(
    NA, datasets::state.abb, datasets::state.name
  )
  if (any(!chk)) {
    bad_state <- .data[[state_col]][which(!chk)]
    warning(
      paste(unique(bad_state), collapse = ", "),
      " is not a valid state name"
    )
  }

  .data %>%
    dplyr::mutate(
      {{ state_col }} := dplyr::if_else(
        .data[[state_col]] %in% datasets::state.name,
        datasets::state.abb[match(.data[[state_col]], datasets::state.name)],
        .data[[state_col]]
      )
    )
}

#' Convert state abbreviations to names
#'
#' @description `abb_to_state` changes all state abbreviations in selected
#' column to state names.
#'
#' @param .data Dataframe.
#' @param state_col String. Name of state column.
#'
#' @seealso [state_to_abb]
#'
#' @returns
#' Updated dataframe where all state abbreviations in `state_col` have been
#' converted to names.
#'
#' @export
abb_to_state <- function(.data, state_col) {
  chk <- .data[[state_col]] %in% c(
    NA, datasets::state.abb, datasets::state.name
  )
  if (any(!chk)) {
    bad_state <- .data[[state_col]][which(!chk)]
    warning(
      paste(unique(bad_state), collapse = ", "),
      " is not a valid state name"
    )
  }

  .data %>%
    dplyr::mutate(
      {{ state_col }} := dplyr::if_else(
        .data[[state_col]] %in% datasets::state.abb,
        datasets::state.name[match(.data[[state_col]], datasets::state.abb)],
        .data[[state_col]]
      )
    )
}
