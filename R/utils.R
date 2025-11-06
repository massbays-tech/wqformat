#' List variable name substitutions
#'
#' @description
#' `fetch_var()` is a helper function for [format_results()] and
#' [format_sites()]. It is designed to check various built-in lookup tables and
#' return a paired, ordered list of variable name substitutions in addition to
#' a list of all acceptable output variables.
#'
#' @section Formatting notes:
#' * The out_format column is sorted alphabetically. To change the sort order,
#' append `##||` (eg `01||`) to the start of the cell. Numbers must be exactly
#' two digits.
#' * When listing multiple column names, use the delimiter `|`. All column names
#' listed under `in_format` will be included, but `out_format` only includes
#' the **first** column name listed in each cell.
#' * Formatted cell example: 01||col1|col2|col3
#'
#' @param df Dataframe.
#' @param in_format,out_format String. Column names for the input
#' (`in_format`) and output formats (`out_format`).
#' @param limit_var Boolean. If `TRUE`, `keep_var` only includes the first,
#' default variable for each row in `out_format`. If `FALSE`, `keep_var`
#' includes both default and alternate values listed in `out_format`. Default
#' `FALSE`.
#'
#' @returns List containing three items: `old_names`, `new_names`, and
#' `keep_var`
#' * `old_names` and `new_names` are paired lists. They contain name
#' substitutions derived from the `in_format` and `out_format` columns.
#' * `keep_var` is a list of unique variables in the `out_format` column.
#'
#' @noRd
fetch_var <- function(df, in_format, out_format, limit_var = FALSE) {
  # Check input values
  chk <- c(in_format, out_format) %in% colnames(df)
  if (any(!chk)) {
    stop(
      "Invalid in_format or out_format. Acceptable values: ",
      paste(colnames(df), collapse = ", ")
    )
  }

  chk <- is.na(df[[out_format]])
  if (all(chk)) {
    stop("out_format is blank")
  }

  # Trim, prep data
  df <- df %>%
    dplyr::select(dplyr::all_of(c(in_format, out_format))) %>%
    dplyr::filter(!is.na(.data[[out_format]])) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::arrange(.data[[out_format]]) %>%
    dplyr::mutate(
      {{ out_format }} := dplyr::if_else(
        grepl("||", .data[[out_format]], fixed = TRUE),
        substring(.data[[out_format]], 5),
        .data[[out_format]]
      )
    )

  # Set keep var
  keep_var <- unique_var(df, out_format, limit_var = limit_var)

  # If in_format == out_format, duplicate col, else trim/prep in_format
  if (in_format == out_format) {
    in_format <- paste0(in_format, "_1")

    df <- df %>%
      dplyr::mutate({{ in_format }} := .data[[out_format]])
  } else {
    df <- df %>%
      dplyr::filter(!is.na(.data[[in_format]])) %>%
      dplyr::mutate(
        {{ in_format }} := dplyr::if_else(
          grepl("||", .data[[in_format]], fixed = TRUE),
          substring(.data[[in_format]], 5),
          .data[[in_format]]
        )
      )
  }

  # Check - is dataframe empty?
  if (nrow(df) == 0) {
    return(
      list(
        old_names = NA,
        new_names = NA,
        keep_var = keep_var
      )
    )
  }

  # Drop extra out_format vars, split extra in_format vars
  df <- df %>%
    dplyr::mutate(
      {{ out_format }} := dplyr::if_else(
        grepl("|", .data[[out_format]], fixed = TRUE),
        stringr::str_split_i(.data[[out_format]], "\\|", 1),
        .data[[out_format]]
      )
    ) %>%
    tidyr::separate_longer_delim({{ in_format }}, "|")

  # Drop rows where in_format == out_format
  df <- df %>%
    dplyr::filter(.data[[in_format]] != .data[[out_format]])

  # Save var
  if (nrow(df) > 0) {
    old_names <- df[[in_format]]
    new_names <- df[[out_format]]
  } else {
    old_names <- NA
    new_names <- NA
  }

  list(
    old_names = old_names,
    new_names = new_names,
    keep_var = keep_var
  )
}

#' List unique values in column
#'
#' @description
#' `unique_var()` generates a list of unique variables in a column. Helper
#' function for [swap_col()], [swap_var()], and [format_mwr_results()].
#'
#' @param .data Dataframe.
#' @param col_name String. Name of column in dataframe.
#' @param delim String. Delimiter. Used to split variables in a cell. Default
#' value `|`.
#' @param limit_var If `TRUE`, `keep_var` only includes the first
#' variable in each cell. If `FALSE`, `keep_var` includes all variables listed
#' in each cell. Default `FALSE`.
#'
#' @returns List of unique variables in the selected column.
#'
#' @noRd
unique_var <- function(.data, col_name, delim = "|", limit_var = FALSE) {
  chk <- col_name %in% colnames(.data)
  if (!chk) {
    stop("col_name is invalid")
  }

  # Tidy data
  dat <- .data %>%
    dplyr::filter(!is.na(.data[[col_name]]))

  if (nrow(dat) == 0) {
    return(NA)
  }

  if (limit_var) {
    delim2 <- paste0("\\", delim)

    dat <- dat %>%
      dplyr::mutate(
        {{ col_name }} := dplyr::if_else(
          grepl(delim, .data[[col_name]], fixed = TRUE),
          stringr::str_split_i(.data[[col_name]], delim2, 1),
          .data[[col_name]]
        )
      )
  } else {
    dat <- dat %>%
      tidyr::separate_longer_delim({{ col_name }}, delim)
  }

  unique(dat[[col_name]])
}


#' Rename variable
#'
#' @description
#' `rename_var()` uses a paired list (`old_varname`, `new_varname`) to generate
#' a list of name substitutions and update the input variable. If no match
#' found, leaves input variable as-is. Helper function for [update_var()].
#'
#' @param in_var String. Variable to update.
#' @param old_varname,new_varname List. List old variable names in `old_varname`
#' and new variable names in `new_varname`. `old_varname` and `new_varname` are
#' a paired list, therefor they must be the same length and in the same order.
#' @param multiple Boolean. If `TRUE`, will return all matches for
#' `in_var`. If `FALSE`, only returns the first match. Default `FALSE`.
#'
#' @seealso [update_var]
#'
#' @return String. If substitution found, provides updated variable name.
#' Otherwise returns input variable.
#'
#' @noRd
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

#' Drop duplicate values from string
#'
#' @description
#' `str_unique()` drops duplicate values from string. Helper function for
#' [concat_col()].
#'
#' @param x String.
#' @param delim Delimiter used to separate items in string. Default ",".
#'
#' @returns String containing only unique values.
#'
#' @noRd
str_unique <- function(x, delim = ",") {
  x <- x %>%
    stringr::str_split(paste0("\\", delim)) %>%
    sapply(trimws, USE.NAMES = FALSE) %>%
    unique() %>%
    paste(collapse = delim)

  return(x)
}

#' Undo name repair for dataframe column names
#'
#' @description
#' `unrepair_names()` undoes name repair for input dataframe column names.
#' Helper function for [format_sites()], [format_results()], and
#' [format_mwr_results()].
#'
#' @param .data Dataframe
#' @param new_col List of unrepaired column names.
#'
#' @returns Dataframe with updated column names.
#'
#' @noRd
unrepair_names <- function(.data, new_col) {
  # Define variables
  new_col <- new_col[!is.na(new_col)]
  new_col <- gsub("..\\|\\|", "", new_col)
  new_col <- stringr::str_split(new_col, "\\|") %>%
    unlist() %>%
    unique()
  old_col <- make.names(new_col)

  # Drop instances where no changes are made
  chk <- new_col == old_col
  if (any(chk)) {
    old_col <- old_col[which(!chk)]
    new_col <- new_col[which(!chk)]
  }

  # Drop ambiguous name repair
  chk <- duplicated(old_col) | duplicated(old_col, fromLast = TRUE)
  if (any(chk)) {
    old_col <- old_col[which(!chk)]
    new_col <- new_col[which(!chk)]
  }

  if (length(old_col) == 0) {
    return(.data)
  }

  # Rename columns
  rename_col(.data, old_col, new_col)
}
