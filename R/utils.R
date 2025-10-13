#' Fetch variable name substitutions
#'
#' @description
#' `fetch_var()` generates a list of name substitutions. Helper function for
#' [format_results()] and [format_sites()].
#'
#' @section How in_table is interpreted:
#' * Each column is sorted alphabetically. If variables need to be listed in a
#' specific order, then append ##|| (eg "01||") to the start of the variable.
#' Numbers must be exactly two digits. If variable contains "||", then the first
#' four letters for the variable will be dropped after alphabetizing but
#' before further processing.
#' * Multiple variable names can be listed in a cell with the delimiter "|".
#' All variable names for `in_format` will be included in the output. Only the
#' first variable in an `out_format` cell will be included.
#' * Formatted cell example: 01||var1|var2|var3
#'
#' @param in_table Dataframe.
#' @param in_format,out_format String. Column names for the input format
#' (`in_format`) and output format (`out_format`).
#' @param name_repair Boolean. If TRUE, converts all `in_format` variables to
#' syntactically valid names by replacing all non alphanumeric or "_" characters
#' with periods. Default FALSE.
#'
#' @returns List containing three items: `old_names`, `new_names`, and
#' `keep_var`
#' * `old_names` and `new_names` are paired lists. They contain name
#' substitutions derived from the `in_format` and `out_format` columns. Matching
#' pairs between `in_format` and `out_format` are removed.
#' * `keep_var` is a list of unique variables in the `out_format` column.
#' Matching pairs between `in_format` and `out_format` are kept.
#'
#' @noRd
fetch_var <- function(in_table, in_format, out_format, name_repair = FALSE) {
  # Check input values
  chk <- inherits(in_table, "data.frame")
  if (!chk) {
    stop("in_table must be a dataframe")
  }

  chk <- c(in_format, out_format) %in% colnames(in_table)
  if (any(!chk)) {
    stop(
      "Invalid in_format or out_format. Acceptable formats: ",
      paste(colnames(in_table), collapse = ", ")
    )
  }

  chk <- is.na(in_table[[out_format]])
  if (all(chk)) {
    stop(
      "out_format is blank"
    )
  }

  # Create matched list of old names, new names
  # Drop rows where out_format is NA
  # Convert all columns to character to prevent errors from numeric variables
  in_table <- in_table %>%
    dplyr::select(dplyr::all_of(c(in_format, out_format))) %>%
    dplyr::filter(!is.na(.data[[out_format]])) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # If in_format == out_format, duplicate col
  if (in_format == out_format) {
    in_format <- paste0(in_format, "_1")

    in_table <- in_table %>%
      dplyr::mutate({{ in_format }} := .data[[out_format]])
  }

  # Sort data by out_format; drop ##|| from start of vars if present
  in_table <- in_table %>%
    dplyr::arrange(.data[[out_format]]) %>%
    dplyr::mutate(
      {{ in_format }} := dplyr::if_else(
        grepl("||", .data[[in_format]], fixed = TRUE),
        substring(.data[[in_format]], 5),
        .data[[in_format]]
      )
    ) %>%
    dplyr::mutate(
      {{ out_format }} := dplyr::if_else(
        grepl("||", .data[[out_format]], fixed = TRUE),
        substring(.data[[out_format]], 5),
        .data[[out_format]]
      )
    )

  # Remove alternate out_format variables
  in_table <- in_table %>%
    dplyr::mutate(
      {{ out_format }} := dplyr::if_else(
        grepl("|", .data[[out_format]], fixed = TRUE),
        stringr::str_split_i(.data[[out_format]], "\\|", 1),
        .data[[out_format]]
      )
    )

  # Set keep_var
  keep_var <- in_table[[out_format]]

  # Tidy data
  in_table <- in_table %>%
    # Drop rows where in_format is NA
    dplyr::filter(!is.na(.data[[in_format]])) %>%
    # If in_format includes alternate vars, split to multiple rows
    tidyr::separate_longer_delim({{ in_format }}, "|")

  # If name_repair is TRUE, update in_format
  if (name_repair) {
    in_table <- in_table %>%
      dplyr::mutate(
        {{ in_format }} := dplyr::if_else(
          is.na(.data[[in_format]]),
          NA,
          make.names(.data[[in_format]])
        )
      )
  }

  # Drop rows where in_format == out_format
  in_table <- in_table %>%
    dplyr::filter(.data[[in_format]] != .data[[out_format]])

  # Save var
  if (nrow(in_table) > 0) {
    old_names <- in_table[[in_format]]
    new_names <- in_table[[out_format]]
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
