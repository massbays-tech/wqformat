#' Find variable names
#'
#' @description Generates list of name substitutions from dataframe. Each column
#'    lists the variables for a given format, and each row lists equivalent
#'    variables. (UNCLEAR WORDING, REWRITE)
#'
#'    Alternate variables can be listed in a cell with the delimiter "|".
#'    Alternate variables in `in_format` will be included in output. Alternate
#'    variables in `out_format` will only be included if `multiple_out_var` is TRUE.
#'
#' @param df Dataframe.
#' @param in_format String. Name of column containing input variable names.
#' @param out_format String. Name of column containing output variable names.
#'
#' @returns List. `old_names` and `new_names` contain paired lists of variable
#'   names derived from `in_format` and `out_format` columns. Duplicate values
#'   are not included in lists. `keep_var` contains list of all variables
#'   listed in `in_format` column.
find_var_names <- function(df, in_format, out_format){

  # Check if df is dataframe
  chk <- inherits(df, "data.frame")
  if(!chk) {
    stop("df must be type dataframe")
  }

  # Check if dataframe includes in_format, out_format
  chk <- in_format %in% colnames(df)
  chk2 <- out_format %in% colnames(df)
  if (!chk & !chk2) {
    stop("Invalid in_format and out_format. Acceptable formats: ",
         paste(colnames(df), collapse=", "))
  } else if (!chk) {
    stop("Invalid in_format. Acceptable formats: ",
         paste(colnames(df), collapse=", "))
  } else if (!chk2) {
    stop("Invalid out_format. Acceptable formats: ",
         paste(colnames(df), collapse=", "))
  }

  # Create matched list of old names, new names; drop rows where out_format is NA
  # Convert all columns to character to prevent errors from numeric variables
  df <- df %>%
    dplyr::select(dplyr::all_of(c(in_format, out_format))) %>%
    dplyr::filter(!is.na(.data[[out_format]])) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Sort data by out_format; drop ##|| from start of vars if present
  df <- df %>%
    dplyr::arrange(.data[[out_format]]) %>%
    dplyr::mutate(
      {{in_format}} := dplyr::if_else(
        grepl("||", .data[[in_format]], fixed=TRUE),
        substring(.data[[in_format]], 5),
        .data[[in_format]]
      )
    ) %>%
    dplyr::mutate(
      {{out_format}} := dplyr::if_else(
        grepl("||", .data[[out_format]], fixed=TRUE),
        substring(.data[[out_format]], 5),
        .data[[out_format]]
      )
    )

  # Remove alternate out_format variables
  df <- df %>%
    dplyr::mutate(
      {{out_format}} := dplyr::if_else(
        grepl("|", .data[[out_format]], fixed=TRUE),
        stringr::str_split_i(.data[[out_format]], "\\|", 1),
        .data[[out_format]]
      )
    )

  # Set keep_var
  keep_var <- df[[out_format]]

  # Tidy data
  df <- df %>%
    # Drop rows where in_format is NA
    dplyr::filter(!is.na(.data[[in_format]])) %>%
    # If in_format includes alternate vars, split to multiple rows
    tidyr::separate_longer_delim({{in_format}}, "|") %>%
    # Drop rows where in_format == out_format
    dplyr::filter(.data[[in_format]] != .data[[out_format]])

  # Save var
  if (nrow(df) > 0) {
    old_names <- df[[in_format]]
    new_names <- df[[out_format]]
  } else {
    old_names <- NA
    new_names <- NA
  }

  return(
    list(
      old_names = old_names,
      new_names = new_names,
      keep_var = keep_var
    )
  )
}
