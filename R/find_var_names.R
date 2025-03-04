#' Fetch list of variable name substitutions
#'
#' @description
#' `fetch_var()` generates list of name substitutions. Helper function for
#' [format_results()] and [format_sites()].
#'
#' @section How table data is interpreted:
#' * Each column is sorted alphabetically. If variables need to be listed in a
#' specific order, then append ##|| (eg "01||") to the start of the variable.
#' Numbers must be exactly two digits. If variable contains "||", then the first
#' four letters for the variable will be dropped after alphabetizing but
#' before further processing.
#' * Alternate variable names can be listed in a cell with the delimiter "|".
#' Alternate variable names for `in_format` will be included in the output.
#' Alternate variable names for `out_format` will be dropped.
#'
#' @param in_table Dataframe.
#' @param in_format,out_format String. Column names for the input format
#' (`in_format`) and output format (`out_format`).
#'
#' @returns List containing three items: `old_names`, `new_names`, and
#' `keep_var`
#' * `old_names` and `new_names` are paired lists. They contain name
#' substitutions derived from the `in_format` and `out_format` columns. Matching
#' pairs between `in_format` and `out_format` are removed.
#' * `keep_var` is a list of unique variables in the `out_format` column.
#' Matching pairs between `in_format` and `out_format` are kept.
#'
fetch_var <- function(in_table, in_format, out_format) {
  # Check input values
  chk <- inherits(in_table, "data.frame")
  if (!chk) {
    stop("in_table must be a dataframe")
  }

  chk <- in_format %in% colnames(in_table)
  chk2 <- out_format %in% colnames(in_table)
  if (!chk && !chk2) {
    stop(
      "Invalid in_format and out_format. Acceptable formats: ",
      paste(colnames(in_table), collapse = ", ")
    )
  } else if (!chk) {
    stop(
      "Invalid in_format. Acceptable formats: ",
      paste(colnames(in_table), collapse = ", ")
    )
  } else if (!chk2) {
    stop(
      "Invalid out_format. Acceptable formats: ",
      paste(colnames(in_table), collapse = ", ")
    )
  }

  # Create matched list of old names, new names
  # Drop rows where out_format is NA
  # Convert all columns to character to prevent errors from numeric variables
  in_table <- in_table %>%
    dplyr::select(dplyr::all_of(c(in_format, out_format))) %>%
    dplyr::filter(!is.na(.data[[out_format]])) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

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
    tidyr::separate_longer_delim({{ in_format }}, "|") %>%
    # Drop rows where in_format == out_format
    dplyr::filter(.data[[in_format]] != .data[[out_format]])

  # Save var
  if (nrow(in_table) > 0) {
    old_names <- in_table[[in_format]]
    new_names <- in_table[[out_format]]
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
