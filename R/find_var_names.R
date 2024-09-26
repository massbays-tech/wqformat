#' Find variable names
#'
#' @description Extracts list of old and new variable names from a dataframe. (reword this)
#'
#' @param df Dataframe with list of variables. (word better)
#' @param in_format Input format.
#' @param out_format Output format.
#'
#' @returns List of name conversions + all new variable names. (word better)
#'
#' @noRd
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

  # List new column names
  keep_var <- df[[out_format]]
  keep_var <- keep_var[!is.na(keep_var)]

  # Create matched list of old names, new names
  df <- df %>%
    dplyr::select(dplyr::all_of(c(in_format, out_format))) %>%
    dplyr::filter_at(out_format, dplyr::all_vars(!is.na(.)  & . != "")) %>%
    dplyr::filter_at(in_format, dplyr::all_vars(!is.na(.) & . != "")) %>%
    # If out_format includes multiple vars, only keep first value
    dplyr::mutate(
      {{out_format}} := dplyr::if_else(
        grepl("|", .data[[out_format]], fixed=TRUE),
        stringr::str_split_i(.data[[out_format]], "\\|", 1),
        .data[[out_format]]
        ))
  # If in_format includes multiple vars, split to multiple rows
  df <- tidyr::separate_longer_delim(df, {{in_format}}, "|") %>%
    dplyr::filter(.data[[in_format]] != .data[[out_format]])

  if (nrow(df) > 0) {
    old_names <- unlist(df[in_format])
    names(old_names) <- NULL
    new_names <- unlist(df[out_format])
    names(new_names) <- NULL
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
