#' Rename columns
#'
#' @description Rename columns from `old_colnames` to `new_colnames`. If no
#'   match found, leaves old column name.
#'
#' @param df Dataframe.
#' @param old_colnames List. Old column names; must be same length and in
#'   same order as `new_colnames`.
#' @param new_colnames List. New column names; must be same length and in
#'   same order as `old_colnames`.
#'
#' @returns Updated dataframe.
rename_col <- function(df, old_colnames, new_colnames){

  # Check inputs
  chk <- all(is.na(old_colnames))
  chk2 <- all(is.na(new_colnames))
  if (chk & chk2) {
    return(df)
  } else if (chk | chk2) {
    stop("old_colnames and new_colnames are different lengths")
  } else if (all(old_colnames == new_colnames)) {
    return(df)
  } else if (length(old_colnames) != length(new_colnames)) {
    stop("old_colnames and new_colnames are different lengths")
  }

  # Rename columns
  names(new_colnames) <- old_colnames
  new_colnames <- new_colnames[!is.na(new_colnames)]
  new_colnames <- new_colnames[!is.na(names(new_colnames))]
  field_subs <- new_colnames[intersect(colnames(df), names(new_colnames))]

  if (length(field_subs) > 0) {
    df <- dplyr::rename_with(df, ~ field_subs, names(field_subs))
  }

  return(df)
}
