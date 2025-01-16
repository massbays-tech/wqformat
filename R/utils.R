#' Column to Numeric
#'
#' @description Checks if column can be converted to numeric format. If TRUE,
#'  updates column type.
#'
#' @param df Dataframe.
#' @param field String. Name of column to convert to numeric.
#'
#' @returns Updated dataframe.
col_to_numeric <- function(df, field){
  if (is.numeric(df[[field]])) {
    return(df)
  }

  typ <- df[field]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ))) | is.na(typ)

  if(all(chk)){
    df <- dplyr::mutate(df, {{field}} := as.numeric(.data[[field]]))
  }

  return(df)
}
