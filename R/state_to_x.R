#' Format states as abbreviations
#'
#' @description Converts all state names in column to abbreviations.
#'
#' @param df Dataframe.
#' @param state_col Name of state column.
#'
#' @noRd
state_to_abb <- function(df, state_col) {
  df <- df %>%
    dplyr::mutate(
      {{state_col}} := dplyr::if_else(
        .data[[state_col]] %in% state.name,
        state.abb[match(.data[[state_col]], state.name)],
        .data[[state_col]]
      )
    )
  return(df)
}

#' Format state column as full names
#'
#' @description Converts all state abbreviations in column to full names.
#'
#' @param df Dataframe.
#' @param state_col Name of state column.
#'
#' @noRd
state_to_name <- function(df, state_col) {
  df <- df %>%
    dplyr::mutate(
      {{state_col}} := dplyr::if_else(
        .data[[state_col]] %in% state.abb,
        state.name[match(.data[[state_col]], state.abb)],
        .data[[state_col]]
      )
    )
  return(df)
}
