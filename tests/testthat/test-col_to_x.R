#' Column to Numeric
#'
#' @description Checks if column can be converted to numeric format. If TRUE,
#'  updates column type.
#'
#' @param dat Dataframe.
#' @param field String. Name of column to convert to numeric.
#'
#' @returns Updated dataframe.
col_to_numeric <- function(dat, field){
  if (is.numeric(dat[[field]])) {
    return(dat)
  }

  typ <- dat[field]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ))) | is.na(typ)

  if(all(chk)){
    dat <- dplyr::mutate(dat, {{field}} := as.numeric(.data[[field]]))
  }

  return(dat)
}

#' Column to Date
#'
#' @description Checks if column is formatted as a date. If column is not
#'   formatted as date, converts column to date type.
#' @param dat Input dataframe.
#' @param date_col String. Name of date column.
#' @param date_format String. Uses the same formatting as
#'  `lubridate::parse_date_time`. Default value "m/d/Y".
#'
#' @noRd
col_to_date <- function(dat, date_col, date_format="m/d/Y") {

  dat_temp <- dplyr::filter(dat, !is.na(.data[[date_col]]))
  chk <- mapply(lubridate::is.Date, dat_temp[[date_col]])

  if(all(chk)){
    return(dat)
  } else if (is.null(date_format)) {
    stop("Date format is missing", call. = FALSE)
  }

  date_var <- c("OS", "Om", "Op", "a", "A", "b", "B", "d", "H", "I", "j", "q",
                "m", "M", "p", "S", "U", "w", "W", "y", "Y", "z", "r", "R", "T")
  chk_var <- gsub("[^a-zA-Z]", "", date_format)
  chk_var <- gsub(paste(unlist(date_var), collapse = "|"), "", chk_var)

  if(chk_var != "") {
    stop("date_format contains invalid variables: ", chk_var, call. = FALSE)
  }

  chk <- is.na(dat[[date_col]])

  dat <- dat %>%
    dplyr::mutate(
      {{date_col}} := as.Date(
        lubridate::parse_date_time(
          as.character(.data[[date_col]]),
          date_format,
          quiet = TRUE)))

  chk2 <- !is.na(dat[[date_col]])
  chk <- chk | chk2

  if(all(!chk2)) {
    stop('Date does not match format "', date_format, '"',
         call. = FALSE)
  } else if (any(!chk)) {
    rws <- which(!chk)
    stop("Date is improperly formatted in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }

  return(dat)
}

#' Format State Columns
#'
#' @description Updates all state names in column to either abbreviations or
#'  full names.
#'
#' @param dat Dataframe.
#' @param state_col String. Name of state column.
#' @param full_name Boolean. If TRUE, updates `state_col` to include full state
#'  names. If FALSE, updates `state_col` to include state abbreviations. Default
#'  FALSE.
#'
#' @returns Updated dataframe.
col_to_state <- function(dat, state_col, full_name = FALSE) {
  if (full_name) {
    dat <- dat %>%
      dplyr::mutate(
        {{state_col}} := dplyr::if_else(
          .data[[state_col]] %in% state.abb,
          state.name[match(.data[[state_col]], state.abb)],
          .data[[state_col]]
        )
      )
  } else {
    dat <- dat %>%
      dplyr::mutate(
        {{state_col}} := dplyr::if_else(
          .data[[state_col]] %in% state.name,
          state.abb[match(.data[[state_col]], state.name)],
          .data[[state_col]]
        )
      )
  }

  return(dat)
}
