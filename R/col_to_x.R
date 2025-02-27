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

#' Column to Date
#'
#' @description Checks if column is formatted as a date. If column is not
#'   formatted as date, converts column to date type.
#' @param df Input dataframe.
#' @param date_col String. Name of date column.
#' @param date_format String. Date format. Uses the same formatting as
#'  `lubridate::parse_date_time`. Default value "m/d/Y".
#' @param tz String. Specifies the timezone used to parse dates. Defaults to
#'    system timezone.
col_to_date <- function(df, date_col, date_format="m/d/Y", tz=Sys.timezone()) {

  df_temp <- dplyr::filter(df, !is.na(.data[[date_col]]))
  chk <- mapply(lubridate::is.Date, df_temp[[date_col]])  # Update to check if datetime!

  if(all(chk)){
    return(df)
  } else if (is.null(date_format)) {
    stop("Date format is missing", call. = FALSE)
  }

  date_var <- c("Om", "a", "A", "b", "B", "d", "j", "q", "m", "U", "w", "W",
                "y", "Y")
  time_var <- c("Op", "OS", "H", "I", "M", "p", "S", "r", "R", "T", "z")
  chk_var <- gsub("[^a-zA-Z]", "", date_format)
  chk_time <- gsub(paste(unlist(date_var), collapse = "|"), "", chk_var)
  chk_var <- gsub(paste(unlist(time_var), collapse = "|"), "", chk_time)

  if(chk_var != "") {
    stop("date_format contains invalid variables: ", chk_var, call. = FALSE)
  }

  chk <- is.na(df[[date_col]])

  df <- df %>%
    dplyr::mutate(
      {{date_col}} := lubridate::parse_date_time(
        as.character(.data[[date_col]]),
        date_format,
        tz = tz,
        quiet = TRUE
      )
    )

  if(chk_time == "") {
    df[[date_col]] <- as.Date(df[[date_col]])
  }

  chk2 <- !is.na(df[[date_col]])
  chk <- chk | chk2

  if(all(!chk2)) {
    stop('Date does not match format "', date_format, '"',
         call. = FALSE)
  } else if (any(!chk)) {
    rws <- which(!chk)
    stop("Date is improperly formatted in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }

  return(df)
}

#' Format State Columns
#'
#' @description Updates all state names in column to either abbreviations or
#'  full names.
#'
#' @param df Dataframe.
#' @param state_col String. Name of state column.
#' @param full_name Boolean. If TRUE, updates `state_col` to include full state
#'  names. If FALSE, updates `state_col` to include state abbreviations. Default
#'  FALSE.
#'
#' @returns Updated dataframe.
col_to_state <- function(df, state_col, full_name = FALSE) {
  if (full_name) {
    df <- df %>%
      dplyr::mutate(
        {{state_col}} := dplyr::if_else(
          .data[[state_col]] %in% datasets::state.abb,
          datasets::state.name[match(.data[[state_col]], datasets::state.abb)],
          .data[[state_col]]
        )
      )
  } else {
    df <- df %>%
      dplyr::mutate(
        {{state_col}} := dplyr::if_else(
          .data[[state_col]] %in% datasets::state.name,
          datasets::state.abb[match(.data[[state_col]], datasets::state.name)],
          .data[[state_col]]
        )
      )
  }

  return(df)
}
