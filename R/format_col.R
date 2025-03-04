#' Convert column to numeric format
#'
#' @description
#' If all values in column are numbers, `col_to_numeric()` converts column type
#' to numeric.
#'
#' @param .data Dataframe.
#' @param col_name String. Column name.
#'
#' @seealso [col_to_date], [col_to_state], [rename_all_var]
#'
#' @returns
#' If all values are numeric, converts column to numeric and returns dataframe.
#' If there are any non-numeric values, leaves column as-is and returns
#' dataframe.
col_to_numeric <- function(.data, col_name) {
  if (is.numeric(.data[[col_name]])) {
    return(.data)
  }

  typ <- .data[col_name]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ))) | is.na(typ)

  if (all(chk)) {
    .data[[col_name]] <- as.numeric(.data[[col_name]])
  }

  return(.data)
}

#' Convert column to date or datetime format
#'
#' @description
#' `col_to_date` converts column to date or datetime format.
#' * Uses date format by default
#' * Will use datetime format if time variables are included in `date_format`
#'
#' @param .data Dataframe.
#' @param date_col String. Name of date column.
#' @param date_format String. Date format. Uses the same formatting as
#' [lubridate::parse_date_time()]. Default value "m/d/Y".
#' @param tz String. Specifies the timezone used to parse dates. Uses system
#' timezone as default.
#'
#' @seealso [col_to_numeric], [col_to_state], [rename_all_var]
#'
#' @return
#' Converts column to date or datetime and returns dataframe
col_to_date <- function(.data, date_col, date_format = "m/d/Y",
                        tz = Sys.timezone()) {
  if (!date_col %in% colnames(.data)) {
    stop(date_col, " is not a valid column")
  }

  chk <- inherits(.data[[date_col]], c("Date", "POSIXt"))
  if (chk) {
    return(.data)
  }

  chk <- is.na(.data[[date_col]])
  if (all(chk)) {
    .data[[date_col]] <- as.Date(.data[[date_col]])
    return(.data)
  }

  if (date_format %in% c("", " ", NA)) {
    stop("Date format is missing", call. = FALSE)
  }

  date_var <- c(
    "Om", "a", "A", "b", "B", "d", "j", "q", "m", "U", "w", "W",
    "y", "Y"
  )
  time_var <- c("Op", "OS", "H", "I", "M", "p", "S", "r", "R", "T", "z")
  chk_var <- gsub("[^a-zA-Z]", "", date_format)
  chk_time <- gsub(paste(unlist(date_var), collapse = "|"), "", chk_var)
  chk_var <- gsub(paste(unlist(time_var), collapse = "|"), "", chk_time)

  if (chk_var != "") {
    stop("date_format contains invalid variables: ", chk_var, call. = FALSE)
  }

  chk <- is.na(.data[[date_col]])

  dat <- .data %>%
    dplyr::mutate(
      {{ date_col }} := lubridate::parse_date_time(
        as.character(.data[[date_col]]),
        date_format,
        tz = tz,
        quiet = TRUE
      )
    )

  if (chk_time == "") {
    dat[[date_col]] <- as.Date(dat[[date_col]])
  }

  chk2 <- !is.na(dat[[date_col]])
  chk <- chk | chk2

  if (all(!chk2)) {
    stop(
      'Date does not match format "', date_format, '"',
      call. = FALSE
    )
  } else if (any(!chk)) {
    rws <- which(!chk)
    stop(
      "Date is improperly formatted in rows: ",
      paste(rws, collapse = ", "),
      call. = FALSE
    )
  }

  return(dat)
}

#' Format state names in column
#'
#' @description Formats all state names in column as either abbreviations or
#'  full names.
#'
#' @param .data Dataframe.
#' @param state_col String. Name of state column.
#' @param abb Boolean. If `TRUE`, uses abbreviations. If `FALSE`, uses full
#' state names. Default `TRUE`.
#'
#' @seealso [col_to_numeric], [call_to_date], [rename_all_var]
#'
#' @returns
#' * If abb is `TRUE`, converts all state names in column to abbreviations
#' * If abb is `FALSE`, converts all state abbreviations in column to names
col_to_state <- function(.data, state_col, abb = TRUE) {
  chk <- .data[[state_col]] %in% c(
    NA, datasets::state.abb, datasets::state.name
  )
  if (any(!chk)) {
    bad_state <- .data[[state_col]][which(!chk)]
    warning(
      paste(unique(bad_state), collapse = ", "),
      " is not a valid state name"
    )
  }

  if (abb) {
    dat <- .data %>%
      dplyr::mutate(
        {{ state_col }} := dplyr::if_else(
          .data[[state_col]] %in% datasets::state.name,
          datasets::state.abb[match(.data[[state_col]], datasets::state.name)],
          .data[[state_col]]
        )
      )
  } else {
    dat <- .data %>%
      dplyr::mutate(
        {{ state_col }} := dplyr::if_else(
          .data[[state_col]] %in% datasets::state.abb,
          datasets::state.name[match(.data[[state_col]], datasets::state.abb)],
          .data[[state_col]]
        )
      )
  }

  return(dat)
}
