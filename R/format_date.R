#' Format date column
#'
#' @description Checks if "Date" column is date format, converts to date if not.
#'
#' @param df Dataframe.
#' @param date_col Name of date column.
#' @param date_format Date format.
#'
#' @noRd
format_date <- function(df, date_col, date_format="m/d/Y") {

  df_temp <- dplyr::filter(df, !is.na(.data[[date_col]]))
  chk <- mapply(lubridate::is.Date, df_temp[[date_col]])

  if(all(chk)){
    return(df)
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

  chk <- is.na(df[[date_col]])

  df <- df %>%
    dplyr::mutate(
      {{date_col}} := as.Date(
        lubridate::parse_date_time(
          as.character(.data[[date_col]]),
          date_format,
          quiet = TRUE)))

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
