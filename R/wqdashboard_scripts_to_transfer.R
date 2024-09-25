#' Detect data format
#'
#' @description Uses column names to detect data format. (eg WQX, WQdashboard,
#'   Watershed Watch, etc)
#'
#' @param df Input dataframe.
#' @param df_colnames Dataframe with column name substitutions. Acceptable
#'  entries are `colnames_sites`, `colnames_results`.
#'
#' @returns Name of data format as string.
#'
#' @noRd
detect_column_format <- function(df, df_colnames){
  msg <- paste("\tDetecting format...")
  ok_colnames <- c(colnames_sites, colnames_results)
  if(!all(df_colnames %in% ok_colnames)){
    stop("Invalid df_colnames. Acceptable inputs: colnames_sites,
         colnames_results")
  }

  # Iterate through formats
  for (x in colnames(df_colnames)) {
    df_format <- df_colnames %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::filter_at(x, dplyr::all_vars(!is.na(.) & . != ""))
    # Check if df matches selected format
    chk <- unlist(df_format) %in% colnames(df)
    if(all(chk)){
      message(msg, x)
      return(x)
    }
  }
  stop(msg, "Invalid format. Acceptable formats include: ",
       paste(colnames(df_colnames), collapse = ", "), call. = FALSE)
}


#' Check for missing columns
#'
#' @description Produces error message if any columns are missing.
#'
#' @param df Input dataframe.
#' @param field List of column names.
#'
#' @noRd
check_column_missing <- function(df, field) {
  # Modified code from MassWateR::checkMWRsites

  chk <- field %in% colnames(df)
  if(any(!chk)){
    tochk <- field[!chk]
    stop("\tThe following columns are missing: ",
         paste(tochk, collapse = ", "), call. = FALSE)
  }
}

#' Ignore dq rows
#'
#' @description Updates check to ignore rows where `Qualifier` listed in
#'   `qaqc_fail`. Mini helper function for other QAQC checks.
#'
#' @param df Dataframe.
#' @param chk Existing QAQC check.
#'
#' @return Updated QAQC check.
#'
#' @noRd
skip_dq_rows <- function(df, chk) {
  if("Qualifier" %in% colnames(df)) {
    chk <- (chk | df$Qualifier %in% qaqc_fail)
  }
  return(chk)
}

#' Ignore qc rows
#'
#' @description Updates check to ignore rows where `Activity_Type` is
#'   `Quality Control`.
#'
#' @param df Dataframe.
#' @param chk Existing QAQC check.
#'
#' @return Updated QAQC check.
#'
#' @noRd
skip_qc_rows <- function(df, chk) {
  if("Activity_Type" %in% colnames(df)) {
    chk <- (chk | stringr::str_detect(df$Activity_Type, "Quality Control"))
  }
  return(chk)
}

#' Check for missing values
#'
#' @description Produces error message if any values are missing in column.
#'
#' @param df Input dataframe.
#' @param field List of column names.
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#' @param ignore_qc Boolean. If TRUE, ignores rows where `Activity_Type` starts
#'   with "Quality Control". Default TRUE.
#' @param is_stop Boolean. If TRUE, returns stop(). If FALSE, returns warning().
#'   Default TRUE.
#'
#' @noRd
check_val_missing <- function(df, field, ignore_dq = TRUE, ignore_qc = TRUE,
                              is_stop = TRUE) {
  # Modified code from MassWateR::checkMWRsites

  df_field <- df[field]
  chk <- !is.na(df_field)
  if(ignore_dq) { chk <- skip_dq_rows(df, chk) }
  if(ignore_qc) { chk <- skip_qc_rows(df, chk) }

  if(any(!chk)){
    rws <- which(!chk)
    msg <- paste("\t", field, "missing in rows", paste(rws, collapse = ", "))
    if (length(rws) > 20){
      msg <- paste("\t", field, "missing in", toString(length(rws)), "rows.")
    }
    if (is_stop == TRUE){
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }
}

#' Check value numeric
#'
#' @description Produces error message if column value is not in numeric format
#'
#' @param df Dataframe.
#' @param field Column name.
#' @param exceptions List of acceptable string values. Default NULL.
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#' @param ignore_qc Boolean. If TRUE, ignores rows where `Activity_Type` starts
#'   with "Quality Control". Default FALSE.
#'
#' @noRd
check_val_numeric <- function(df, field, exceptions = NULL, ignore_dq = TRUE,
                              ignore_qc = FALSE) {
  # Modified code from MassWateR::checkMWRsites

  typ <- df[field]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ))) | is.na(typ)
  if(ignore_dq) { chk <- skip_dq_rows(df, chk) }
  if(ignore_qc) { chk <- skip_qc_rows(df, chk) }
  if(!is.null(exceptions)){
    chk <- (chk | sapply(typ, function(x) x %in% exceptions))
  }

  if(any(!chk)){
    rws <- which(!chk)
    stop("\tNon-numeric entries for ", field, " found in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }
}

#' Format date column
#'
#' @description Checks if "Date" column is date format, converts to date if not.
#'
#' @param df Dataframe.
#' @param date_format Date format.
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#'
#' @noRd
format_date_col <- function(df, date_format, ignore_dq = TRUE) {
  chk <- mapply(lubridate::is.Date, df$Date)
  if (ignore_dq) {
    df_temp <- dplyr::filter(df, !Qualifier %in% qaqc_fail)
    chk <- mapply(lubridate::is.Date, df_temp$Date)
  }

  if(all(chk)){
    return(df)
  } else if (is.null(date_format)) {
    stop("Date format is missing", call. = FALSE)
  }

  date_var <- c("a", "A", "b", "B", "d", "H", "I", "j", "q", "m", "M", "p", "S",
                "OS", "U", "w", "W", "y", "Y", "z", "Om", "Op", "r", "R", "T")
  chk <- gsub("[^a-zA-Z]", "", date_format)
  chk <- gsub(paste(unlist(date_var), collapse = "|"), "", chk)

  if(length(chk) > 0 & chk != "") {
    stop("date_format contains invalid variables: ",
         paste(chk, collapse = ", "), call. = FALSE)
  }

  df <- dplyr::mutate(df,
                      Date = as.Date(lubridate::parse_date_time(as.character(Date), date_format,
                                                                quiet = TRUE)))

  chk <- !is.na(df$Date)
  chk2 <- chk
  if (ignore_dq) { chk <- skip_dq_rows(df, chk) }

  if(all(!chk) | all(!chk2)) {
    stop('Date column does not match format "', date_format, '"',
         call. = FALSE)
  } else if (any(!chk)) {
    rws <- which(!chk)
    stop("Date is improperly formatted in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }
  return(df)
}
