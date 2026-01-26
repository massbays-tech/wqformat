#' Rename columns
#'
#' @description
#' `rename_col()` renames dataframe columns by using a paired list to replace
#' `old_colnames` with `new_colnames`. If no match is found for an existing
#' column name, it is left as-is.
#'
#' @param .data Dataframe
#' @param old_colnames,new_colnames List. Columns listed in `old_colnames` will
#' be renamed to match paired name in `new_colnames`. `old_colnames` and
#' `new_colnames` must be the same length and in the same order.
#'
#' @seealso [concat_col]
#'
#' @returns Dataframe with updated column names.
#'
#' @export
rename_col <- function(.data, old_colnames, new_colnames) {
  # Check inputs
  if (length(old_colnames) != length(new_colnames)) {
    stop("old_colnames and new_colnames are different lengths")
  }

  dat <- .data

  dat_colnames <- data.frame(
    old_name = old_colnames,
    new_name = new_colnames
  ) %>%
    dplyr::filter(!is.na(.data$old_name)) %>%
    dplyr::filter(!is.na(.data$new_name)) %>%
    dplyr::filter(.data$old_name %in% colnames(dat))

  if (nrow(dat_colnames) == 0) {
    return(.data)
  }

  # Check if duplicate values in new_colnames
  chk <- duplicated(dat_colnames$new_name)
  if (any(chk)) {
    # List duplicate new_name
    dup_val <- unique(dat_colnames$new_name[which(chk)])

    # Iterate through each duplicate new_name
    for (val in dup_val) {
      dat_temp <- dplyr::filter(dat_colnames, .data$new_name == val)
      target_col <- dat_temp$old_name

      # Add new column, set to first non-NA value from old columns
      dat <- concat_col(dat, target_col, val)

      # Drop redundant conversions from dat_colnames
      dat_colnames <- dplyr::filter(dat_colnames, .data$new_name != val)

      # Check if old columns needed for further conversions, else drop
      chk <- target_col %in% dat_colnames$old_name
      if (any(!chk)) {
        drop_col <- target_col[which(!chk)]
        dat <- dplyr::select(dat, !dplyr::any_of(drop_col))
      }
    }
  }

  if (nrow(dat_colnames) == 0) {
    return(dat)
  }

  # Check if duplicate values in old_colnames
  chk <- duplicated(dat_colnames$old_name)
  if (any(chk)) {
    # List duplicate old_name
    dup_val <- unique(dat_colnames$old_name[which(chk)])
    dat_temp <- dplyr::filter(dat_colnames, .data$old_name %in% dup_val)

    # Add new columns
    for (i in seq_len(nrow(dat_temp))) {
      old_col <- dat_temp$old_name[i]
      new_col <- dat_temp$new_name[i]

      dat[[new_col]] <- dat[[old_col]]
    }

    # Drop old columns, update dat_colnames
    dat <- dplyr::select(dat, !dplyr::any_of(dup_val))
    dat_colnames <- dplyr::filter(dat_colnames, !.data$old_name %in% dup_val)
  }

  if (nrow(dat_colnames) == 0) {
    return(dat)
  }

  # Rename columns
  field_subs <- dat_colnames$new_name
  names(field_subs) <- dat_colnames$old_name

  dat <- dplyr::rename_with(dat, ~field_subs, names(field_subs))
}

#' Concatenate Columns
#'
#' @description
#' `concat_col()` combines values from multiple columns.
#' * If `concat` is `TRUE`, the output column will include the concatenated
#' data from all the input columns.
#' * If `concat` is `FALSE`, the output column will contain the first non-`NA`
#' value from the input columns.
#'
#' @param .data Dataframe.
#' @param in_colnames List. Input column names.
#' * Columns will be assessed in the order they are listed
#' * The first non-`NA` value will be transferred to `out_colname`
#' * If all columns contain `NA` values, then the value for `out_colname` value
#' will also be `NA`.
#' @param out_colname String. Name of column to transfer values to.
#' * Will create new column if `out_colname` is missing from dataframe.
#' @param concat Boolean. If `TRUE`, the values for the input columns will be
#' concatenated and saved in the output column. If `FALSE`, only the first
#' non-`NA` value will be saved in the output column. Default `FALSE`.
#'
#' @seealso [rename_col]
#'
#' @returns Updated dataframe.
#' * If `concat` is `TRUE`, the output column will include the concatenated
#' data from all the input columns.
#' * If `concat` is `FALSE`, the output column will contain the first non-`NA`
#' value from the input columns.
#'
#' @export
concat_col <- function(.data, in_colnames, out_colname, concat = FALSE) {
  if (!out_colname %in% colnames(.data)) {
    .data[[out_colname]] <- NA
  }

  in_colnames <- intersect(in_colnames, colnames(.data))

  if (length(in_colnames) == 0) {
    return(.data)
  } else if (length(in_colnames) == 1) {
    .data[[out_colname]] <- .data[[in_colnames]]
    return(.data)
  }

  dat <- .data %>%
    tidyr::unite(
      {{ out_colname }},
      dplyr::any_of(in_colnames),
      sep = "|",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      {{ out_colname }} := gsub(
        "NA\\|", "", .data[[out_colname]]
      )
    )

  if (concat) {
    dat <- dat %>%
      dplyr::mutate(
        {{ out_colname }} := gsub(
          "\\|NA", "", .data[[out_colname]]
        )
      ) %>%
      dplyr::mutate(
        {{ out_colname }} := sapply(
          .data[[out_colname]],
          function(x) str_unique(x, "|"),
          USE.NAMES = FALSE
        )
      ) %>%
      dplyr::mutate(
        {{ out_colname }} := gsub(
          "\\|", "; ", .data[[out_colname]]
        )
      )
  } else {
    dat <- dat %>%
      dplyr::mutate(
        {{ out_colname }} :=
          stringr::str_split_i(.data[[out_colname]], "\\|", 1)
      )
  }

  dat <- dat %>%
    dplyr::mutate(
      {{ out_colname }} := dplyr::if_else(
        .data[[out_colname]] == "NA",
        NA,
        .data[[out_colname]]
      )
    )

  return(dat)
}

#' Convert column to numeric format
#'
#' @description
#' If all values in column are numbers, `col_to_numeric()` converts column type
#' to numeric.
#'
#' @param .data Dataframe.
#' @param col_name String. Column name.
#' @param silent Boolean. If TRUE, does not provide warning if unable to set
#' column to numeric. If FALSE, provides error message. Default TRUE.
#'
#' @seealso [col_to_date]
#'
#' @returns
#' If all values are numeric, converts column to numeric and returns dataframe.
#' If there are any non-numeric values, leaves column as-is and returns
#' dataframe.
#'
#' @export
col_to_numeric <- function(.data, col_name, silent = TRUE) {
  if (is.numeric(.data[[col_name]])) {
    return(.data)
  }

  .data <- .data %>%
    dplyr::mutate(
      {{ col_name }} := dplyr::if_else(
        .data[[col_name]] %in% c("", " "),
        NA,
        .data[[col_name]]
      )
    )

  typ <- .data[col_name]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ))) | is.na(typ)

  if (all(chk)) {
    .data[[col_name]] <- as.numeric(.data[[col_name]])
  } else if (!silent) {
    stop(
      "Non-numeric values detected in ", col_name, ". Check rows: ",
      paste(which(!chk), collapse = ", ")
    )
  }

  return(.data)
}

#' Convert column to date or datetime format
#'
#' @description
#' `col_to_date` converts column to date or datetime format.
#'
#' @param .data Dataframe.
#' @param date_col String. Name of date column.
#' @param date_format String. Date format. Uses the same formatting as
#' [lubridate::parse_date_time()]. Default value "m/d/Y".
#' @param tz String. Specifies the timezone used to parse dates. Uses system
#' timezone as default.
#' @param datetime Boolean. If `TRUE`, returns column in datetime format. If
#' `FALSE`, returns column in date format. Default `FALSE`.
#'
#' @seealso [col_to_numeric]
#'
#' @return
#' Converts column to date or datetime and returns dataframe
#'
#' @export
col_to_date <- function(.data, date_col, date_format = "m/d/Y",
                        tz = Sys.timezone(), datetime = FALSE) {
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
    "Om", "Op", "OS", "a", "A", "b", "B", "d", "j", "q", "m", "U", "w", "W",
    "y", "Y", "H", "I", "M", "p", "S", "r", "R", "T", "z"
  )
  chk_var <- gsub("[^a-zA-Z]", "", date_format)
  chk_var <- gsub(paste(unlist(date_var), collapse = "|"), "", chk_var)

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

  if (!datetime) {
    dat[[date_col]] <- as.Date(dat[[date_col]], tz = tz)
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
