#' Rename columns
#'
#' @description
#' `rename_col()` renames columns by substituting values from `new_colnames` for
#' values from `old_colnames`.
#'
#' @param .data Dataframe
#' @param old_colnames,new_colnames List. Columns listed in `old_colnames` will
#' be renamed to match paired name in `new_colnames`. `old_colnames` and
#' `new_colnames` must be the same length and in the same order.
#'
#' @seealso [concat_columns]
#'
#' @returns Dataframe with updated column names.
rename_col <- function(.data, old_colnames, new_colnames) {
  dat <- .data

  # Check inputs
  chk <- is.na(c(old_colnames, new_colnames))
  if (all(chk)) {
    return(dat)
  } else if (any(chk)) {
    stop("Can not include NA values in old_colnames or new_colnames")
  } else if (all(old_colnames == new_colnames)) {
    return(dat)
  } else if (length(old_colnames) != length(new_colnames)) {
    stop("old_colnames and new_colnames are different lengths")
  }

  dat_colnames <- data.frame(
    old_name = old_colnames,
    new_name = new_colnames
  ) %>%
    dplyr::filter(.data$old_name %in% colnames(dat))

  if (nrow(dat_colnames) == 0) {
    return(dat)
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
      dat <- concat_columns(dat, target_col, val)

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

  return(dat)
}

#' Concatenate Columns
#'
#' @description
#' `concat_columns()` combines values from multiple columns and saves the first
#'  non-`NA` value to the output column.
#'
#' @param .data Dataframe.
#' @param in_colnames List. Input column names.
#' * Columns will be assessed in the order they are listed
#' * The first non-`NA` value will be transferred to `out_colname`
#' * If all columns contain `NA` values, then the value for `out_colname` value
#' will also be `NA`.
#' @param out_colname String. Name of column to transfer values to.
#' * Will create new column if `out_colname` is missing from dataframe.
#'
#' @seealso [rename_col]
#'
#' @returns Updated dataframe where `out_colname` continues the first non-`NA`
#' value from `in_colnames`. If all values for `in_colnames` are `NA`, then
#' the value for `out_colname` will also be `NA`.
concat_columns <- function(.data, in_colnames, out_colname) {
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
    ) %>%
    dplyr::mutate(
      {{ out_colname }} := dplyr::case_when(
        grepl("|", .data[[out_colname]], fixed = TRUE) ~
          stringr::str_split_i(.data[[out_colname]], "\\|", 1),
        .data[[out_colname]] == "NA" ~ NA,
        TRUE ~ .data[[out_colname]]
      )
    )

  return(dat)
}
