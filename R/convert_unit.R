#' Convert unit
#'
#' @description `convert_unit()` calculates the value for `x` when converted
#' to a new unit.
#'
#' @param x Numeric. Value to convert.
#' @param old_unit String. Current unit.
#' @param new_unit String. New unit.
#' @param unit_format String. Format used for units. Not case sensitive.
#' Accepted values: WQX, MassWateR, wqdashboard, RI_WW, RI_DEM, MA_BRC, ME_DEP,
#' ME_FOCB. Default wqx.
#'
#' @return If `old_unit` and `new_unit` are compatible, returns updated value.
#' If they are incompatible, returns `-999999`.
#'
#' @export
convert_unit <- function(x, old_unit, new_unit, unit_format = "wqx") {
  if (is.na(x) || is.na(old_unit) || is.na(new_unit) || old_unit == new_unit) {
    return(x)
  }

  # Check input format
  ok_formats <- setdiff(
    colnames(varnames_units),
    c("Target_Unit", "To_X", "From_X")
  )
  unit_format <- tolower(unit_format)

  chk <- unit_format %in% ok_formats
  if (!chk) {
    stop(
      "unit_format is invalid. Acceptable values: ",
      paste(ok_formats, collapse = ", ")
    )
  }

  msg <- paste("Unable to convert", old_unit, "to", new_unit)

  # Update units to work with measurements::conv_unit()
  old_unit <- suppressWarnings(fetch_unit(old_unit, unit_format))
  new_unit <- suppressWarnings(fetch_unit(new_unit, unit_format))

  if (is.null(old_unit) || is.null(new_unit)) {
    warning(msg, ". Did not recognize units.")
    return(-999999)
  } else if (old_unit$target_unit != new_unit$target_unit) {
    warning(msg, ". Incompatible units.")
    return(-999999)
  }

  x <- gsub("x", x, old_unit$from)
  x <- gsub("x", x, new_unit$to)

  eval(parse(text = x))
}

#' Standardize units
#'
#' @description `standardize_units()` standardizes the unit used for each
#' parameter in `group`.
#'
#' @param .data Dataframe
#' @param group String. Column used to group data.
#' @param value String. Column with result values.
#' @param unit String. Column with units.
#' @param warn_only Boolean. If `TRUE`, returns a warning message if unable to
#' convert units. If `FALSE`, returns an error message.
#'
#' @inheritParams convert_unit
#'
#' @returns Updated dataframe with updated unit and result values. Rows that
#' can not be converted to a standard unit are left as-is and in informative
#' warning or error message is provided.
#'
#' @export
standardize_units <- function(
    .data, group, value, unit, unit_format = "wqx", warn_only = TRUE
  ) {
  # Set var
  dat <- .data %>%
    dplyr::filter(
      !is.na(.data[[group]]),
      !is.na(.data[[unit]])
    )

  # List parameters with multiple units, set target unit
  dat_group <- dat %>%
    dplyr::group_by(.data[[group]]) %>%
    dplyr::summarize(
      "n" = length(unique(.data[[unit]])),
      "temp_unit" = dplyr::last(.data[[unit]])
    ) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::select(!"n")

  if (nrow(dat_group) == 0) {
    return(.data)
  }

  # Simplify data, convert units
  dat <- dat %>%
    dplyr::select(dplyr::all_of(c(group, unit, value))) %>%
    unique()

  dat <- dplyr::inner_join(dat, dat_group, by = group) %>%
    dplyr::filter(.data[[unit]] != .data$temp_unit) %>%
    dplyr::mutate(
      "temp_result" = mapply(
        function(x, y, z) suppressWarnings(convert_unit(x, y, z, unit_format)),
        .data[[value]], .data[[unit]], .data$temp_unit
      )
    )

  # Check - any data fail to convert?
  chk <- dat$temp_result == -999999
  if (any(chk)) {
    bad_par <- dat[which(chk), ]
    bad_par <- paste(unique(bad_par[[group]]), collapse = ", ")
    msg <- paste("Unable to standardize units for", bad_par)

    if (!warn_only) {
      stop(msg)
    }
    warning(msg, call. = FALSE)
  }

  # Join updated, old data
  join_col <- c(group, value, unit)
  dat <- dplyr::left_join(.data, dat, by = join_col)

  # Update units, results
  dat <- dat %>%
    dplyr::mutate(
      {{ value }} := dplyr::if_else(
        .data$temp_result == -999999 | is.na(.data$temp_unit),
        .data[[value]],
        .data$temp_result
      )
    ) %>%
    dplyr::mutate(
      {{ unit }} := dplyr::if_else(
        .data$temp_result == -999999 | is.na(.data$temp_unit),
        .data[[unit]],
        .data$temp_unit
      )
    ) %>%
    dplyr::select(!dplyr::any_of(c("temp_unit", "temp_result")))

  # Check - any rows missing unit?
  chk <- is.na(dat[[unit]])

  if (all(!chk)) {
    return(dat)
  }

  # Patch in missing units
  dplyr::left_join(dat, dat_group, by = group) %>%
    dplyr::mutate(
      {{ unit }} := dplyr::if_else(
        is.na(.data[[unit]]),
        .data$temp_unit,
        .data[[unit]]
      )
    ) %>%
    dplyr::select(!"temp_unit")
}

#' #' Standardize units across a row
#' #'
#' #' @description `standardize_units_across()` updates result data so that
#' #' each row uses the same units for results and detection limits.
#' #'
#' #' @param .data Dataframe
#' #'
#' #' @returns Updated dataframe. Detection limit and detection limit unit will
#' #' be updated to match that row's result unit.
#' #'
#' #' @export
#' standardize_units_across <- function(.data) {
#'   # Check - all rows okay as is?
#'   chk <- is.na(.data$Detection_Limit_Unit) |
#'     .data$Detection_Limit_Unit == .data$Result_Unit
#'   if (all(chk)) {
#'     return(.data)
#'   }
#'
#'   # Split data according to whether it needs to be updated
#'   dat <- dplyr::mutate(.data, "temp_row" = dplyr::row_number())
#'
#'   dat1 <- dat[which(chk), ]
#'   dat2 <- dat[which(!chk), ] %>%
#'     dplyr::mutate(
#'       "temp_lower" = mapply(
#'         function(x, y, z) convert_unit(x, y, z),
#'         .data$Lower_Detection_Limit, .data$Detection_Limit_Unit,
#'         .data$Result_Unit
#'       )
#'     ) %>%
#'     dplyr::mutate(
#'       "temp_upper" = mapply(
#'         function(x, y, z) convert_unit(x, y, z),
#'         .data$Upper_Detection_Limit, .data$Detection_Limit_Unit,
#'         .data$Result_Unit
#'       )
#'     )
#'
#'   # Check - any rows fail to update data?
#'   chk <- (!is.na(dat2$temp_lower) & dat2$temp_lower == -999999) |
#'     (!is.na(dat2$temp_upper) & dat2$temp_upper == -999999)
#'   if (any(chk)) {
#'     bad_par <- dat2[which(chk), ]
#'     bad_par <- unique(bad_par$temp_row)
#'     stop(
#'       "Result and detection units are incompatible. Check rows: ",
#'       paste(bad_par, collapse = ", "),
#'       call. = FALSE
#'     )
#'   }
#'
#'   dat2 <- dat2 %>%
#'     dplyr::mutate(
#'       "Lower_Detection_Limit" = dplyr::if_else(
#'         is.na(.data$temp_lower),
#'         .data$Lower_Detection_Limit,
#'         .data$temp_lower
#'       )
#'     ) %>%
#'     dplyr::mutate(
#'       "Upper_Detection_Limit" = dplyr::if_else(
#'         is.na(.data$temp_upper),
#'         .data$Upper_Detection_Limit,
#'         .data$temp_upper
#'       )
#'     ) %>%
#'     dplyr::mutate(
#'       "Detection_Limit_Unit" = dplyr::if_else(
#'         is.na(.data$Result_Unit),
#'         .data$Detection_Limit_Unit,
#'         .data$Result_Unit
#'       )
#'     ) %>%
#'     dplyr::select(!c("temp_upper", "temp_lower"))
#'
#'   # Combine data
#'   rbind(dat1, dat2) %>%
#'     dplyr::arrange(.data$temp_row) %>%
#'     dplyr::select(!"temp_row")
#' }
