#' Format site metadata
#'
#' @description Converts water quality site metadata between different formats.
#'
#' @param .data Dataframe
#' @param in_format,out_format String. Desired input and output formats. Not
#' case sensitive. Accepted formats:
#' * WQX
#' * MassWateR
#' * wqdashboard
#' * RI_WW (Rhode Island Watershed Watch)
#' * MA_BRC (Blackstone River Coalition)
#' * ME_FOCB (Friends of Casco Bay)
#'
#' @inheritParams format_results
#'
#' @returns Updated dataframe.
#'
#' @export
format_sites <- function(.data, in_format, out_format, drop_extra_col = TRUE) {
  message("Reformatting data...")

  # Check inputs ----
  in_format <- tolower(in_format)
  out_format <- tolower(out_format)

  target_formats <- c(
    "wqx", "masswater", "wqdashboard", "ri_ww", "ma_brc", "me_focb"
  )
  chk <- c(in_format, out_format) %in% target_formats
  if (any(!chk)) {
    stop(
      "Invalid format. Acceptable options: ",
      paste(target_formats, collapse = ", ")
    )
  }

  # Fix common typos, entry errors
  dat <- prep_df(.data)

  # Check - repaired column names?
  chk <- grepl("\\.", colnames(dat))
  chk2 <- grepl(" ", colnames(dat))
  if (any(chk) && !any(chk2)) {
    dat <- dat |>
      unrepair_names(colnames_sites[[in_format]])
  }

  # Prep data with nonstandard formats ----
  if (in_format == "ma_brc") {
    dat <- prep_brc_sites(dat)
  }

  # Update columns ----
  var_names <- fetch_var(
    colnames_sites,
    in_format,
    out_format,
    limit_var = TRUE
  )
  dat <- rename_col(dat, var_names$old_names, var_names$new_names)

  # Add missing columns
  missing_col <- setdiff(var_names$keep_var, colnames(dat))
  if (length(missing_col) > 0) {
    dat[missing_col] <- NA
    message(
      "\tAdded ", toString(length(missing_col)), " new columns: ",
      paste(missing_col, collapse = ", ")
    )
  }

  # Sort columns, drop surplus if drop_extra_col is TRUE
  keep_col <- var_names$keep_var
  drop_col <- setdiff(colnames(dat), keep_col)
  if (length(drop_col) == 0) {
    dat <- dplyr::select(dat, dplyr::all_of(keep_col))
  } else if (drop_extra_col) {
    dat <- dplyr::select(dat, dplyr::all_of(keep_col))
    message("\tDropped ", toString(length(drop_col)), " columns")
  } else {
    dat <- dplyr::select(dat, dplyr::all_of(c(keep_col, drop_col)))
    message(
      "\tKept ", toString(length(drop_col)), " extra columns"
    )
  }

  # Update variables ----
  if (out_format == "wqx") {
    dat <- state_to_abb(dat, "State Code")
  } else if (out_format == "wqdashboard") {
    dat <- state_to_abb(dat, "State")
  }

  # Format data with nonstandard formats ----
  if (out_format == "ma_brc") {
    dat <- sites_to_brc(dat)
  } else if (out_format == "wqdashboard") {
    dat <- sites_to_wqdashboard(dat, drop_extra_col)
  }

  message("Done")
  dat
}
