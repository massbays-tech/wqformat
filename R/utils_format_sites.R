#' Preformat site data from the Blackstone River Coalition
#'
#' @description
#' `prep_MA_BRC_sites()` is a helper function for [format_sites()] that
#' pre-formats site data from the Blackstone River Coalition (MA_BRC).
#' * Adds column "STATE" with each site's state
#' * Adds column "WATER_DEPTH_M", fills with values from "WATER_DEPTH_FT"
#' that have been converted to meters
#'
#' @param .data Dataframe
#'
#' @returns
#' Dataframe with adjusted columns and values so the data can be run through
#' [format_site()]
#'
#' @noRd
prep_MA_BRC_sites <- function(.data) {
  missing_col <- setdiff(c("TOWN", "WATER_DEPTH_FT"), colnames(.data))

  if (length(missing_col) == 2) {
    warning('Columns "TOWN", "WATER_DEPTH_FT" are missing')
    return(.data)
  } else if (length(missing_col) == 1) {
    .data[[missing_col]] <- NA
    warning('Column "', missing_col, '" is missing')
  }

  ri_towns <- c(
    "Burrillville", "Central Falls", "Cumberland", "Glocester",
    "Lincoln", "North Smithfield", "Pawtucket", "Scituate", "Woonsocket"
  )
  ma_towns <- c(
    "Attleboro", "Auburn", "Bellingham", "Blackstone", "Boylston", "Douglas",
    "Franklin", "Grafton", "Holden", "Hopedale", "Leicester", "Mendon",
    "Milford", "Millbury", "Millville", "North Attleborough", "Northbridge",
    "Oxford", "Paxton", "Plainville", "Shrewsbury", "Smithfield", "Sutton",
    "Upton", "Uxbridge", "Webster", "West Boylston", "Westborough", "Worcester",
    "Wrentham"
  )

  dat <- .data %>%
    dplyr::mutate(
      "STATE" = dplyr::case_when(
        .data$TOWN %in% ri_towns ~ "RI",
        .data$TOWN %in% ma_towns ~ "MA",
        TRUE ~ NA
      )
    ) %>%
    dplyr::mutate("WATER_DEPTH_M" = as.numeric(.data$WATER_DEPTH_FT) * 0.3048)

  return(dat)
}

#' Format site data for the Blackstone River Coalition
#'
#' @description
#' `sites_to_MA_BRC()` is a helper function for [format_sites()] that formats
#' site data for the Blackstone River Coalition (MA_BRC).
#' * Fills empty values in column "WATER_DEPTH_FT" by converting "WATER_DEPTH_M"
#' to feet
#' * Removes columns "STATE", "WATER_DEPTH_M"
#'
#' @param .data Dataframe
#'
#' @returns
#' Dataframe with adjusted columns and values so it matches the standard format
#' used by the Blackstone River Coalition.
#'
#' @noRd
sites_to_MA_BRC <- function(.data) {
  if (!"WATER_DEPTH_FT" %in% colnames(.data)) {
    .data$WATER_DEPTH_FT <- NA
  }

  if ("WATER_DEPTH_M" %in% colnames(.data)) {
    .data <- .data %>%
      dplyr::mutate(
        "WATER_DEPTH_FT" = dplyr::if_else(
          is.na(as.numeric(.data$WATER_DEPTH_FT)),
          as.numeric(.data$WATER_DEPTH_M) / 0.3048,
          .data$WATER_DEPTH_FT
        )
      )
  }

  dat <- dplyr::select(.data, !dplyr::any_of(c("STATE", "WATER_DEPTH_M")))

  return(dat)
}
