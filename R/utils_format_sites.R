#' Preformat site data from the Blackstone River Coalition
#'
#' @description
#' `prep_MA_BRC_sites()` is a helper function for [format_sites()] that
#' pre-formats site data from the Blackstone River Coalition (MA_BRC).
#' * Adds column "STATE" with each site's state
#' * Adds column "WATER_DEPTH_M", fills with values from "WATER_DEPTH_FT"
#' that have been converted to meters
#' * Converts values in "CFR" from Yes, No to Coldwater, Warmwater
#'
#' @param .data Dataframe
#'
#' @returns
#' Dataframe with adjusted columns and values so the data can be run through
#' [format_site()]
#'
#' @noRd
prep_MA_BRC_sites <- function(.data) {
  if ("TOWN" %in% colnames(.data)) {
    .data <- .data %>%
      dplyr::mutate(
        "STATE" = dplyr::case_when(
          .data$TOWN %in% c(
            "Burrillville", "Central Falls", "Cumberland", "Glocester",
            "Lincoln", "North Smithfield", "Pawtucket", "Scituate", "Woonsocket"
          ) ~ "RI",
          .data$TOWN %in% c(
            "Attleboro", "Auburn", "Bellingham", "Blackstone", "Boylston",
            "Douglas", "Franklin", "Grafton", "Holden", "Hopedale", "Leicester",
            "Mendon", "Milford", "Millbury", "Millville", "North Attleborough",
            "Northbridge", "Oxford", "Paxton", "Plainville", "Shrewsbury",
            "Smithfield", "Sutton", "Upton", "Uxbridge", "Webster",
            "West Boylston", "Westborough", "Worcester", "Wrentham"
          ) ~ "MA",
          TRUE ~ NA
        )
      )
  }

  if ("WATER_DEPTH_FT" %in% colnames(.data)) {
    .data <- .data %>%
      dplyr::mutate("WATER_DEPTH_M" = as.numeric(.data$WATER_DEPTH_FT) * 0.3048)
  }

  if ("CFR" %in% colnames(.data)) {
    .data <- .data %>%
      dplyr::mutate(
        "CFR" = dplyr::case_when(
          tolower(.data$CFR) == "no" ~ "Warmwater",
          tolower(.data$CFR) == "yes" ~ "Coldwater",
          TRUE ~ .data$CFR
        )
      )
  }

  return(.data)
}

#' Format site data for the Blackstone River Coalition
#'
#' @description
#' `sites_to_MA_BRC()` is a helper function for [format_sites()] that formats
#' site data for the Blackstone River Coalition (MA_BRC).
#' * Fills empty values in column "WATER_DEPTH_FT" by converting "WATER_DEPTH_M"
#' to feet
#' * Converts values in "CFR" from Coldwater, Warmwater to Yes, No
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

  if ("CFR" %in% colnames(.data)) {
    .data <- .data %>%
      dplyr::mutate(
        "CFR" = dplyr::case_when(
          tolower(.data$CFR) == "warmwater" ~ "No",
          tolower(.data$CFR) == "coldwater" ~ "Yes",
          TRUE ~ .data$CFR
        )
      )
  }

  dat <- dplyr::select(.data, !dplyr::any_of(c("STATE", "WATER_DEPTH_M")))

  return(dat)
}
