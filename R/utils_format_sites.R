#' Sites from Massachusetts Blackstone River Coalition
#'
#' @description Helper function for `format_sites` that formats site data from
#'    the Blackstone River Coalition (MA_BRC) format.
#'    * Adds column "STATE" with each site's state
#'    * Adds column "WATER_DEPTH_M" and converts values in "WATER_DEPTH_FT" to
#'      meters
#'
#' @param df Input dataframe.
#'
#' @returns Updated dataframe.
sites_from_MA_BRC <- function(df) {

  ri_towns = c(
    "Burrillville", "Central Falls", "Cumberland", "Glocester",
    "Lincoln", "North Smithfield", "Pawtucket", "Scituate", "Woonsocket"
  )
  ma_towns = c(
    "Attleboro", "Auburn", "Bellingham", "Blackstone", "Boylston", "Douglas",
    "Franklin", "Grafton", "Holden", "Hopedale", "Leicester", "Mendon",
    "Milford", "Millbury", "Millville", "North Attleborough", "Northbridge",
    "Oxford", "Paxton", "Plainville", "Shrewsbury", "Smithfield", "Sutton",
    "Upton", "Uxbridge", "Webster", "West Boylston", "Westborough", "Worcester",
    "Wrentham"
  )

  missing_col <- setdiff(c("TOWN", "WATER_DEPTH_FT"), colnames(df))

  if(length(missing_col) == 2) {
    warning('Columns "TOWN", "WATER_DEPTH_FT" are missing')
    return(df)
  } else if (length(missing_col) == 1) {
    warning('Column "', missing_col, '" is missing')
  }

  if ("TOWN" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        "STATE" = dplyr::case_when(
          .data$TOWN %in% ri_towns ~ "RI",
          .data$TOWN %in% ma_towns ~ "MA",
          TRUE ~ NA
        )
      )
  }

  if ("WATER_DEPTH_FT" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate("WATER_DEPTH_M" = as.numeric(.data$WATER_DEPTH_FT)*0.3048)
  }

  return(df)
}

#' Sites to Massachusetts Blackstone River Coalition
#'
#' @description Helper function for `format_sites` that formats site data for
#'    the Blackstone River Coalition (MA_BRC).
#'    * Converts values in column "WATER_DEPTH_M" to feet and fills empty
#'      values in column "WATER_DEPTH_FT"
#'    * Removes columns "STATE", "WATER_DEPTH_M"
#'
#' @param df Input dataframe.
#'
#' @returns Updated dataframe.
sites_to_MA_BRC <- function(df) {

  if (!"WATER_DEPTH_FT" %in% colnames(df)) {
    df <- dplyr::mutate(df, "WATER_DEPTH_FT" = NA)
  }

  if ("WATER_DEPTH_M" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        "WATER_DEPTH_FT" = dplyr::if_else(
          is.na(as.numeric(.data$WATER_DEPTH_FT)),
          as.numeric(.data$WATER_DEPTH_M)/0.3048,
          .data$WATER_DEPTH_FT
        )
      )
  }

  df <- dplyr::select(df, !dplyr::any_of(c("STATE", "WATER_DEPTH_M")))

  return(df)
}
