#' To MassWateR
#'
#' @description Preps data for MassWateR. Helper function for `format_results`.
#'  * If Qualifier indicates data was below the detection limit, sets
#'    `Qualifier` to NA and `Result Value` to "BDL".
#'  * All other qualifiers are set to "Q".
#'  * Transfers replicate and duplicate values to `QC Reference Value`.
#'
#' @param df Dataframe.
#'
#' @returns Updated dataframe.
results_to_MassWateR <- function(df){



  return(df)
}

#' From MassWateR
#'
#' @description Converts MassWateR data to standard format. Helper function for
#'  `format_results`.
#'  * If `Result Value` is "BDL", sets `Qualifier` to "BDL" and `Result Value`
#'    to NA.
#'  * Data in `QC Reference Value` is transferred to its own row.
#'
#' @param df Dataframe.
#'
#' @returns Updated dataframe.
results_from_MassWateR <- function(df){


  return(df)
}
