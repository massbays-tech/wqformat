#' Lookup table for result data column names
#'
#' Dataframe lookup table containing column names for various water
#' quality data formats. Each row represents an equivalent column name and each
#' column represents a different format. Additional formatting notes:
#' * Column order is specified by the format ##|| at the beginning of each
#' column name (eg 01||col1, 02||col2).
#' * If a data format is known to use alternate column names for the same data,
#' then the alternate names are listed after the preferred/most common name with
#' the delimeter | (eg colA|colB|colC).
#'
#' @format A dataframe with x rows and 8 columns:
#'  \describe{
#'    \item{WQX}{WQX}
#'    \item{MassWateR}{MassWateR}
#'    \item{WQdashboard}{WQdashboard}
#'    \item{RI_WW}{Rhode Island Watershed Watch}
#'    \item{RI_DEM}{Rhode Island DEM}
#'    \item{MA_BRC}{Blackstone River Coalition}
#'    \item{ME_DEP}{Maine DEP}
#'    \item{ME_FOCB}{Friends of Casco Bay}
#'  }
"colnames_results"

#' Lookup table for site data column names
#'
#' Dataframe lookup table containing column names for various water
#' quality site formats. Each row represents an equivalent column name and each
#' column represents a different format. Additional formatting notes:
#' * Column order is specified by the format ##|| at the beginning of each
#' column name (eg 01||col1, 02||col2).
#' * If a data format is known to use alternate column names for the same data,
#' then the alternate names are listed after the preferred/most common name with
#' the delimeter | (eg colA|colB|colC).
#'
#' @format A dataframe with x rows and 6 columns:
#'  \describe{
#'    \item{WQX}{WQX}
#'    \item{MassWateR}{MassWateR}
#'    \item{WQdashboard}{WQdashboard}
#'    \item{RI_WW}{Rhode Island Watershed Watch}
#'    \item{MA_BRC}{Blackstone River Coalition}
#'    \item{ME_FOCB}{Friends of Casco Bay}
#'  }
"colnames_sites"

#' Lookup table for activity type names
#'
#' Dataframe lookup table containing variable names for activity type in various
#' water quality formats. Each row represents an equivalent variable name and
#' each column represents a different format. Additional formatting notes:
#' * If a data format is known to allow multiple variable names for the same
#' data, then the alternate names are listed after the preferred/most common
#' name with the delimeter | (eg varA|varB|varC).
#'
#' @format A dataframe with x rows and 6 columns:
#'  \describe{
#'    \item{WQX}{WQX}
#'    \item{MassWateR}{MassWateR}
#'    \item{RI_DEM}{Rhode Island DEM}
#'    \item{MA_BRC}{Blackstone River Coalition}
#'    \item{ME_DEP}{Maine DEP}
#'    \item{ME_FOCB}{Friends of Casco Bay}
#'  }
"varnames_activity"

#' Lookup table for parameter names
#'
#' Dataframe lookup table containing variable names for parameter type in
#' various water quality formats. Each row represents an equivalent variable
#' name and each column represents a different format. Additional formatting
#' notes:
#' * If a data format is known to allow multiple variable names for the same
#' data, then the alternate names are listed after the preferred/most common
#' name with the delimeter | (eg varA|varB|varC).
#'
#' @format A dataframe with x rows and 6 columns:
#'  \describe{
#'    \item{WQX}{WQX}
#'    \item{MassWateR}{MassWateR}
#'    \item{RI_DEM}{Rhode Island DEM}
#'    \item{MA_BRC}{Blackstone River Coalition}
#'    \item{ME_DEP}{Maine DEP}
#'    \item{ME_FOCB}{Friends of Casco Bay}
#'  }
"varnames_parameters"

#' Lookup table for qualifier names
#'
#' Dataframe lookup table containing variable names for qualifiers in
#' various water quality formats. Each row represents an equivalent variable
#' name and each column represents a different format. Additional formatting
#' notes:
#' * If a data format is known to allow multiple variable names for the same
#' data, then the alternate names are listed after the preferred/most common
#' name with the delimeter | (eg varA|varB|varC).
#'
#' @format A dataframe with x rows and 6 columns:
#'  \describe{
#'    \item{WQX}{WQX}
#'    \item{MassWateR}{MassWateR}
#'    \item{RI_DEM}{Rhode Island DEM}
#'    \item{MA_BRC}{Blackstone River Coalition}
#'    \item{ME_DEP}{Maine DEP}
#'    \item{ME_FOCB}{Friends of Casco Bay}
#'    \item{flag}{Type of qualifier. Options: Pass, Suspect, Over-Detect,
#'    Non-Detect, Not Reviewed}
#'  }
"varnames_qualifiers"

#' Lookup table for unit names
#'
#' Dataframe lookup table containing variable names for units in various water
#' quality formats. Each row represents an equivalent variable name and each
#' column represents a different format. Additional formatting notes:
#' * If a data format is known to allow multiple variable names for the same
#' data, then the alternate names are listed after the preferred/most common
#' name with the delimeter | (eg varA|varB|varC).
#'
#' @format A dataframe with x rows and 6 columns:
#'  \describe{
#'    \item{WQX}{WQX}
#'    \item{MassWateR}{MassWateR}
#'    \item{RI_DEM}{Rhode Island DEM}
#'    \item{MA_BRC}{Blackstone River Coalition}
#'    \item{ME_DEP}{Maine DEP}
#'    \item{ME_FOCB}{Friends of Casco Bay}
#'  }
"varnames_units"
