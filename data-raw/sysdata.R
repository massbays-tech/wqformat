library(readr)
library(dplyr)

# Import column lookup tables ----
colnames_sites <- readr::read_csv(
  "inst/extdata/colnames_sites.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x))))

colnames_results <- readr::read_csv(
  "inst/extdata/colnames_results.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x))))

# Import variable lookup tables ----
varnames_activity <- readr::read_csv(
  "inst/extdata/varnames_activity.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) %>%
  dplyr::mutate("wqdashboard" = .data$wqx) %>%
  dplyr::mutate("ri_ww" = .data$ri_dem)

varnames_parameters <- readr::read_csv(
  "inst/extdata/varnames_parameters.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) %>%
  dplyr::mutate("ri_ww" = .data$ri_dem) %>%
  dplyr::mutate(
    "wqdashboard" = dplyr::if_else(
      is.na(.data$wqdashboard),
      .data$wqx,
      .data$wqdashboard
    )
  )

varnames_qualifiers <- readr::read_csv(
  "inst/extdata/varnames_qualifiers.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) %>%
  dplyr::select(!dplyr::any_of(c("Flag", "Description"))) %>%
  dplyr::mutate("masswater" = .data$wqx) %>%
  dplyr::mutate("wqdashboard" = .data$wqx) %>%
  dplyr::mutate("ri_ww" = .data$ri_dem)

varnames_units <- readr::read_csv(
  "inst/extdata/varnames_units.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) %>%
  dplyr::mutate("wqdashboard" = .data$wqx) %>%
  dplyr::mutate("ri_ww" = .data$ri_dem)

# Save data ----
usethis::use_data(
  colnames_results, colnames_sites, varnames_activity, varnames_parameters,
  varnames_qualifiers, varnames_units,
  internal = TRUE,
  overwrite = TRUE
)
