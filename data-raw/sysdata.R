library(readr)
library(dplyr)

# Import column lookup tables ----
colnames_sites <- readr::read_csv(
  "inst/extdata/colnames_sites.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

colnames_results <- readr::read_csv(
  "inst/extdata/colnames_results.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

# Import variable lookup tables ----
varnames_activity <- readr::read_csv(
  "inst/extdata/varnames_activity.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

varnames_parameters <- readr::read_csv(
  "inst/extdata/varnames_parameters.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

varnames_qualifiers <- readr::read_csv(
  "inst/extdata/varnames_qualifiers.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) %>% # drop empty columns
  dplyr::select(!dplyr::any_of(c("Flag", "Description")))

varnames_units <- readr::read_csv(
  "inst/extdata/varnames_units.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

# Save data ----
usethis::use_data(
  colnames_results, colnames_sites, varnames_activity, varnames_parameters,
  varnames_qualifiers, varnames_units,
  internal = TRUE
)
