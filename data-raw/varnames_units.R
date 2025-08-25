library(readr)
library(dplyr)

varnames_units <- readr::read_csv(
  "inst/extdata/varnames_units.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

usethis::use_data(varnames_units, overwrite = TRUE)
