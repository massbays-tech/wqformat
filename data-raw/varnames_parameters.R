library(readr)
library(dplyr)

varnames_parameters <- readr::read_csv("data-raw/varnames_parameters.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

usethis::use_data(varnames_parameters, overwrite = TRUE)
