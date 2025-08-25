library(readr)
library(dplyr)

varnames_qualifiers <- readr::read_csv(
  "inst/extdata/varnames_qualifiers.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) %>% # drop empty columns
  dplyr::select(!dplyr::any_of(c("Flag", "Description")))

usethis::use_data(varnames_qualifiers, overwrite = TRUE)
