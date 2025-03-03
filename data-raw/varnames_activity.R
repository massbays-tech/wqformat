library(readr)
library(dplyr)

varnames_activity <- readr::read_csv("data-raw/varnames_activity.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

usethis::use_data(varnames_activity, overwrite = TRUE)
