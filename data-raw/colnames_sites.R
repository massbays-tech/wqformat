library(readr)
library(dplyr)

colnames_sites <- readr::read_csv("data-raw/colnames_sites.csv",
                                  show_col_types = FALSE) %>%
  dplyr::select_if(function(x) !(all(is.na(x))))  # drop empty columns

usethis::use_data(colnames_sites, overwrite = TRUE)
