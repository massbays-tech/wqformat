library(readr)
library(dplyr)

colnames_results <- readr::read_csv(
  "inst/extdata/colnames_results.csv",
  show_col_types = FALSE
) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) # drop empty columns

usethis::use_data(colnames_results, overwrite = TRUE)
