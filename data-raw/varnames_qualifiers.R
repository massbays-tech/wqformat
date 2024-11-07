library(readr)
library(dplyr)

df <- readr::read_csv("data-raw/varnames_qualifiers.csv",
                      show_col_types = FALSE) %>%
  dplyr::select_if(function(x) !(all(is.na(x))))  # drop empty columns

# Drop rows with only 1 unique value
df$temp_count <- apply(df, 1, function(x) length(unique(na.omit(x))))
varnames_qualifiers <- df %>%
  dplyr::filter(temp_count > 1) %>%
  dplyr::select(!temp_count)

usethis::use_data(varnames_qualifiers, overwrite = TRUE)
