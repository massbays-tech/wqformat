tst <- list(
  # MassWateR site, result data
  mwr_sites = read.csv(
    system.file("extdata/example_data/masswater_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  mwr_data = read.csv(
    system.file("extdata/example_data/masswater_results.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # WQX site, result data
  wqx_sites = read.csv(
    system.file("extdata/example_data/wqx_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  wqx_data = read.csv(
    system.file("extdata/example_data/wqx_results.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # Blackstone River Coalition site, result data
  ma_brc_sites = read.csv(
    system.file("extdata/example_data/ma_brc_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  ma_brc_data = read.csv(
    system.file("extdata/example_data/ma_brc_results.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # Friends of Casco Bay site, result data
  me_focb_sites = read.csv(
    system.file("extdata/example_data/me_focb_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  me_focb_data1 = read.csv(
    system.file("extdata/example_data/me_focb_results_1.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  me_focb_data2 = read.csv(
    system.file("extdata/example_data/me_focb_results_2.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  me_focb_data3 = read.csv(
    system.file("extdata/example_data/me_focb_results_3.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # Maine DEP result data
  me_dep_data = read.csv(
    system.file("extdata/example_data/me_dep_results.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  )
)
