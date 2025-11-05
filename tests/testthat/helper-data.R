tst <- list(
  # MassWateR site, result data
  mwr_sites = read.csv(
    system.file("extdata/example_masswater_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  mwr_data = read.csv(
    system.file("extdata/example_masswater_data.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # WQX site, result data
  wqx_sites = read.csv(
    system.file("extdata/example_wqx_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  wqx_data = read.csv(
    system.file("extdata/example_wqx_data.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # Blackstone River Coalition site, result data
  ma_brc_sites = read.csv(
    system.file("extdata/example_ma_brc_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  ma_brc_data = read.csv(
    system.file("extdata/example_ma_brc_data.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # Friends of Casco Bay site, result data
  me_focb_sites = read.csv(
    system.file("extdata/example_me_focb_sites.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  me_focb_data1 = read.csv(
    system.file("extdata/example_me_focb_data_1.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  me_focb_data2 = read.csv(
    system.file("extdata/example_me_focb_data_2.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  me_focb_data3 = read.csv(
    system.file("extdata/example_me_focb_data_3.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  ),
  # Maine DEP result data
  me_dep_data = read.csv(
    system.file("extdata/example_me_dep_data.csv", package = "wqformat"),
    na.strings = c("NA", "NaN", "", " ")
  )
)
