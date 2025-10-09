tst <- list(
  # MassWateR site, result data
  mwr_sites = read.csv(
    system.file('extdata/example_masswater_sites.csv', package = 'wqformat'),
    na.strings=c("NA","NaN", "", " ")
  ),
  mwr_data = read.csv(
    system.file('extdata/example_masswater_data.csv', package = 'wqformat'),
    na.strings=c("NA","NaN", "", " ")
  ),
  # WQX site, result data
  wqx_sites = read.csv(
    system.file('extdata/example_wqx_sites.csv', package = 'wqformat'),
    na.strings=c("NA","NaN", "", " ")
  ),
  wqx_data = read.csv(
    system.file('extdata/example_wqx_data.csv', package = 'wqformat'),
    na.strings=c("NA","NaN", "", " ")
  )
)

