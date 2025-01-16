test_that("prep_MassWateR works", {
  dat <- data.frame(
    'Activity Type' = c(
      'Field Msr/Obs',
      'Quality Control-Meter Lab Duplicate',
      'Quality Control Sample-Lab Duplicate',
      'Quality Control Sample-Lab Duplicate'
      ),
    'Activity Start Date' = c(as.Date('2024-05-01'), as.Date('2024-05-02'),
      as.Date('2024-05-02'), as.Date('2024-05-04')),
    'Activity Start Time' = c('9:00', '8:00', '9:00', '10:00'),
    'Characteristic Name' = c('TDN', 'TDP', 'TDN', 'TDP'),
    'Result Value' = c('12', 'BDL', 'AQL', '15'),
    'Result Measure Qualifier' = c(NA, NA, NA, 'Q'),
    'QC Reference Value' = c(11, 6, NA, 16),
    check.names = FALSE
  )

  dat_out <- data.frame(
    'Activity Type' = c(
      'Field Msr/Obs',
      'Quality Control-Meter Lab Duplicate',
      'Quality Control-Meter Lab Duplicate',
      'Quality Control Sample-Lab Duplicate',
      'Quality Control Sample-Lab Duplicate',
      'Quality Control Sample-Lab Duplicate'
      ),
    'Activity Start Date' = c(as.Date('2024-05-01'), as.Date('2024-05-02'),
      as.Date('2024-05-02'), as.Date('2024-05-02'), as.Date('2024-05-04'),
      as.Date('2024-05-04')),
    'Activity Start Time' = c('9:00', '8:00', '8:00', '9:00', '10:00', '10:00'),
    'Characteristic Name' = c('TDN', 'TDP', 'TDP', 'TDN', 'TDP', 'TDP'),
    'Result Value' = c(12, NA, 6, NA, 15, 16),
    'Result Measure Qualifier' = c(NA, 'BDL', NA, 'AQL', 'Q', 'Q'),
    'QC Reference Value' = c(11, NA, NA, NA, NA, NA),
    check.names = FALSE
  )

  expect_equal(prep_MassWateR(dat), dat_out)
})

test_that("to_MassWateR works", {
  dat <- data.frame(
    'Activity Type' = c(
      'Field Msr/Obs',
      'Quality Control-Meter Lab Duplicate',
      'Quality Control-Meter Lab Duplicate',
      'Quality Control Sample-Lab Duplicate',
      'Quality Control Sample-Lab Duplicate',
      'Quality Control Sample-Lab Duplicate'
    ),
    'Activity Start Date' = c(as.Date('2024-05-01'), as.Date('2024-05-02'),
      as.Date('2024-05-02'), as.Date('2024-05-02'), as.Date('2024-05-04'),
      as.Date('2024-05-04')),
    'Activity Start Time' = c('9:00', '8:00', '8:00', '9:00', '10:00', '10:00'),
    'Characteristic Name' = c('TDN', 'TDP', 'TDP', 'TDN', 'TDP', 'TDP'),
    'Result Value' = c(12, NA, 6, NA, 15, 16),
    'Result Measure Qualifier' = c(NA, 'DL', NA, 'GT', 'Q', 'Q'),
    'QC Reference Value' = c(11, NA, NA, NA, NA, NA),
    check.names = FALSE
  )

  dat_out <- data.frame(
    'Activity Type' = c(
      'Field Msr/Obs',
      'Quality Control-Meter Lab Duplicate',
      'Quality Control Sample-Lab Duplicate',
      'Quality Control Sample-Lab Duplicate'
    ),
    'Activity Start Date' = c(as.Date('2024-05-01'), as.Date('2024-05-02'),
      as.Date('2024-05-02'), as.Date('2024-05-04')),
    'Activity Start Time' = c('9:00', '8:00', '9:00', '10:00'),
    'Characteristic Name' = c('TDN', 'TDP', 'TDN', 'TDP'),
    'Result Value' = c('12', 'BDL', 'AQL', '15'),
    'Result Measure Qualifier' = c(NA, NA, NA, 'Q'),
    'QC Reference Value' = c(11, 6, NA, 16),
    check.names = FALSE
  )

  expect_equal(to_MassWateR(dat, 'WQX'), dat_out)
})
