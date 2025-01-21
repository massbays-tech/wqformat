#' Prep MassWateR
#'
#' @description Converts MassWateR data to standard format. Helper function for
#'   `format_results`.
#'  * Transfers "BDL" and "AQL" values from `Result Value` to
#'    `Result Measure Qualifier`.
#'  * Data in `QC Reference Value` is copied to its own row.
#'
#' @param dat Dataframe.
#'
#' @returns Updated dataframe.
prep_MassWateR <- function(dat){
  # Prep data
  dat$`Result Value` <- as.character(dat$`Result Value`)
  dat$`QC Reference Value` <- as.character(dat$`QC Reference Value`)

  # Transfer QC Reference Value to own row
  qc_duplicate <- c('Quality Control Sample-Lab Duplicate',
                    'Quality Control-Meter Lab Duplicate')

  dat <- dat %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        `Activity Type` %in% qc_duplicate & !is.na(`QC Reference Value`),
        paste0('NA|', `QC Reference Value`),
        `QC Reference Value`
      )
    ) %>%
    tidyr::separate_longer_delim(`QC Reference Value`, '|') %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        `QC Reference Value` == 'NA',
        NA,
        `QC Reference Value`
      )
    ) %>%
    dplyr::mutate(
      `Result Value` = dplyr::if_else(
        `Activity Type` %in% qc_duplicate & !is.na(`QC Reference Value`),
        `QC Reference Value`,
        `Result Value`
      )
    ) %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        `Activity Type` %in% qc_duplicate,
        NA,
        `QC Reference Value`
      )
    )

  # Update qualifiers, result value
  dat <- dat %>%
    dplyr::mutate(
      `Result Measure Qualifier` = dplyr::if_else(
        `Result Value` %in% c('BDL', 'AQL'),
        `Result Value`,
        `Result Measure Qualifier`
      )
    ) %>%
    dplyr::mutate(
      `Result Value` = dplyr::if_else(
        `Result Value` %in% c('BDL', 'AQL'),
        NA,
        `Result Value`
      )
    )

  # Reset columns to numeric
  dat <- col_to_numeric(dat, 'Result Value')
  dat <- col_to_numeric(dat, 'QC Reference Value')

  return(dat)
}

#' To MassWateR
#'
#' @description Helper function for `format_results` that formats data for
#'  `MassWateR`.
#'  * Sets `Result Value` to "BDL" or "AQL" as needed
#'  * Sets `Result Measure Qualifier` to Q or NA
#'  * Transfers duplicate values to `QC Reference Value`
#'
#' @param dat Dataframe.
#'
#' @returns Updated dataframe.
to_MassWateR <- function(dat, in_format){
  # Prep data
  dat$`Result Value` <- as.character(dat$`Result Value`)
  dat$`QC Reference Value` <- as.character(dat$`QC Reference Value`)

  dat_colnames <- colnames(dat)

  # Update qualifiers, result value
  qual <- find_var_names(varnames_qualifiers, in_format, 'Flag')
  dat <- rename_all_var(dat, 'Result Measure Qualifier', qual$old_names, qual$new_names)

  dat <- dat %>%
    dplyr::mutate(
      `Result Value` = dplyr::case_when(
        `Result Measure Qualifier` == 'Non-Detect' ~ 'BDL',
        `Result Measure Qualifier` == 'Over-Detect' ~ 'AQL',
        TRUE ~ `Result Value`
      )
    ) %>%
    dplyr::mutate(
      `Result Measure Qualifier` = dplyr::if_else(
        `Result Measure Qualifier` %in% c('Not Reviewed', 'Suspect'),
        'Q',
        NA
      )
    )

  # Transfer QC duplicates to QC Reference Value
  qc_duplicate <- c('Quality Control Sample-Lab Duplicate',
                    'Quality Control-Meter Lab Duplicate')
  group_col <- setdiff(dat_colnames, 'Result Value')

  chk <- dat[['Activity Type']] %in% qc_duplicate &
    is.na(dat[['QC Reference Value']])
  dat1 <- dat[which(chk),]  # data to group
  dat2 <- dat[which(!chk),]  # data to leave as-is

  dat1 <- dat1 %>%
    dplyr::group_by_at(group_col) %>%
    dplyr::summarize(
      `Result Value` = stringr::str_c(`Result Value`, collapse = '|'),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      `QC Reference Value` = dplyr::if_else(
        stringr::str_count(`Result Value`, '\\|') == 1,
        stringr::str_split_i(`Result Value`, '\\|', 2),
        `QC Reference Value`
      )
    ) %>%
    dplyr::mutate(
      `Result Value` = dplyr::if_else(
        stringr::str_count(`Result Value`, '\\|') == 1,
        stringr::str_split_i(`Result Value`, '\\|', 1),
        `Result Value`
      )
    ) %>%
    tidyr::separate_longer_delim(`Result Value`, '|')

  dat <- rbind(dat1, dat2)
  dat <- as.data.frame(dat) %>%
    dplyr::arrange(`Activity Start Date`, `Activity Start Time`) %>%  # Sort data by Date, Time
    dplyr::select(dplyr::all_of(dat_colnames))  # Reorder columns, else 'Result Value' sent to end
  dat <- col_to_numeric(dat, 'Result Value')
  dat <- col_to_numeric(dat, 'QC Reference Value')

  return(dat)
}
