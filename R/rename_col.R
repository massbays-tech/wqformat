#' Rename columns
#'
#' @description Rename columns from `old_colnames` to `new_colnames`. If no
#'   match found, leaves old column name.
#'
#' @param df Dataframe.
#' @param old_colnames List. Old column names; must be same length and in
#'   same order as `new_colnames`.
#' @param new_colnames List. New column names; must be same length and in
#'   same order as `old_colnames`.
#'
#' @returns Updated dataframe.
rename_col <- function(df, old_colnames, new_colnames){

  # Check inputs
  chk <- is.na(c(old_colnames, new_colnames))
  if (all(chk)) {
    return(df)
  } else if (any(chk)) {
    stop("Can not include NA values in old_colnames or new_colnames")
  } else if (all(old_colnames == new_colnames)) {
    return(df)
  } else if (length(old_colnames) != length(new_colnames)) {
    stop("old_colnames and new_colnames are different lengths")
  }

  df_colnames <- data.frame(
      old_name = old_colnames,
      new_name = new_colnames
    ) %>%
    dplyr::filter(old_name %in% colnames(df))

  if (nrow(df_colnames) == 0) {
    return(df)
  }

  # Check if duplicate values in new_colnames
  chk <- duplicated(df_colnames$new_name)
  if (any(chk)) {
    # List duplicate new_name
    dup_val <- unique(df_colnames$new_name[which(chk)])

    # Iterate through each duplicate new_name
    for (val in dup_val) {
      df_temp <- dplyr::filter(df_colnames, new_name == val)
      target_col <- df_temp$old_name

      # Add new column, set to first non-NA value from old columns
      df <- concat_columns(df, target_col, val)

      # Drop redundant conversions from df_colnames
      df_colnames <- dplyr::filter(df_colnames, new_name != val)

      # Check if old columns needed for further conversions, else drop
      chk <- target_col %in% df_colnames$old_name
      if (any(!chk)) {
        drop_col <- target_col[which(!chk)]
        df <- dplyr::select(df, !dplyr::any_of(drop_col))
      }
    }
  }

  if (nrow(df_colnames) == 0) {
    return(df)
  }

  # Check if duplicate values in old_colnames
  chk <- duplicated(df_colnames$old_name)
  if (any(chk)) {
    # List duplicate old_name
    dup_val <- unique(df_colnames$old_name[which(chk)])
    df_temp <- dplyr::filter(df_colnames, old_name %in% dup_val)

    # Add new columns
    for (i in 1:nrow(df_temp)) {
      old_col <- df_temp$old_name[i]
      new_col <- df_temp$new_name[i]

      df <- dplyr::mutate(df, {{new_col}} := .data[[old_col]])
    }

    # Drop old columns, update df_colnames
    df <- dplyr::select(df, !dplyr::any_of(dup_val))
    df_colnames <- dplyr::filter(df_colnames, !old_name %in% dup_val)
  }

  if (nrow(df_colnames) == 0) {
    return(df)
  }

  # Rename columns
  field_subs <- df_colnames$new_name
  names(field_subs) <- df_colnames$old_name

  df <- dplyr::rename_with(df, ~ field_subs, names(field_subs))

  return(df)
}

#' Concatenate Columns
#'
#' @description Combines values from multiple columns and saves the first
#'  non-NA value to the output column.
#'
#' @param df Dataframe.
#' @param in_fields List. Dataframe columns to combine.
#' @param out_field String. Name of column to transfer values to.
#'
#' @returns Updated dataframe.
concat_columns <- function(df, in_fields, out_field) {
  if (!out_field %in% colnames(df)) {
    df[[out_field]] <- NA
  }

  in_fields <- intersect(in_fields, colnames(df))

  if (length(in_fields) == 0) {
    return(df)
  } else if (length(in_fields) == 1) {
    df <- dplyr::mutate(df, {{out_field}} := .data[[in_fields]])
    return(df)
  }

  df <- df %>%
    tidyr::unite(
      {{out_field}},
      dplyr::any_of(in_fields),
      sep="|",
      remove = FALSE
    ) %>%
    dplyr::mutate({{out_field}} := gsub("NA\\|", "", .data[[out_field]])) %>%
    dplyr::mutate(
      {{out_field}} := dplyr::case_when(
        grepl("|", .data[[out_field]], fixed=TRUE) ~
          stringr::str_split_i(.data[[out_field]], "\\|", 1),
        .data[[out_field]] == "NA" ~ NA,
        TRUE ~ .data[[out_field]]
      )
    )

  return(df)
}
