# Convert column to numeric format

If all values in column are numbers, \`col_to_numeric()\` converts
column type to numeric.

## Usage

``` r
col_to_numeric(.data, col_name, silent = TRUE)
```

## Arguments

- .data:

  Dataframe.

- col_name:

  String. Column name.

- silent:

  Boolean. If TRUE, does not provide warning if unable to set column to
  numeric. If FALSE, provides error message. Default TRUE.

## Value

If all values are numeric, converts column to numeric and returns
dataframe. If there are any non-numeric values, leaves column as-is and
returns dataframe.

## See also

\[col_to_date, col_to_time\]
