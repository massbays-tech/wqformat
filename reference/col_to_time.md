# Convert datetime column to time

\`col_to_time\` converts a datetime column to a string column containing
the time.

## Usage

``` r
col_to_time(.data, time_col)
```

## Arguments

- .data:

  Dataframe.

- time_col:

  String. Name of time column.

## Value

Converts column to HH:MM string and returns dataframe

## See also

\[col_to_numeric, col_to_date\]
