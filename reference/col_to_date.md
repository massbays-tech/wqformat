# Convert column to date or datetime format

\`col_to_date\` converts column to date or datetime format.

## Usage

``` r
col_to_date(
  .data,
  date_col,
  date_format = "m/d/Y",
  tz = Sys.timezone(),
  datetime = FALSE
)
```

## Arguments

- .data:

  Dataframe.

- date_col:

  String. Name of date column.

- date_format:

  String. Date format. Uses the same formatting as
  \[lubridate::parse_date_time()\]. Default value "m/d/Y".

- tz:

  String. Specifies the timezone used to parse dates. Uses system
  timezone as default.

- datetime:

  Boolean. If \`TRUE\`, returns column in datetime format. If \`FALSE\`,
  returns column in date format. Default \`FALSE\`.

## Value

Converts column to date or datetime and returns dataframe

## See also

\[col_to_numeric, col_to_time\]
