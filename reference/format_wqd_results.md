# Check and improve formatting for wqdashboard result data

\`format_wqd_results()\` reviews wqdashboard result data and checks for
common formatting errors. \* Checks for missing columns \* Checks for
unknown parameters, units, qualifiers, and activity type \* Formats Date
column as date

## Usage

``` r
format_wqd_results(.data, date_format, categorical = FALSE)
```

## Arguments

- .data:

  Input dataframe

- date_format:

  String. Date format. Uses the same formatting as
  \[lubridate::parse_date_time()\]. Default value "m/d/Y".

- categorical:

  Boolean. Set to \`TRUE\` for categorical data that may be missing
  column "Result_Unit." Default \`FALSE\`.

## Value

Updated dataframe
