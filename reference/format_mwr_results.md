# Check and improve formatting for MassWateR result data

\`format_mwr_results()\` reviews MassWateR result data and checks for
common formatting errors. \* Checks for missing columns and places
columns in the correct order \* Updates "Result Value" and "Result
Measure Qualifier" for over-detects and under-detects \* Transfers
duplicate values to "QC Reference Value"

## Usage

``` r
format_mwr_results(.data, date_format = "m/d/Y")
```

## Arguments

- .data:

  Input dataframe

- date_format:

  String. Date format. Uses the same formatting as
  \[lubridate::parse_date_time()\]. Default value "m/d/Y".

## Value

Updated dataframe
