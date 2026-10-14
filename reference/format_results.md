# Format result data

Converts water quality result data between different formats.

## Usage

``` r
format_results(
  .data,
  in_format,
  out_format,
  date_format = "m/d/Y",
  tz = Sys.timezone(),
  drop_extra_col = TRUE
)
```

## Arguments

- .data:

  Input dataframe.

- in_format, out_format:

  String. Desired input and output formats. Not case sensitive. Accepted
  formats:

  \* WQX

  \* MassWateR

  \* wqdashboard

  \* RI_WW (Rhode Island Watershed Watch)

  \* RI_DEM (Rhode Island DEM)

  \* MA_BRC (Blackstone River Coalition)

  \* ME_DEP (Maine DEP)

  \* ME_FOCB (Friends of Casco Bay)

- date_format:

  String. Date format. Uses the same formatting as
  \[lubridate::parse_date_time()\]. Default value "m/d/Y".

- tz:

  String. Specifies the timezone used to parse dates. Uses system
  timezone as default.

- drop_extra_col:

  Boolean. If \`TRUE\`, removes any columns that can't be converted to
  \`out_format\`. Default \`TRUE\`.

## Value

Updated dataframe
