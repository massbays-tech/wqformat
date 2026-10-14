# Format site metadata

Converts water quality site metadata between different formats.

## Usage

``` r
format_sites(.data, in_format, out_format, drop_extra_col = TRUE)
```

## Arguments

- .data:

  Dataframe

- in_format, out_format:

  String. Desired input and output formats. Not case sensitive. Accepted
  formats: \* WQX \* MassWateR \* wqdashboard \* RI_WW (Rhode Island
  Watershed Watch) \* MA_BRC (Blackstone River Coalition) \* ME_FOCB
  (Friends of Casco Bay)

- drop_extra_col:

  Boolean. If \`TRUE\`, removes any columns that can't be converted to
  \`out_format\`. Default \`TRUE\`.

## Value

Updated dataframe.
