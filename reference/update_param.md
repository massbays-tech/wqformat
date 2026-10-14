# Rename parameters

\`update_param()\` converts parameters in target column to new format.

## Usage

``` r
update_param(.data, col_name, in_format, out_format)
```

## Arguments

- .data:

  Dataframe

- col_name:

  String. Column name.

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

## Value

Dataframe with updated parameters in target column

## See also

\[update_var\], \[update_unit\]
