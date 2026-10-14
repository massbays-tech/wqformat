# Standardize units

\`standardize_units()\` standardizes the unit used for each parameter in
\`group\`.

## Usage

``` r
standardize_units(
  .data,
  group,
  value,
  unit,
  unit_format = "wqx",
  warn_only = TRUE
)
```

## Arguments

- .data:

  Dataframe

- group:

  String. Column used to group data.

- value:

  String. Column with result values.

- unit:

  String. Column with units.

- unit_format:

  String. Format used for units. Not case sensitive. Accepted values:
  WQX, MassWateR, wqdashboard, RI_WW, RI_DEM, MA_BRC, ME_DEP, ME_FOCB.
  Default wqx.

- warn_only:

  Boolean. If \`TRUE\`, returns a warning message if unable to convert
  units. If \`FALSE\`, returns an error message.

## Value

Updated dataframe with updated unit and result values. Rows that can not
be converted to a standard unit are left as-is and in informative
warning or error message is provided.
