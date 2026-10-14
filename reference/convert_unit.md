# Convert unit

\`convert_unit()\` calculates the value for \`x\` when converted to a
new unit.

## Usage

``` r
convert_unit(x, old_unit, new_unit, unit_format = "wqx")
```

## Arguments

- x:

  Numeric. Value to convert.

- old_unit:

  String. Current unit.

- new_unit:

  String. New unit.

- unit_format:

  String. Format used for units. Not case sensitive. Accepted values:
  WQX, MassWateR, wqdashboard, RI_WW, RI_DEM, MA_BRC, ME_DEP, ME_FOCB.
  Default wqx.

## Value

If \`old_unit\` and \`new_unit\` are compatible, returns updated value.
If they are incompatible, returns \`-999999\`.
