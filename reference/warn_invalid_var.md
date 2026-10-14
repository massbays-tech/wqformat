# Check column for invalid variables

\`warn_invalid_var()\` checks if the input column contains any variables
that aren't included in \`varlist\`.

## Usage

``` r
warn_invalid_var(dat, col_name, varlist)
```

## Arguments

- dat:

  Dataframe

- col_name:

  String. Column name.

- varlist:

  List. List of acceptable variable names.

## Value

Updated dataframe.

## See also

\[update_var\]
