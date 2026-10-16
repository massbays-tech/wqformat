# Rename all variables in column

\`update_var()\` uses a paired list (\`old_varname\`, \`new_varname\`)
to generate a list of name substitutions and update all variables in the
input column. If no match found, leaves old variable name.

## Usage

``` r
update_var(.data, col_name, old_varname, new_varname, case_sensitive = FALSE)
```

## Arguments

- .data:

  Dataframe

- col_name:

  String. Column name.

- old_varname, new_varname:

  List. List old variable names in \`old_varname\` and new variable
  names in \`new_varname\`. \`old_varname\` and \`new_varname\` are a
  paired list, therefor they must be the same length and in the same
  order.

- case_sensitive:

  Boolean. If \`TRUE\`, \`old_varname\` is case sensitive. Default
  \`FALSE\`.

## Value

Dataframe with updated variables in target column. If no substitutions
found, returns original dataframe.

## See also

\[update_param\], \[update_unit\], \[warn_invalid_var\]
