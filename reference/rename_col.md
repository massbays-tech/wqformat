# Rename columns

\`rename_col()\` renames dataframe columns by using a paired list to
replace \`old_colnames\` with \`new_colnames\`. If no match is found for
an existing column name, it is left as-is.

## Usage

``` r
rename_col(.data, old_colnames, new_colnames)
```

## Arguments

- .data:

  Dataframe

- old_colnames, new_colnames:

  List. Columns listed in \`old_colnames\` will be renamed to match
  paired name in \`new_colnames\`. \`old_colnames\` and \`new_colnames\`
  must be the same length and in the same order.

## Value

Dataframe with updated column names.

## See also

\[concat_col\]
