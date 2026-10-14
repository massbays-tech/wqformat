# wqformat

## Overview

The wqformat R package converts water quality data to several different
formats. It is primarily designed to format water quality data for
[MassWateR](https://massbays-tech.github.io/MassWateR/) and
[WQdashboard](https://nbep.github.io/WQdashboard/index.html), but it can
convert data to [other formats](#standard) as well.

The package is created in collaboration with the [Massachusetts Bays
National Estuary Partnership](https://massbays.org/) with funding from
an EPA Exchange Network Grant (Grant No. ).

## Package Installation

Install the package as follows:

``` r

library(wqformat)
```

## Data Inputs

wqformat accepts two types of data: **water quality results** and **site
metadata**. Before using wqformat, please double check that all columns
and variables are spelled correctly; wqformat provides informative error
messages, but will not work correctly if things are misspelled.

wqformat will:

- Rename columns
- Update variable names

wqformat will not:

- Check for QAQC errors

## Converting Standard Formats

By default, wqformat accepts eight different formats, listed below.
Converting data between accepted formats is a simple process: use
[`format_results()`](https://massbays-tech.github.io/wqformat/reference/format_results.md)
to format water quality results and
[`format_sites()`](https://massbays-tech.github.io/wqformat/reference/format_sites.md)
to format site metadata. These functions will update column names,
update variable names, and standardize units.

| Format                     | Code        | Water Quality Data |
|----------------------------|-------------|--------------------|
| Blackstone River Coalition | MA_BRC      | Sites, Results     |
| Friends of Casco Bay       | ME_FOCB     | Sites, Results     |
| Maine DEP                  | ME_DEP      | Results only       |
| MassWateR                  | MassWateR   | Sites, Results     |
| Rhode Island DEM           | RI_DEM      | Results only       |
| URI Watershed Watch        | RI_WW       | Sites, Results     |
| wqdashboard                | wqdashboard | Sites, Results     |
| WQX                        | WQX         | Sites, Results     |

Accepted formats. Water quality result data is supported for all
formats, however site data is not supported for Maine DEP or Rhode
Island DEM. {.table}

To format result data for MassWateR or WQdashboard, you may use
[`format_mwr_results()`](https://massbays-tech.github.io/wqformat/reference/format_mwr_results.md)
or
[`format_wqd_results()`](https://massbays-tech.github.io/wqformat/reference/format_wqd_results.md)
respectively.

## Converting Custom Formats

wqformat includes a series of helper functions to convert data to or
from a custom format.

### Update Columns

To adjust column names, use
[`rename_col()`](https://massbays-tech.github.io/wqformat/reference/rename_col.md)
or
[`concat_col()`](https://massbays-tech.github.io/wqformat/reference/concat_col.md).

To change a column type, use
[`col_to_numeric()`](https://massbays-tech.github.io/wqformat/reference/col_to_numeric.md),
[`col_to_date()`](https://massbays-tech.github.io/wqformat/reference/col_to_date.md),
or
[`col_to_time()`](https://massbays-tech.github.io/wqformat/reference/col_to_time.md).

### Update Variables

To update the variables within a column, use
[`update_var()`](https://massbays-tech.github.io/wqformat/reference/update_var.md).
To check for typos or other errors, use
[`warn_invalid_var()`](https://massbays-tech.github.io/wqformat/reference/warn_invalid_var.md).

If a column contains state names, use
[`state_to_abb()`](https://massbays-tech.github.io/wqformat/reference/state_to_abb.md)
or
[`abb_to_state()`](https://massbays-tech.github.io/wqformat/reference/abb_to_state.md)
to format all states as abbreviations or full names respectively.

### Update Units

To convert all values in a column to a unit of your choice, use
[`set_units()`](https://massbays-tech.github.io/wqformat/reference/set_units.md).

To standardize units within a parameter type or other group, use
[`standardize_units()`](https://massbays-tech.github.io/wqformat/reference/standardize_units.md)
or
[`standardize_units_across()`](https://massbays-tech.github.io/wqformat/reference/standardize_units_across.md).
