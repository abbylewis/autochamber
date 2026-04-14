# Add maintenance log

This function uses a maintenance log on Google Sheets to identify
periods of time in a flux dataframe that should be flagged and
potentially removed (set to NA).

## Usage

``` r
add_maintenance_log(slopes, gs_url, group_cols = NULL)
```

## Arguments

- slopes:

  Calculated slopes

- gs_url:

  Google sheets URL for maintenance log

- group_cols:

  Columns to group by. Must be in data and maintenance log

## Value

Updated slopes with maintenance applied
