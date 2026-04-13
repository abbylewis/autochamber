# Remove fluxes with low SE

Remove fluxes with low SE

## Usage

``` r
filter_fluxes_by_SE(seasonal_fluxes, group_cols = NULL)
```

## Arguments

- seasonal_fluxes:

  Seasonal flux file

- group_cols:

  Columns to group by. Must be in data and maintenance log

## Value

seasonal_fluxes dataframe with removed values and updated flags
