# Plot raw flux chamber data

Visualize raw chamber concentration data, separated by flux interval

## Usage

``` r
plot_raw_data(small_raw_dataset, cutoff_start, cutoff_end, group_cols = NULL)
```

## Arguments

- small_raw_dataset:

  Data frame containing raw flux data. Must include TIMESTAMP,
  Manifold_Timer, Chamber, and gas columns.

- cutoff_start:

  Numeric. Start of flux window in seconds.

- cutoff_end:

  Numeric. End of flux window in seconds.

- group_cols:

  Optional character vector of grouping variables for faceting.

## Value

A plotly object
