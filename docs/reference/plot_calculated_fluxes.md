# Plot calculated fluxes

Visualize calculated fluxes with optional grouping.

## Usage

``` r
plot_calculated_fluxes(
  slopes,
  group_cols = NULL,
  start_date = NULL,
  end_date = NULL,
  daily = FALSE
)
```

## Arguments

- slopes:

  Data frame containing calculated fluxes. Must include TIMESTAMP,
  Chamber, and gas columns (e.g., CH4, CO2, N2O).

- group_cols:

  Optional character vector of grouping variables.

- start_date:

  Optional start date

- end_date:

  Optional end date

- daily:

  Logical. Aggregate to daily mean?

## Value

A plotly object
