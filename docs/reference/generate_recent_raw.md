# Generate recent raw data

Generate an output file of recent raw data

## Usage

``` r
generate_recent_raw(
  data_small,
  today = Sys.Date(),
  group_cols = NULL,
  days = 7
)
```

## Arguments

- data_small:

  dataset. Must contain columns for MIU_VALVE, TIMESTAMP, and
  CH4d_ppm/CO2d_ppm/N2Od_ppm

- today:

  Latest date of flux data requested

- group_cols:

  Optional grouping variable

- days:

  Number of days of data

## Value

Recent raw dataset
