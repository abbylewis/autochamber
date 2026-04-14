# Example flux processing

## Load package

``` r
# download package, if needed
# install.packages("remotes")
# remotes::install_github("abbylewis/autochamber")

# load package
library(autochamber)
```

## The data

This package includes two example datasets, `data_small_genx` and
`data_small_chapada`. Each of these datasets includes a few days of
automated flux chamber data. Columns include:

- `TIMESTAMP`: time of sample
- `CH4d_ppm`: methane concentration (ppm)
- `CO2d_ppm`: carbon dioxide concentration (ppm)
- `N2Od_ppm`: nitrous oxide concentration (ppm). Note that this is not
  included in `data_small_chapada` (not measured)
- `MIU_VALVE`: ID number for chamber being sampled at any given time
- `Manifold_Timer` (time in seconds output from sampling manifold)

You will notice that `data_small_genx` also has a column `Format`, while
`data_small_chapada` has columns for `location` and `Flux_Status`.

- `location` is an example of a grouping variable that may be relevant
  to flux processing. `data_small_chapada` includes data from two gas
  analyzers, which are differentiated by the `location` column. This
  will need to be specified in many of the functions below.
- Conversely, `Format` and `Flux_Status` are not needed for the current
  flux processing workflow, and they will be ignored and ultimately
  removed throughout the functions below.

``` r
head(data_small_genx)
#>             TIMESTAMP CH4d_ppm CO2d_ppm  N2Od_ppm MIU_VALVE Manifold_Timer
#> 1 2026-03-31 23:55:50 2.009185 512.4178 0.3556001         6            340
#> 2 2026-03-31 23:56:00 2.009406  513.241 0.3551433         6            350
#> 3 2026-03-31 23:56:10 2.009223 513.1506 0.3550273         6            360
#> 4 2026-03-31 23:56:20 2.009825 514.8185 0.3554337         6            370
#> 5 2026-03-31 23:56:30 2.010011 514.4644 0.3554064         6            380
#> 6 2026-03-31 23:56:40  2.00984  514.113 0.3554103         6            390
#>   Format
#> 1    NEW
#> 2    NEW
#> 3    NEW
#> 4    NEW
#> 5    NEW
#> 6    NEW

head(data_small_chapada)
#>   location           TIMESTAMP CH4d_ppm CO2d_ppm Fluxing_Chamber Manifold_Timer
#> 1      low 2026-04-01 01:30:10 1.967526 519.8607              30            430
#> 2      low 2026-04-01 01:30:20 1.968225 520.7725              30            440
#> 3      low 2026-04-01 01:30:30 1.968620 521.8657              30            450
#> 4      low 2026-04-01 01:30:40 1.969235 522.1307              30            460
#> 5      low 2026-04-01 01:30:50 1.969673 523.2021              30            470
#> 6      low 2026-04-01 01:31:00 1.970378 523.8209              30            480
#>   Flux_Status
#> 1           3
#> 2           3
#> 3           3
#> 4           3
#> 5           3
#> 6           3
```

## Isolate recent raw data

When working with months or years of flux data, file sizes can get huge.
It may be helpful to isolate recent raw data for real-time visualization
and quality control. The
[`generate_recent_raw()`](https://abbylewis.github.io/autochamber/reference/generate_recent_raw.md)
function takes a dataset of raw flux data and outputs a trimmed file
with a certain number of recent days, which may then be stored and
quickly visualized.

``` r
recent_raw_genx <- generate_recent_raw(
  data_small_genx,
  # For real-time workflows, set 'today' via Sys.Date()
  # Here, we are instead just using the most recent data
  today = lubridate::as_date(max(data_small_genx$TIMESTAMP)),
  days = 2
)

recent_raw_chapada <- generate_recent_raw(
  data_small_chapada,
  group_cols = "location",
  today = lubridate::as_date(max(data_small_chapada$TIMESTAMP)),
  days = 2
)
```

## Visualize

Let’s take a look at the most recent data.
[`plot_raw_data()`](https://abbylewis.github.io/autochamber/reference/plot_raw_data.md)
plots the greenhouse gas data from the gas analyzer, with linear slopes
for a given flux interval shown as solid black lines.

``` r
# Only want ~an hour or two for this figure
data_to_plot_genx <- head(recent_raw_genx, 500)
# Plot
plot_raw_data(
  data_to_plot_genx,
  cutoff_start = 220,
  cutoff_end = 510
)
#> `geom_smooth()` using formula = 'y ~ x'
```

``` r

data_to_plot_chapada <- head(recent_raw_chapada, 500)
# Plot
plot_raw_data(
  data_to_plot_chapada,
  cutoff_start = 180,
  cutoff_end = 680,
  group_cols = "location"
)
#> `geom_smooth()` using formula = 'y ~ x'
```

## Calculate fluxes

Those flux intervals look pretty good! That is, they look like there is
a linear slope in the range of data we specified. Let’s now use those
same flux intervals to calculate slopes for all gases over the full
dataset using
[`calculate_flux()`](https://abbylewis.github.io/autochamber/reference/calculate_flux.md).

``` r
genx_fluxes <- calculate_flux(data_small_genx,
  cutoff_start = 220, # Same as above
  cutoff_end = 510
)

chapada_fluxes <- calculate_flux(data_small_chapada,
  cutoff_start = 180,
  cutoff_end = 680,
  group_cols = "location"
)
```

## Plot raw fluxes

To take a look at fluxes, you can use the function
[`plot_calculated_fluxes()`](https://abbylewis.github.io/autochamber/reference/plot_calculated_fluxes.md),
which includes options for grouping columns, start and end dates, and
daily aggregation (see documentation).

``` r
plot_calculated_fluxes(genx_fluxes)
```

``` r

plot_calculated_fluxes(chapada_fluxes,
  group_cols = "location"
)
```

## Add maintenance log

For each project, we have created maintenance logs that record any
intervals of data that should be flagged and potentially also set to NA.
These maintenance logs are stored as google sheets, which you can access
via the links below.

When adapting this workflow for other projects, the maintenance log will
need to follow the same general format. The sheet needs to be set so
that anyone with the link can view. Column order does not matter, but
column name and capitalization does.

- `Start_time`: Start of the window of time that should be flagged
- `End_time`: End time
- `Description`: Text description of why this period is being flagged
- `Chambers`: Which chambers does this apply to
- `Analyzer`: Type of gas analyzer. Options are `CO2/CH4`, `N2O`, and
  `all`
- `Flag`: Short text flag that will be added to data. E.g., “Chamber
  maintenance”
- `Remove`: Whether or not to remove data in this window (y/n)

Grouping columns should be included in the maintenance log. For example,
you will see `location` in the Chapada maintenance log.

``` r
genx_fluxes_cleaned <-
  add_maintenance_log(
    slopes = genx_fluxes,
    gs_url = "http://docs.google.com/spreadsheets/d/1_uk8-335NDJOdVU6OjLcxWx4MamNJeVEbVkSmdb9oRs/edit?gid=0#gid=0"
  )
#> ✔ Reading from GenX QAQC log.
#> ✔ Range Sheet1.

chapada_fluxes_cleaned <-
  add_maintenance_log(
    slopes = chapada_fluxes,
    gs_url = "https://docs.google.com/spreadsheets/d/103PpjEmjLAQkov9ywjA5KxyJiIP3nEWy7V8gWVZhd1M/edit?gid=0#gid=0",
    group_cols = "location"
  )
#> ✔ Reading from Chapada_QAQC_log.
#> ✔ Range Sheet1.
```

## Run seasonal QAQC

``` r
genx_fluxes_qaqced <- filter_fluxes_by_SE(
  genx_fluxes_cleaned
)
#> [1] CH4: 3 removed (2.7%); CO2: 1 removed (0.9%)
#> [2] CH4: 1 removed (0.9%); CO2: 0 removed (0%)
#> [3] CH4: 2 removed (1.8%); CO2: 2 removed (1.8%)
#> [4] CH4: 1 removed (0.9%); CO2: 2 removed (1.8%)
#> [5] CH4: 2 removed (1.8%); CO2: 3 removed (2.8%)
#> [6] CH4: 2 removed (1.8%); CO2: 2 removed (1.8%)
#> [7] CH4: 2 removed (1.8%); CO2: 3 removed (2.7%)
#> [8] CH4: 2 removed (1.8%); CO2: 2 removed (1.8%)
#> [9] CH4: 2 removed (1.8%); CO2: 3 removed (2.8%)
#> [10] CH4: 3 removed (2.7%); CO2: 0 removed (0%)
#> [11] CH4: 2 removed (1.8%); CO2: 2 removed (1.8%)
#> [12] CH4: 2 removed (1.8%); CO2: 1 removed (0.9%)

chapada_fluxes_qaqced <- filter_fluxes_by_SE(
  chapada_fluxes_cleaned,
  group_cols = "location"
)
#> [10 | high] CH4: 0 removed (0%); CO2: 0 removed (0%)
#> [10 | low] CH4: 0 removed (0%); CO2: 3 removed (5.6%)
#> [20 | high] CH4: 2 removed (3.8%); CO2: 1 removed (1.9%)
#> [20 | low] CH4: 0 removed (0%); CO2: 0 removed (0%)
#> [30 | high] CH4: 2 removed (3.7%); CO2: 2 removed (3.7%)
#> [30 | low] CH4: 1 removed (1.8%); CO2: 1 removed (1.8%)
#> [40 | high] CH4: 1 removed (1.9%); CO2: 2 removed (3.7%)
#> [40 | low] CH4: 0 removed (0%); CO2: 3 removed (5.6%)
#> [50 | high] CH4: 0 removed (0%); CO2: 1 removed (1.9%)
#> [50 | low] CH4: 0 removed (0%); CO2: 2 removed (3.6%)
#> [60 | high] CH4: 0 removed (0%); CO2: 1 removed (1.8%)
#> [60 | low] CH4: 1 removed (1.8%); CO2: 1 removed (1.8%)
#> [70 | high] CH4: 0 removed (0%); CO2: 1 removed (1.8%)
#> [70 | low] CH4: 1 removed (1.8%); CO2: 1 removed (1.8%)
#> [80 | high] CH4: 1 removed (1.8%); CO2: 2 removed (3.6%)
#> [80 | low] CH4: 1 removed (1.9%); CO2: 2 removed (3.7%)
#> [90 | high] CH4: 0 removed (0%); CO2: 2 removed (3.8%)
#> [90 | low] CH4: 1 removed (1.9%); CO2: 1 removed (1.9%)
```

## Visualize

Let’s run the same
[`plot_calculated_fluxes()`](https://abbylewis.github.io/autochamber/reference/plot_calculated_fluxes.md)
function with these quality controlled fluxes to make sure everything
looks right.

``` r
plot_calculated_fluxes(
  genx_fluxes_qaqced |>
    # Plotting function requires TIMESTAMP column
    dplyr::rename(TIMESTAMP = flux_time)
)
```

``` r

plot_calculated_fluxes(
  chapada_fluxes_qaqced |>
    dplyr::rename(TIMESTAMP = flux_time),
  group_cols = "location"
)
```

## Ta da!

That’s all for now! Please reach out to Abby Lewis
(abigail.sl.lewis@gmail.com) if you have any questions or concerns.
Requests for new features can be added as Issues within the GitHub
repository for this project.
