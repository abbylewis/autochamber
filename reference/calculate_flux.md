# calculate_flux

Calculate linear rate of change in CO2, CH4, and/or N2O for each flux
interval

## Usage

``` r
calculate_flux(data_small, cutoff_start, cutoff_end, group_cols = NULL)
```

## Arguments

- data_small:

  dataset. Must contain columns for MIU_VALVE, TIMESTAMP, and
  CH4d_ppm/CO2d_ppm/N2Od_ppm

- cutoff_start:

  Amount of time to remove after the start of the flux interval (in
  seconds)

- cutoff_end:

  End of flux, as determined from the start of the flux interval (in
  seconds)

- group_cols:

  Optional grouping variable. See details

## Details

This function calculates the linear rate of change in gas (CO2, CH4,
and/or N2O) concentrations. The function requires a dataframe of input
data that contains columns for gas concentrations (in ppm), timestamps,
and a column with the chamber index. By default, fluxes are grouped by
timestamp and chamber index, but additional grouping columns can be
identified in group_cols (e.g., if data from multiple gas analyzers are
combined in data_small)
