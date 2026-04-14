# Load and combine data

This file takes in a list of file paths and loads/formats the data for
analysis. The formatting done here is specific to the loggernet file
formats for the GENX and Chapada projects.

## Usage

``` r
load_loggernet_flux_data(files, format)
```

## Arguments

- files:

  list of file paths

- format:

  file format ("GENX" or "Chapada")

## Value

Combined and formatted data
