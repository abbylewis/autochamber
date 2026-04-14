# autochamber

Utilities for processing greenhouse gas data from automated chambers in
R.

Contact: Abigail Lewis (<abigail.sl.lewis@gmail.com>)

## Description

`autochamber` provides functions to process CH₄, CO₂, and N₂O data from
automated flux chambers. It includes tools for:

- Visualizing, trimming, and compiling raw data
- Calculating fluxes
- Flagging data using maintenance logs and QA/QC filters
- Plotting calculated fluxes

## Installation

``` r
# install.packages("remotes")
remotes::install_github("abbylewis/autochamber")
library(autochamber)
```

## Documentation

Full workflow vignette:
<https://abbylewis.github.io/autochamber/articles/Example_flux_processing.html>
