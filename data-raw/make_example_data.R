# Make example data from .dat files

# Set file paths
genx <- here::here("data-raw", "current.dat")
chapada <- c(
  here::here("data-raw", "Chapada_Low_FLUX_COMB.dat"),
  here::here("data-raw", "Chapada_High_FLUX_COMB.dat")
)

# Load data
data_small_genx <- load_loggernet_flux_data(
  genx,
  format = "GENX"
)

data_small_chapada <- load_loggernet_flux_data(
  chapada,
  format = "Chapada"
)

# Export
usethis::use_data(data_small_genx, overwrite = TRUE)
usethis::use_data(data_small_chapada, overwrite = TRUE)
