genx <- here::here("data-raw", "current.dat")
chapada <- c(
  here::here("data-raw", "Chapada_Low_FLUX_COMB.dat"),
  here::here("data-raw", "Chapada_High_FLUX_COMB.dat")
)

source(here::here("data-raw", "load_data.R"))

data_small_genx <- load_data_genx(genx)
data_small_chapada <- chapada %>%
  map(load_data_chapada) %>%
  bind_rows()

usethis::use_data(data_small_genx, overwrite = TRUE)
usethis::use_data(data_small_chapada, overwrite = TRUE)
