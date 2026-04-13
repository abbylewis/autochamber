combine_files <- function(files) {
  # Load data
  data_small <- files %>%
    map(load_data) %>% # custom data loading function that deals with multiple file formats
    bind_rows() %>%
    distinct()

  return(data_small)
}
