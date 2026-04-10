combine_files <- function(files) {
  # Load data
  data_small <- files %>%
    map(load_data) %>% # custom data loading function that deals with multiple file formats
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, tz = "EST")) %>%
    filter(
      !is.na(TIMESTAMP),
      year(TIMESTAMP) >= 2021
    ) %>%
    distinct()
  
  return(data_small)
}