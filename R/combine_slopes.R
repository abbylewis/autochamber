combine_slopes <- function(new, old) {
  if (!reprocess | !is.null(start_date)) {
    # Load previously calculated slopes
    old_slopes <- read_csv(here::here("processed_data", "L0.csv"),
                           show_col_types = F
    ) %>%
      mutate(
        TIMESTAMP = force_tz(TIMESTAMP, tz = "EST"),
        flux_start = force_tz(flux_start, tz = "EST"),
        flux_end = force_tz(flux_end, tz = "EST")
      ) %>%
      filter(TIMESTAMP < min(slopes$TIMESTAMP) |
               TIMESTAMP > max(slopes$TIMESTAMP))
    #Combine
    slopes_comb <- bind_rows(old_slopes, slopes)
  } else {
    slopes_comb <- slopes
  }
}