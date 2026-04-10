generate_l2_fluxes <- function(){
  
  # Output
  write.csv(slopes_comb %>% select(-max_s), 
            here::here("processed_data", "L0.csv"),
            row.names = FALSE
  )
  
  write.csv(
    slopes_comb %>%
      select(-max_s) %>%
      filter(TIMESTAMP > as.Date("2025-03-18")),
    here::here("processed_data", "L0_for_dashboard.csv"),
    row.names = FALSE
  )
  
  recent_raw <- grouped_data %>%
    filter(date >= Sys.Date() - days(7)) %>%
    select(TIMESTAMP, Manifold_Timer, change, MIU_VALVE, group, CH4d_ppm, 
           CO2d_ppm, N2Od_ppm)
  
  write.csv(recent_raw,
            here::here("processed_data", "raw_for_dashboard.csv"),
            row.names = FALSE
  )
  
  if (plot) {
    for (year_i in unique(year(slopes$TIMESTAMP))) {
      p <- slopes %>%
        filter(
          max_s <= 1000,
          month(date) %in% c(1:12)
        ) %>%
        mutate(MIU_VALVE = factor(MIU_VALVE,
                                  levels = c(
                                    1, 4, 7, 10,
                                    3, 6, 9, 12,
                                    2, 5, 8, 11
                                  )
        )) %>%
        filter(year(TIMESTAMP) == year_i) %>%
        ggplot(aes(x = TIMESTAMP, y = max_s)) +
        geom_point(alpha = 0.02) +
        geom_line(aes(y = cutoff, x = as.POSIXct(date)), color = "red") +
        facet_wrap(~MIU_VALVE) +
        ggtitle(year_i) +
        xlab("Date") +
        ylab("Time to peak (s)") +
        theme_bw()
      jpeg(here::here("figures", paste0("TimeToPeak_", year_i, ".jpeg")), width = 6, height = 5, units = "in", res = 300)
      print(p)
      dev.off()
    }
  }
}