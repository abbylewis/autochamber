add_maintenance_log <- function(slopes, gs_url) {
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
  
  # Remove data as specified in maintenance log
  googlesheets4::gs4_deauth() # No authentication needed
  today <- Sys.time()
  maint_log <- googlesheets4::read_sheet("http://docs.google.com/spreadsheets/d/1_uk8-335NDJOdVU6OjLcxWx4MamNJeVEbVkSmdb9oRs/edit?gid=0#gid=0",
                                         col_types = "c"
  ) %>%
    mutate(
      Start_time = as_datetime(Start_time, tz = "America/New_York"),
      End_time = as_datetime(End_time, tz = "America/New_York"),
      End_time = ifelse(is.na(End_time), today, End_time),
      End_time = as_datetime(End_time, tz = "America/New_York")
    )
  
  for (i in 1:nrow(maint_log)) {
    condition <- slopes_comb$TIMESTAMP <= maint_log$End_time[i] &
      slopes_comb$TIMESTAMP >= maint_log$Start_time[i] &
      slopes_comb$MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i]))
    
    condition <- ifelse(is.na(condition),
                        FALSE,
                        condition)
    
    remove_this_row <- condition & maint_log$Remove[i] == "y"
    
    slopes_comb <- slopes_comb %>%
      mutate(
        # All CO2 and CH4 columns
        across(
          matches("CH4|CO2") & !contains("Flag"),
          ~ ifelse(
            remove_this_row &
              maint_log$Analyzer[i] %in% c("CO2/CH4", "all"),
            NA,
            .
          )
        ),
        
        # All N2O columns
        across(
          contains("N2O") & !contains("Flag"),
          ~ ifelse(
            remove_this_row &
              maint_log$Analyzer[i] %in% c("N2O", "all"),
            NA,
            .
          )
        ),
        
        # Reset N2O flag
        across(
          contains("Flag_N"),
          ~case_when(condition & 
                       maint_log$Analyzer[i] %in% c("N2O", "all") &
                       (. == "No issues" |is.na(.)) ~ maint_log$Flag[i],
                     condition & 
                       maint_log$Analyzer[i] %in% c("N2O", "all") &
                       grepl(maint_log$Flag[i], .) ~ .,
                     condition & 
                       maint_log$Analyzer[i] %in% c("N2O", "all") ~ 
                       paste(., maint_log$Flag[i], sep = "; "),
                     TRUE ~ .)
        ),
        
        # Reset CO2 and CH4 flag
        across(
          contains("Flag_C"),
          ~case_when(condition & 
                       maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
                       (. == "No issues" |is.na(.)) ~ maint_log$Flag[i],
                     condition & 
                       maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
                       grepl(maint_log$Flag[i], .) ~ .,
                     condition & 
                       maint_log$Analyzer[i] %in% c("CO2/CH4", "all") ~ 
                       paste(., maint_log$Flag[i], sep = "; "),
                     TRUE ~ .)
        )
      )
  }
}