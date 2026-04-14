#' Add maintenance log
#'
#' @param slopes Calculated slopes
#' @param gs_url Google sheets URL for maintenance log
#' @param group_cols Columns to group by. Must be in data and maintenance log
#' 
#' @description
#' This function uses a maintenance log on Google Sheets to identify periods 
#' of time in a flux dataframe that should be flagged and potentially removed 
#' (set to NA). 
#'
#' @returns Updated slopes with maintenance applied
#' 
#' @export
add_maintenance_log <- function(slopes,
                                gs_url,
                                group_cols = NULL) {
  # --- Load maintenance log ---
  googlesheets4::gs4_deauth()
  today <- Sys.time()

  maint_log <- googlesheets4::read_sheet(gs_url, col_types = "c") |>
    dplyr::mutate(
      Start_time = lubridate::as_datetime(Start_time, tz = "America/New_York"),
      End_time   = lubridate::as_datetime(End_time, tz = "America/New_York"),
      End_time   = ifelse(is.na(End_time), today, End_time),
      End_time   = lubridate::as_datetime(End_time, tz = "America/New_York")
    )

  # --- Determine grouping columns ---
  if (is.null(group_cols)) group_cols <- character(0)

  # --- Apply maintenance log ---
  for (i in seq_len(nrow(maint_log))) {
    condition <- slopes$TIMESTAMP <= maint_log$End_time[i] &
      slopes$TIMESTAMP >= maint_log$Start_time[i] &
      slopes$MIU_VALVE %in% eval(parse(text = maint_log$Chambers[i]))

    # --- Apply grouping filters ---
    if (length(group_cols) > 0) {
      for (col in group_cols) {
        condition <- condition &
          slopes[[col]] %in% eval(parse(text = maint_log[[col]][i]))
      }
    }

    condition <- ifelse(is.na(condition), FALSE, condition)

    remove_this_row <- condition & maint_log$Remove[i] == "y"

    # --- Apply removals + flags ---
    slopes <- slopes |>
      dplyr::mutate(
        # CH4 + CO2 removal
        dplyr::across(
          dplyr::matches("CH4|CO2") & !dplyr::contains("Flag"),
          ~ ifelse(
            remove_this_row &
              maint_log$Analyzer[i] %in% c("CO2/CH4", "all"),
            NA,
            .
          )
        ),

        # N2O removal
        dplyr::across(
          dplyr::contains("N2O") & !dplyr::contains("Flag"),
          ~ ifelse(
            remove_this_row &
              maint_log$Analyzer[i] %in% c("N2O", "all"),
            NA,
            .
          )
        ),

        # CH4 + CO2 flags
        dplyr::across(
          dplyr::contains("Flag_C"),
          ~ dplyr::case_when(
            condition &
              maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
              (. == "No issues" | is.na(.)) ~ maint_log$Flag[i],
            condition &
              maint_log$Analyzer[i] %in% c("CO2/CH4", "all") &
              grepl(maint_log$Flag[i], .) ~ .,
            condition &
              maint_log$Analyzer[i] %in% c("CO2/CH4", "all") ~
              paste(., maint_log$Flag[i], sep = "; "),
            TRUE ~ .
          )
        ),

        # N2O flags
        dplyr::across(
          dplyr::contains("Flag_N"),
          ~ dplyr::case_when(
            condition &
              maint_log$Analyzer[i] %in% c("N2O", "all") &
              (. == "No issues" | is.na(.)) ~ maint_log$Flag[i],
            condition &
              maint_log$Analyzer[i] %in% c("N2O", "all") &
              grepl(maint_log$Flag[i], .) ~ .,
            condition &
              maint_log$Analyzer[i] %in% c("N2O", "all") ~
              paste(., maint_log$Flag[i], sep = "; "),
            TRUE ~ .
          )
        )
      )
  }

  return(slopes)
}
