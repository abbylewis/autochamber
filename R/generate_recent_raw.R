
#' generate_recent_raw
#'
#' @description
#' Generate an output file of recent raw data
#'
#' @param data_small dataset. Must contain columns for MIU_VALVE, TIMESTAMP, 
#'   and CH4d_ppm/CO2d_ppm/N2Od_ppm
#' @param group_col Optional grouping variable. See details
#'
#' @return L0 slopes
generate_recent_raw <- function(data_small,
                                today = Sys.Date(),
                                group_col = NULL) {
  
  group_vars <- c("group", "MIU_VALVE", group_col) %>%
    purrr::discard(is.null)
  
  # --- Standardize column names ---
  if (!"MIU_VALVE" %in% names(data_small)) {
    if ("Fluxing_Chamber" %in% names(data_small)) {
      data_small <- data_small %>%
        rename(MIU_VALVE = Fluxing_Chamber)
    } else {
      stop("No column found for chamber ID (expected MIU_VALVE or Fluxing_Chamber)")
    }
  }
  
  ### --- Detect available gases ---
  gas_cols_all <- c("CH4d_ppm", "CO2d_ppm", "N2Od_ppm")
  gas_cols_present <- gas_cols_all[gas_cols_all %in% names(data_small)]
  
  ### --- Trim to recent ---
  data_recent <- data_small %>%
    mutate(date = as_date(TIMESTAMP)) %>%
    filter(date >= as_date(today) - days(7))
  
  ### --- Format data ---
  data_numeric <- data_recent %>%
    mutate(across(all_of(c(gas_cols_present, "Manifold_Timer", "MIU_VALVE")), 
                  as.numeric)) %>%
    
    # Clean gases dynamically
    mutate(across(any_of(gas_cols_all),
                  ~ ifelse(.x <= 0, NA, .x)))
  
  ### --- Determine final gas columns ---
  gas_cols <- intersect(c("CH4d_ppm", "CO2d_ppm", "N2Od_ppm"),
                        names(data_numeric))
  
  ### --- Group flux intervals ---
  grouped_data <- data_numeric %>%
    arrange(TIMESTAMP) %>%
    { if (!is.null(group_col)) group_by(., across(all_of(group_col))) else . } %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      start = min(TIMESTAMP),
      end = max(TIMESTAMP),
      change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
      change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs"))
    )
  
  recent_output <- grouped_data  %>%
    select(all_of(c("TIMESTAMP", "Manifold_Timer", "change", group_vars, gas_cols)))
  
  return(recent_output)
}
