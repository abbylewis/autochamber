
# TO DO warning message for negative values set to NA
# TO DO deal with ppb upstream
# TO DO filter(!is.na(MIU_VALVE), MIU_VALVE %in% 1:12) upstream
# TO DO right now this renames Fluxing_Chamber to MIU_VALVE. 



# requires zoo, tidyverse

#' calculate_flux
#'
#' @description
#' Calculate linear rate of change in CO2, CH4, and/or N2O for each flux interval
#'
#' @param data_small dataset. Must contain columns for MIU_VALVE, TIMESTAMP, 
#'   and CH4d_ppm/CO2d_ppm/N2Od_ppm
#' @param cutoff_start Amount of time to remove after the start of the flux 
#'    interval (in seconds)
#' @param cutoff_end End of flux, as determined from the start of the flux 
#'    interval (in seconds)
#' @param group_col Optional grouping variable. See details
#' 
#' @details This function calculates the linear rate of change in gas (CO2, CH4, 
#'   and/or N2O) concentrations. The function requires a dataframe of input data
#'   that contains columns for gas concentrations (in ppm), timestamps, and 
#'   a column with the chamber index. By default, fluxes are grouped by 
#'   timestamp and chamber index, but additional grouping columns can be 
#'   identified in group_col (e.g., if data from multiple gas analyzers are 
#'   combined in data_small)
#'
#' @return L0 slopes
calculate_flux <- function(data_small,
                           cutoff_start = NA,
                           cutoff_end = NA,
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
  
  ### --- Format data ---
  data_numeric <- data_small %>%
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
  
  ### --- Filter ---
  filtered_data <- grouped_data %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      n = sum(Manifold_Timer >= cutoff_start &
                Manifold_Timer <= cutoff_end)
    ) %>%
    filter(
      Manifold_Timer >= cutoff_start,
      Manifold_Timer <= cutoff_end,
      max(change_s) < 1000,
      n < 200
    )
  
  ### --- Data flags (dynamic) ---
  data_flags <- filtered_data %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      across(all_of(gas_cols),
             ~ ifelse(sum(!is.na(.x)) > 5,
                      "No issues", "Insufficient data"),
             .names = "Flag_{.col}_slope"),
      TIMESTAMP = unique(start),
      n_removed = unique(n),
      .groups = "drop"
    )
  
  ### --- Run models ---
  slopes <- filtered_data %>%
    pivot_longer(all_of(gas_cols), names_to = "gas", values_to = "conc") %>%
    group_by(across(all_of(c("gas", group_vars)))) %>%
    mutate(n = sum(!is.na(conc))) %>%
    filter(!is.na(conc), n > 5) %>%
    summarize(
      model = list(lm(conc ~ change)),
      slope_ppm_per_day = model[[1]]$coefficients[[2]],
      R2 = summary(model[[1]])$r.squared,
      p = summary(model[[1]])$coefficients[2, 4],
      se = summary(model[[1]])$coefficients[2, 2],
      rmse = sqrt(mean(model[[1]]$residuals^2)),
      max = max(conc),
      min = min(conc),
      init = first(conc),
      flux_start = min(TIMESTAMP),
      flux_end = max(TIMESTAMP),
      TIMESTAMP = unique(start),
      n = sum(!is.na(conc)),
      .groups = "drop"
    ) %>%
    select(-model) %>%
    
    # Clean gas names dynamically
    mutate(gas = case_when(
      gas == "CH4d_ppm" ~ "CH4",
      gas == "CO2d_ppm" ~ "CO2",
      gas == "N2Od_ppm" ~ "N2O",
      TRUE ~ gas
    )) %>%
    
    pivot_wider(
      names_from = gas,
      values_from = c(slope_ppm_per_day, R2, p, se, rmse, init, max, min),
      names_glue = "{gas}_{.value}"
    ) %>%
    
    full_join(data_flags) %>%
    
    mutate(
      n = ifelse(is.na(n), n_removed, n)
    ) %>%
    
    select(-n_removed)
  
  return(slopes)
}
