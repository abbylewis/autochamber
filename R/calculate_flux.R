#' calculate_flux
#'
#' @description
#' Calculate linear rate of change in CO2, CH4, and/or N2O for each flux interval
#'
#' @param data_small dataset. Must contain columns for Chamber, TIMESTAMP,
#'   and CH4d_ppm/CO2d_ppm/N2Od_ppm
#' @param cutoff_start Amount of time to remove after the start of the flux
#'    interval (in seconds)
#' @param cutoff_end End of flux, as determined from the start of the flux
#'    interval (in seconds)
#' @param group_cols Optional grouping variable. See details
#' @param ebullition_cutoff Rolling variance threshold for ebullition
#' @param ebullition_window Number of observations used to calculate rolling variance
#'
#' @details This function calculates the linear rate of change in gas (CO2, CH4,
#'   and/or N2O) concentrations. The function requires a dataframe of input data
#'   that contains columns for gas concentrations (in ppm), timestamps, and
#'   a column with the chamber index. By default, fluxes are grouped by
#'   timestamp and chamber index, but additional grouping columns can be
#'   identified in group_cols (e.g., if data from multiple gas analyzers are
#'   combined in data_small). Ebullition events are identified based on rolling
#'   variance, with the threshold and rolling window provided as parameters. 
#'   
#' @returns Dataframe of calculated fluxes
#'
#' @export
calculate_flux <- function(data_small,
                           cutoff_start,
                           cutoff_end,
                           group_cols = NULL,
                           ebullition_cutoff = 0.001,
                           ebullition_window = 5) {
  # --- Identify all grouping variables ---
  # Note that "group" will be created later
  group_vars <- c("group", "Chamber", group_cols) |>
    purrr::discard(is.null)

  # --- Detect available gases ---
  gas_cols_all <- c("CH4d_ppm", "CO2d_ppm", "N2Od_ppm", "H2O_ppm")
  gas_cols_present <- gas_cols_all[gas_cols_all %in% names(data_small)]

  # --- Format data ---
  data_numeric <- data_small |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(gas_cols_present, "Manifold_Timer", "Chamber")),
        as.numeric
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(gas_cols_all),
        ~ ifelse(.x <= 0, NA, .x)
      )
    )

  gas_cols <- intersect(gas_cols_all, names(data_numeric))

  # --- Group flux intervals ---
  grouped_data <- data_numeric |>
    dplyr::arrange(TIMESTAMP)

  if (!is.null(group_cols)) {
    grouped_data <- grouped_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  grouped_data <- grouped_data |>
    dplyr::mutate(group = group_fun(Chamber)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate(
      start = min(TIMESTAMP),
      end = max(TIMESTAMP),
      change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
      change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs"))
    )

  # --- Filter ---
  filtered_data <- grouped_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate(
      n = sum(Manifold_Timer >= cutoff_start &
        Manifold_Timer <= cutoff_end)
    ) |>
    dplyr::filter(
      Manifold_Timer >= cutoff_start,
      Manifold_Timer <= cutoff_end,
      max(change_s) < 1000,
      n < 200
    )

  # --- Data flags ---
  data_flags <- filtered_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(gas_cols),
        ~ ifelse(sum(!is.na(.x)) > 5, "No issues", "Insufficient data"),
        .names = "Flag_{.col}_slope"
      ),
      TIMESTAMP = unique(start),
      n_removed = unique(n),
      .groups = "drop"
    )
  
  eb_by_roll_var <- filtered_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::arrange(change_s, .by_group = T) |>
    dplyr::mutate(
      delta = CH4d_ppm - lag(CH4d_ppm),
      run_var = RcppRoll::roll_var(CH4d_ppm, 5, fill = NA),
      ebullition = run_var > ebullition_cutoff & 
        delta > 0
    ) |>
    dplyr::summarize(
      ebullition = sum(ebullition, na.rm = T) > 0,
      CH4_slope_ppm_per_day_ebullition = (dplyr::last(CH4d_ppm) - dplyr::first(CH4d_ppm)) /
        (dplyr::last(change_s) - dplyr::first(change_s)) *60*60*24,
      .groups = "drop") |>
    dplyr::mutate(
      #Can't have negative ebullition
      ebullition = ifelse(
        CH4_slope_ppm_per_day_ebullition < 0,
        F,
        ebullition),
      #Don't record ebullition flux if not ebullition
      CH4_slope_ppm_per_day_ebullition = ifelse(
        !ebullition,
        NA,
        CH4_slope_ppm_per_day_ebullition)) |>
    dplyr::select(dplyr::all_of(c(
      group_vars, 
      "ebullition", 
      "CH4_slope_ppm_per_day_ebullition")))
    
  # --- Models ---
  slopes <- filtered_data |>
    tidyr::pivot_longer(
      dplyr::all_of(gas_cols),
      names_to = "gas",
      values_to = "conc"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("gas", group_vars)))) |>
    dplyr::mutate(n = sum(!is.na(conc))) |>
    dplyr::filter(!is.na(conc), n > 5) |>
    dplyr::summarize(
      model = list(stats::lm(conc ~ change)),
      slope_ppm_per_day = model[[1]]$coefficients[[2]],
      R2 = summary(model[[1]])$r.squared,
      p = summary(model[[1]])$coefficients[2, 4],
      se = summary(model[[1]])$coefficients[2, 2],
      rmse = sqrt(mean(model[[1]]$residuals^2)),
      max = max(conc),
      min = min(conc),
      init = dplyr::first(conc),
      flux_start = min(TIMESTAMP),
      flux_end = max(TIMESTAMP),
      TIMESTAMP = unique(start),
      n = sum(!is.na(conc)),
      .groups = "drop"
    ) |>
    dplyr::select(-model) |>
    dplyr::mutate(
      gas = dplyr::case_when(
        gas == "CH4d_ppm" ~ "CH4",
        gas == "CO2d_ppm" ~ "CO2",
        gas == "N2Od_ppm" ~ "N2O",
        gas == "H2O_ppm" ~ "H2O",
        TRUE ~ gas
      )
    ) |>
    tidyr::pivot_wider(
      names_from = gas,
      values_from = c(slope_ppm_per_day, R2, p, se, rmse, init, max, min),
      names_glue = "{gas}_{.value}"
    ) |>
    dplyr::full_join(
      eb_by_roll_var,
      by = c(group_vars)
    ) |>
    dplyr::full_join(
      data_flags,
      by = c(group_vars, "TIMESTAMP")
    ) |>
    dplyr::mutate(
      n = ifelse(is.na(n), n_removed, n)
    ) |>
    dplyr::select(-n_removed, -group)

  return(slopes)
}
