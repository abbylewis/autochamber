#' Remove fluxes with low SE
#'
#' @param seasonal_fluxes Seasonal flux file
#' @param group_cols Columns to group by. Must be in data and maintenance log
#'
#' @returns seasonal_fluxes dataframe with removed values and updated flags
#' @export
filter_fluxes_by_SE <- function(seasonal_fluxes,
                                group_cols = NULL) {
  if (is.null(group_cols)) group_cols <- character(0)

  join_cols <- c("MIU_VALVE", "flux_time", group_cols)

  # --- Prep ---
  seasonal_fluxes <- seasonal_fluxes |>
    dplyr::rename(flux_time = TIMESTAMP) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::filter(!duplicated(flux_time)) |>
    dplyr::ungroup()

  # --- QA/QC ---
  filt <- seasonal_fluxes |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("MIU_VALVE", group_cols)))) |>
    dplyr::mutate(
      cutoff_ch4 = mean(CH4_se, na.rm = TRUE) + 3 * sd(CH4_se, na.rm = TRUE),
      keep_ch4 = !is.na(CH4_se) & CH4_se < cutoff_ch4,
      cutoff_co2 = mean(CO2_se, na.rm = TRUE) + 3 * sd(CO2_se, na.rm = TRUE),
      keep_co2 = !is.na(CO2_se) & CO2_se < cutoff_co2
    ) |>
    dplyr::ungroup()

  # --- Removal table ---
  removal <- filt |>
    dplyr::select(dplyr::all_of(join_cols), keep_ch4, keep_co2) |>
    dplyr::distinct()

  # --- Apply removal ---
  df <- seasonal_fluxes |>
    dplyr::left_join(removal, by = join_cols) |>
    dplyr::mutate(
      CH4_slope_ppm_per_day = ifelse(!keep_ch4, NA, CH4_slope_ppm_per_day),
      CO2_slope_ppm_per_day = ifelse(!keep_co2, NA, CO2_slope_ppm_per_day),
      Flag_CH4d_ppm_slope = ifelse(
        !keep_ch4,
        "Removed due to low SE",
        Flag_CH4d_ppm_slope
      ),
      Flag_CO2d_ppm_slope = ifelse(
        !keep_co2,
        "Removed due to low SE",
        Flag_CO2d_ppm_slope
      )
    ) |>
    dplyr::select(-keep_ch4, -keep_co2)

  # --- Stats ---
  stats <- seasonal_fluxes |>
    dplyr::left_join(removal, by = join_cols) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("MIU_VALVE", group_cols)))) |>
    dplyr::summarize(
      n_removed_ch4 = sum(!keep_ch4 & !is.na(CH4_slope_ppm_per_day), na.rm = TRUE),
      pct_ch4 = n_removed_ch4 / sum(!is.na(CH4_slope_ppm_per_day)) * 100,
      n_removed_co2 = sum(!keep_co2 & !is.na(CO2_slope_ppm_per_day), na.rm = TRUE),
      pct_co2 = n_removed_co2 / sum(!is.na(CO2_slope_ppm_per_day)) * 100,
      .groups = "drop"
    )

  # --- Message ---
  group_display_cols <- c("MIU_VALVE", group_cols)

  msg <- stats |>
    tidyr::unite(
      "group_label",
      dplyr::all_of(group_display_cols),
      sep = " | ",
      remove = FALSE
    ) |>
    dplyr::mutate(
      text = paste0(
        "[", group_label, "] ",
        "CH4: ", n_removed_ch4, " removed (", round(pct_ch4, 1), "%); ",
        "CO2: ", n_removed_co2, " removed (", round(pct_co2, 1), "%)"
      )
    ) |>
    dplyr::pull(text)

  message(paste(msg, collapse = "\n"))

  return(df)
}
