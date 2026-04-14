#' Generate recent raw data
#'
#' @description
#' Generate an output file of recent raw data
#'
#' @param data_small dataset. Must contain columns for Chamber, TIMESTAMP,
#'   and CH4d_ppm/CO2d_ppm/N2Od_ppm
#' @param today Latest date of flux data requested
#' @param group_cols Optional grouping variable
#' @param days Number of days of data
#'
#' @return Recent raw dataset
#' @export
generate_recent_raw <- function(data_small,
                                today = Sys.Date(),
                                group_cols = NULL,
                                days = 7) {
  group_vars <- c("group", "Chamber", group_cols) |>
    purrr::discard(is.null)

  # --- Detect available gases ---
  gas_cols_all <- c("CH4d_ppm", "CO2d_ppm", "N2Od_ppm")
  gas_cols_present <- gas_cols_all[gas_cols_all %in% names(data_small)]

  # --- Trim to recent ---
  data_recent <- data_small |>
    dplyr::mutate(date = lubridate::as_date(TIMESTAMP)) |>
    dplyr::filter(date >= lubridate::as_date(today) - lubridate::days(days))

  # --- Format data ---
  data_numeric <- data_recent |>
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

  # --- Determine final gas columns ---
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

  # --- Final output ---
  recent_output <- grouped_data |>
    dplyr::select(
      dplyr::all_of(c(
        "TIMESTAMP",
        "Manifold_Timer",
        "change",
        group_vars,
        gas_cols
      ))
    ) |>
    dplyr::arrange(rev(TIMESTAMP))

  return(recent_output)
}
