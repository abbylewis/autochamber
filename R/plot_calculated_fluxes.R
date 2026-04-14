#' Plot calculated fluxes
#'
#' @description
#' Visualize calculated fluxes with optional grouping.
#'
#' @param slopes Data frame containing calculated fluxes.
#' Must include TIMESTAMP, Chamber, and gas columns (e.g., CH4, CO2, N2O).
#'
#' @param group_cols Optional character vector of grouping variables.
#' @param start_date Optional start date
#' @param end_date Optional end date
#' @param daily Logical. Aggregate to daily mean?
#'
#' @return A plotly object
#' @export
plot_calculated_fluxes <- function(
  slopes,
  group_cols = NULL,
  start_date = NULL,
  end_date = NULL,
  daily = FALSE
) {
  # -----------------------------
  # Standardize column names
  # -----------------------------

  if (!"Chamber" %in% names(slopes)) {
    stop("slopes must contain Chamber")
  }

  if (!"TIMESTAMP" %in% names(slopes)) {
    stop("slopes must contain TIMESTAMP")
  }

  # -----------------------------
  # Detect gases dynamically
  # -----------------------------

  gas_cols_all <- c(
    "CH4_slope_ppm_per_day",
    "CO2_slope_ppm_per_day",
    "N2O_slope_ppm_per_day"
  )
  gas_cols <- intersect(gas_cols_all, names(slopes))

  # -----------------------------
  # Date filtering
  # -----------------------------

  slopes <- slopes |>
    dplyr::mutate(TIMESTAMP = lubridate::as_datetime(TIMESTAMP))

  if (!is.null(start_date)) {
    slopes <- dplyr::filter(
      slopes,
      TIMESTAMP >= start_date
    )
  }

  if (!is.null(end_date)) {
    slopes <- dplyr::filter(
      slopes,
      TIMESTAMP <= end_date
    )
  }

  # -----------------------------
  # Pivot to long format
  # -----------------------------

  plot_df <- slopes |>
    dplyr::rename(Chamber = Chamber) |>
    dplyr::select(
      dplyr::all_of(c("TIMESTAMP", "Chamber", group_cols, gas_cols))
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(gas_cols),
      names_to = "gas",
      values_to = "value"
    ) |>
    # Clean gas labels
    dplyr::mutate(
      gas = dplyr::case_match(
        gas,
        "CH4_slope_ppm_per_day" ~ "CH4",
        "CO2_slope_ppm_per_day" ~ "CO2",
        "N2O_slope_ppm_per_day" ~ "N2O",
        .default = gas
      )
    )

  # -----------------------------
  # Optional daily aggregation
  # -----------------------------

  if (isTRUE(daily)) {
    plot_df <- plot_df |>
      dplyr::mutate(
        TIMESTAMP = lubridate::as_date(TIMESTAMP),
        TIMESTAMP = as.POSIXct(TIMESTAMP)
      ) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c("TIMESTAMP", "Chamber", "gas", group_cols)))
      ) |>
      dplyr::summarize(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # -----------------------------
  # Build facet formula
  # -----------------------------

  facet_formula <- if (!is.null(group_cols) && length(group_cols) > 0) {
    stats::as.formula(
      paste("gas ~", paste(group_cols, collapse = " + "))
    )
  } else {
    stats::as.formula(paste("gas~."))
  }

  # -----------------------------
  # Plot
  # -----------------------------

  p1 <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = TIMESTAMP,
      y = value,
      color = factor(Chamber)
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70") +
    ggplot2::geom_point(size = 0.6) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(facet_formula, scales = "free_y") +
    ggplot2::labs(
      y = "Flux (umol/m2/s)",
      x = NULL,
      color = "Chamber"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey")
    )

  # -----------------------------
  # Plotly output
  # -----------------------------

  plotly::ggplotly(
    p1,
    tooltip = c("Chamber", "TIMESTAMP", "value")
  ) |>
    plotly::layout(
      legend = list(tracegroupgap = 0)
    )
}
