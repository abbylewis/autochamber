#' Plot raw flux chamber data
#'
#' @description
#' Visualise raw chamber concentration data with optional grouping and flux window highlighting.
#'
#' @param small_raw_dataset Data frame containing raw flux data.
#' Must include TIMESTAMP, Manifold_Timer, MIU_VALVE (or Chamber), and gas columns.
#'
#' @param cutoff_start Numeric. Start of flux window in seconds.
#' @param cutoff_end Numeric. End of flux window in seconds.
#'
#' @param group_cols Optional character vector of grouping variables for faceting.
#'
#' @return A plotly object
#' @export
plot_raw_data <- function(small_raw_dataset,
                          cutoff_start = 200,
                          cutoff_end = 540,
                          group_cols = NULL) {
  # -----------------------------
  # Standardize column names
  # -----------------------------

  if (!"MIU_VALVE" %in% names(small_raw_dataset)) {
    if ("Fluxing_Chamber" %in% names(small_raw_dataset)) {
      small_raw_dataset <- dplyr::rename(
        small_raw_dataset,
        MIU_VALVE = Fluxing_Chamber
      )
    } else {
      stop("Dataset must contain MIU_VALVE or Fluxing_Chamber")
    }
  }

  # -----------------------------
  # Detect gas columns
  # -----------------------------

  gas_cols_all <- c("CH4d_ppm", "CO2d_ppm", "N2Od_ppm")
  gas_cols <- intersect(gas_cols_all, names(small_raw_dataset))

  if (length(gas_cols) == 0) {
    stop("No recognised gas columns found")
  }

  # -----------------------------
  # Prepare data
  # -----------------------------

  raw_plot <- small_raw_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(gas_cols, "Manifold_Timer")),
        as.numeric
      ),
      MIU_VALVE = as.factor(MIU_VALVE)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(gas_cols),
      names_to = "gas",
      values_to = "value"
    ) |>
    dplyr::mutate(
      gas = dplyr::case_match(
        gas,
        "CH4d_ppm" ~ "CH[4]",
        "CO2d_ppm" ~ "CO[2]",
        "N2Od_ppm" ~ "N[2]*O",
        .default = gas
      )
    )

  # -----------------------------
  # Optional grouping/faceting
  # -----------------------------

  facet_formula <- if (!is.null(group_cols)) {
    stats::as.formula(paste("gas~", paste(group_cols, collapse = " + ")))
  } else {
    stats::as.formula(paste("gas~."))
  }

  # -----------------------------
  # Compute time axis
  # -----------------------------

  raw_plot <- raw_plot |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      group_cols
    ))) |>
    dplyr::mutate(
      min_TS = min(TIMESTAMP, na.rm = TRUE),
      secs = as.numeric(difftime(TIMESTAMP, min_TS, units = "secs")),
      used = dplyr::if_else(
        Manifold_Timer >= cutoff_start &
          Manifold_Timer <= cutoff_end,
        "yes",
        "no"
      )
    ) |>
    dplyr::ungroup()

  # -----------------------------
  # Plot
  # -----------------------------

  p1 <- ggplot2::ggplot(
    raw_plot,
    ggplot2::aes(
      x = secs,
      y = value,
      alpha = used,
      shape = used,
      color = MIU_VALVE
    )
  ) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::geom_smooth(
      data = dplyr::filter(raw_plot, used == "yes"),
      ggplot2::aes(
        group = interaction(group, gas, MIU_VALVE),
        x = secs,
        y = value
      ),
      method = "lm",
      se = FALSE,
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::facet_grid(facet_formula, scales = "free_y") +
    ggplot2::scale_alpha_manual(values = c(no = 0.2, yes = 1)) +
    ggplot2::scale_shape_manual(values = c(no = 19, yes = 21)) +
    ggplot2::labs(
      x = "Seconds since start time",
      y = "Value"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey")
    )

  # -----------------------------
  # Plotly output
  # -----------------------------

  plotly::ggplotly(
    p1,
    tooltip = c("TIMESTAMP", "value", "MIU_VALVE")
  )
}
