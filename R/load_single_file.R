#' Load data- GENX
#'
#' @param file File path
#'
#' @returns Formatted file
#'
load_single_file_genx <- function(file) {
  data_raw <- readr::read_csv(
    file,
    col_types = readr::cols(.default = "c"), skip = 1
  ) |>
    dplyr::filter(!TIMESTAMP == "TS") |>
    dplyr::mutate(TIMESTAMP = lubridate::as_datetime(TIMESTAMP, tz = "EST")) |>
    dplyr::filter(
      !is.na(TIMESTAMP),
      lubridate::year(TIMESTAMP) >= 2021
    )

  # Account for different formatting among files
  if ("GENX_CH4ppm" %in% colnames(data_raw)) {
    data_small <- data_raw |>
      dplyr::rename(
        CH4d_ppm = GENX_CH4ppm,
        CO2d_ppm = GENX_CO2ppm,
        N2Od_ppb = GENX_N20ppb,
        MIU_VALVE = Fluxing_Chamber
      ) |>
      dplyr::mutate(
        Format = "NEW",
        N2Od_ppm = as.numeric(N2Od_ppb) / 1000
      ) |>
      dplyr::select(
        TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppm, MIU_VALVE,
        Manifold_Timer, Format
      )
  } else if ("GENX_CH4ppb" %in% colnames(data_raw)) {
    message(file, " has CH4ppb")
    data_small <- data_raw |>
      dplyr::mutate(
        CH4d_ppm = as.numeric(GENX_CH4ppb),
        CH4d_ppm = as.character(ifelse(CH4d_ppm > 1000,
          CH4d_ppm / 1000, CH4d_ppm
        )),
        N2Od_ppm = as.numeric(GENX_N20ppm),
        N2Od_ppm = as.character(ifelse(as.numeric(N2Od_ppm) < 10000,
          N2Od_ppm,
          N2Od_ppm / 1000
        ))
      ) |>
      dplyr::rename(
        CO2d_ppm = GENX_CO2ppm,
        MIU_VALVE = Fluxing_Chamber
      ) |>
      dplyr::mutate(Format = "NEW") |>
      dplyr::select(
        TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppm, MIU_VALVE,
        Manifold_Timer, Format
      )
  } else if ("LGR_Time" %in% colnames(data_raw)) {
    data_small <- data_raw |>
      dplyr::filter(is.na(LGR_Time) | !duplicated(LGR_Time) |
        !duplicated(CH4d_ppm)) |> # I've spent some time looking into this and there are some duplicated LGR rows
      dplyr::mutate(
        Manifold_Timer = NA,
        N2Od_pm = NA
      ) |>
      dplyr::mutate(Format = "OLD") |>
      dplyr::select(
        TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppm, MIU_VALVE,
        Manifold_Timer, Format
      )
  } else {
    data_small <- data_raw |>
      dplyr::mutate(
        Manifold_Timer = NA,
        N2Od_ppm = NA
      ) |>
      dplyr::mutate(Format = "OLD") |>
      dplyr::select(
        TIMESTAMP, CH4d_ppm, CO2d_ppm, N2Od_ppm, MIU_VALVE,
        Manifold_Timer, Format
      )
  }
  
  data_output <- data_small |>
    dplyr::filter(!is.na(MIU_VALVE), MIU_VALVE %in% 1:12) |>
    dplyr::rename(Chamber = MIU_VALVE)

  return(data_output)
}

#' Load data- ChapadaSTEM
#'
#' @param file file path
#'
#' @returns formatted file
#' @export
#'
load_single_file_chapada <- function(file) {
  data_raw <- readr::read_csv(
    file,
    col_types = readr::cols(.default = "c"), skip = 1
  )

  location <- stringr::str_extract(tolower(file), "high|low")

  data_small <- data_raw |>
    dplyr::rename(
      CH4d_ppb = CH4,
      CO2d_ppm = CO2
    ) |>
    dplyr::filter(!CH4d_ppb == "Smp") |>
    dplyr::mutate(
      CH4d_ppm = as.numeric(CH4d_ppb) / 1000,
      location = location
    ) |>
    dplyr::select(
      location, TIMESTAMP, CH4d_ppm, CO2d_ppm,
      Fluxing_Chamber, Manifold_Timer, Flux_Status
    ) |>
    dplyr::rename(Chamber = Fluxing_Chamber)

  return(data_small)
}
