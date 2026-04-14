#' Load and combine data
#'
#' @param files list of file paths
#' @param format file format ("GENX" or "Chapada")
#'
#' @returns Combined and formatted data
#'
#' @description
#' This file takes in a list of file paths and loads/formats the data for
#' analysis. The formatting done here is specific to the loggernet file
#' formats for the GENX and Chapada projects.
#'
#' @export
#'
load_loggernet_flux_data <- function(files,
                                     format) {
  if (!format %in% c("Chapada", "GENX")) {
    stop(paste0(
      "File format not recognized. The current options are 'GENX' and 'Chapada'"
    ))
  }

  if (format == "Chapada") {
    data_small <- files |>
      purrr::map(load_single_file_chapada) |> # custom data loading function that deals with multiple file formats
      dplyr::bind_rows() |>
      dplyr::distinct()
  }

  if (format == "GENX") {
    data_small <- files |>
      purrr::map(load_single_file_genx) |> # custom data loading function that deals with multiple file formats
      dplyr::bind_rows() |>
      dplyr::distinct()
  }

  return(data_small)
}
