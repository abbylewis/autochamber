#' Data from GENX and Chapada flux chambers
#'
#' Formatted data from GENX
#'
#' @format ## `data_small_genx`
#' A data frame with 73,800 rows and 7 columns:
#' \describe{
#'   \item{TIMESTAMP}{Timestamp}
#'   \item{CH4d_ppm}{Methane concentration (ppm)}
#'   \item{CO2d_ppm}{Carbon dioxide concentration (ppm)}
#'   \item{N2Od_ppm}{Nitrous oxide concentration (ppm)}
#'   \item{MIU_VALVE}{Sampling valve (chamber)}
#'   \item{Manifold_Timer}{Time since switching to this valve}
#'   \item{Format}{Data format (only NEW works for this processing code)}
#'   ...
#' }
#' @source <https://serc.si.edu/labs/biogeochemistry-projects/genx>
"data_small_genx"

#' Formatted data from Chapada
#'
#' @format ## `data_small_chapada`
#' A data frame with 80,130 rows and 7 columns:
#' \describe{
#'   \item{location}{Site name}
#'   \item{TIMESTAMP}{Timestamp}
#'   \item{CH4d_ppm}{Methane concentration (ppm)}
#'   \item{CO2d_ppm}{Carbon dioxide concentration (ppm)}
#'   \item{Fluxing_Chamber}{Sampling valve (chamber)}
#'   \item{Manifold_Timer}{Time since switching to this valve}
#'   \item{Flux_Status}{Status of chamber (e.g., open/closed)}
#'   ...
#' }
#' @source <https://serc.si.edu/labs/biogeochemistry-projects/genx>
"data_small_chapada"
