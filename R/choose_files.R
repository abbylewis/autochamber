#' Choose files
#'
#' @param input_folder Folder where input .dat files are stored
#' @param l0_file_path L0 file path
#' @param reprocess Whether or not to re-process calcualted fluxes (T/F)
#' @param start_date Start date of files to process
#' @param end_date End date of files to process
#' @param files_to_exclude Vector of file names to exclude
#'
#' @returns List of files
#' @export
choose_files <- function(input_folder,
                         l0_file_path,
                         reprocess = F,
                         start_date = NULL,
                         end_date = NULL,
                         files_to_exclude = NULL
                         ) {
  ### Load files ###
  files <- list.files(input_folder, full.names = T)
  # By default, only calculate slopes for files that have been modified/created since the last time we ran the script
  if (!reprocess) {
    modif_start_date <- file.info(l0_file_path)$mtime
    files <- files[file.info(files)$mtime > modif_start_date]
  }
  # If a start and end date are provided, look for files that match these dates
  if (!is.null(start_date) & !is.null(end_date)) {
    possible_file_names <- seq(as.Date(start_date),
      as.Date(end_date),
      by = "1 day"
    ) |>
      format("%Y%m%d")
    if (as.Date(end_date) >= Sys.Date()) {
      possible_file_names <- c(possible_file_names, "current.dat")
    }
    files <- files[grepl(paste0(possible_file_names, collapse = "|"), files)]
  } else if (!is.null(start_date) | !is.null(end_date)) {
    stop("If you provide a start or end date, you must provide both")
  }

  if (length(files) == 0) {
    message("No files to process")
    return(read_csv(l0_file_path, show_col_types = F))
  }

  files <- files[!grepl(paste0(files_to_exclude, collapse = "|"), files)]

  return(files)
}
