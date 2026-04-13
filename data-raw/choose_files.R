choose_files <- function(start_date = NULL,
                         end_date = NULL,
                         reprocess = F) {
  ### Load files ###
  files <- list.files(here::here("Raw_data", "dropbox_downloads"), full.names = T)
  # By default, only calculate slopes for files that have been modified/created since the last time we ran the script
  if (!reprocess) {
    modif_start_date <- file.info(here::here("processed_data", "L0.csv"))$mtime
    files <- files[file.info(files)$mtime > modif_start_date]
  }
  # If a start and end date are provided, look for files that match these dates
  if (!is.null(start_date) & !is.null(end_date)) {
    possible_file_names <- seq(as.Date(start_date),
      as.Date(end_date),
      by = "1 day"
    ) %>%
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
    return(read_csv(here::here("processed_data", "L0.csv"), show_col_types = F))
  }

  exclude <- c(
    "GENX_INSTRUMENT_FLUX_COMB_20240417020046.dat",
    "GENX_INSTRUMENT_FLUX_COMB_20240403020045.dat",
    "GENX_INSTRUMENT_FLUX_COMB_20240501020048.dat",
    "GENX_LGR_04142021_20210505020005.dat"
  )
  files <- files[!grepl(paste0(exclude, collapse = "|"), files)]

  return(files)
}
