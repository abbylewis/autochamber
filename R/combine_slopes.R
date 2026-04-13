#' Combine slopes
#'
#' @param new new (re-processed) slopes
#' @param old older slope file
#'
#' @returns combined slopes
#'
combine_slopes <- function(new, old) {
  old_slopes <- old |>
    dplyr::filter(TIMESTAMP < min(new$TIMESTAMP) |
      TIMESTAMP > max(new$TIMESTAMP))
  # Combine
  slopes_comb <- dplyr::bind_rows(old_slopes, new)

  return(slopes_comb)
}
