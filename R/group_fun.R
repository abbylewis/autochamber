#' Assign groups for separate readings
#'
#' @param Chamber Chamber number
#'
#' @description
#' Internal utility for assigning fluxes to groups based on chamber number
#'
#' @returns group number
group_fun <- function(Chamber) {
  group <- rep(1, length(Chamber))
  for (i in 2:length(Chamber)) {
    if (Chamber[i] == Chamber[i - 1]) {
      group[i] <- group[i - 1]
    } else {
      group[i] <- group[i - 1] + 1
    }
  }
  return(group)
}
