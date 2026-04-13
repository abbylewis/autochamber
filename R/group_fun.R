#' Assign groups for separate readings
#'
#' @param MIU_VALVE Chamber number
#'
#' @description
#' Internal utility for assigning fluxes to groups based on chamber number
#'
#' @returns group number
group_fun <- function(MIU_VALVE) {
  group <- rep(1, length(MIU_VALVE))
  for (i in 2:length(MIU_VALVE)) {
    if (MIU_VALVE[i] == MIU_VALVE[i - 1]) {
      group[i] <- group[i - 1]
    } else {
      group[i] <- group[i - 1] + 1
    }
  }
  return(group)
}
