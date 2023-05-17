#' Calculate average wood, earlywood and latewood density for every ring
#'
#' @param densProfile Density profile
#'
#' @return Density profile with average values added
#' @export
#'
#' @examples
calcAvgDens <- function(densProfile) {
  ringList <- splitAt(densProfile$dens, densProfile$ring_limits)
  densProfile$avgDens <- unlist(lapply(ringList, mean))

  if ("ew_limits" %in% names(densProfile)){
    ewLwList <- splitAt(densProfile$dens, sort(c(densProfile$ring_limits, densProfile$ew_limits + 1)))
    ewLwAvgDens <- unlist(lapply(ewLwList, mean))

    densProfile$avgDensEw <- ewLwAvgDens[c(TRUE, FALSE)]
    densProfile$avgDensLw <- ewLwAvgDens[c(FALSE, TRUE)]
  }
  return(densProfile)
}
