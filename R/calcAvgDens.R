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

  ringList <- splitAt(densProfile$distFromPith, densProfile$ring_limits)
  ringListLag <- splitAt(densProfile$distFromPith, densProfile$ring_limits-1)
  densProfile$rw <- mapply(calcRingWidth, ringList, ringListLag)

  if ("ew_limits" %in% names(densProfile)){
    ewLwList <- splitAt(densProfile$dens, sort(c(densProfile$ring_limits, densProfile$ew_limits + 1)))
    ewLwAvgDens <- unlist(lapply(ewLwList, mean))

    densProfile$avgDensEw <- ewLwAvgDens[c(TRUE, FALSE)]
    densProfile$avgDensLw <- ewLwAvgDens[c(FALSE, TRUE)]

    ewLwList <- splitAt(densProfile$distFromPith, sort(c(densProfile$ring_limits, densProfile$ew_limits + 1)))
    ewLwListLag <- splitAt(densProfile$distFromPith, sort(c(densProfile$ring_limits, densProfile$ew_limits + 1)))
    ewLwRw <- mapply(calcRingWidth, ewLwList, ewLwListLag)

    densProfile$rwEw <- ewLwRw[c(TRUE, FALSE)]
    densProfile$rwLw <- densProfile$rw - densProfile$rwEw
  }
  return(densProfile)
}
