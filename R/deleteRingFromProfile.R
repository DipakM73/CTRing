#' Delete ring from a pith to bark profile
#'
#' @param n Number of rings to remove
#' @param densProfile Density profile
#'
#' @return Corrected density profile with ring(s) removed and red bar in plot of deleted ring
#' @export
#'
#' @examples
deleteRingFromProfile <- function(n = 1, densProfile) {

  print(paste("Select", n, "ring(s) on graph delete"))
  newCoord <- locator(n)

  for (i in c(1:n)){
    newIndex <- min(which.min(abs(densProfile$distRingChange - newCoord$x[i])))
    newDist <- densProfile$distRingChange[newIndex]
    densProfile$ring_limits <- densProfile$ring_limits[-newIndex]
    densProfile$distRingChange <- densProfile$distRingChange[-newIndex]
    abline(v = newDist, col = 'red')
  }
  return(densProfile)
}
