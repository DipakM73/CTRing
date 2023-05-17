#' Add ring to pith to bark profile from profile plot
#'
#' @param n Number of rings to add
#' @param densProfile Density profile
#'
#' @return Corrected density profile with new ring(s) added and blue bar in plot of added ring
#' @export
#'
#' @examples
addRingFromProfile <- function(n = 1, densProfile) {

  print(paste("Clic", n, "time(s) on graph to position new ring(s)"))
  newCoord <- locator(n)

  for (i in c(1:n)){
    newIndex <- min(which.min(abs(densProfile$distFromPith - newCoord$x[i])))
    newDist <- densProfile$distFromPith[newIndex]
    densProfile$ring_limits <- sort(c(densProfile$ring_limits, newIndex))
    densProfile$distRingChange <- sort(c(densProfile$distRingChange, newDist))
    abline(v = newDist, col = 'blue')
  }
  return(densProfile)
}
