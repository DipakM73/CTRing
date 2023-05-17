#' Add ring to pith to bark profile from CT scan image
#'
#' @param n Number of rings to add
#' @param densProfile Density profile
#' @param im Density matrix
#'
#' @return Corrected density profile with new ring(s) added and blue bar in plot of added ring
#' @export
#'
#' @examples
addRingFromImage <- function(n = 1, densProfile, im) {
  segXY <- data.frame(x = densProfile$xx,
                      y = densProfile$yy)

  print(paste("Clic", n, "time(s) on graph to position new ring(s)"))
  newCoord <- locator(n)
  newCoord$x <- newCoord$x * dim(im)[1]
  newCoord$y <- newCoord$y * dim(im)[2]

  for (i in 1:n){
    segXY$dist <- sqrt((segXY$x - newCoord$x[i])^2 + (segXY$y - newCoord$y[i])^2)
    newIndex <- which.min(segXY$dist)
    newDist <- densProfile$distFromPith[newIndex]
    densProfile$ring_limits <- sort(c(densProfile$ring_limits, newIndex))
    densProfile$distRingChange <- sort(c(densProfile$distRingChange, newDist))
    points(x = densProfile$xx[newIndex]/dim(im)[1], y = densProfile$yy[newIndex]/dim(im)[2],
           pch = 10, col = 'blue')
  }

  return(densProfile)
}
