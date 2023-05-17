
#' Add ring to pith to bark profile from CT scan image
#'
#' @param n Number of rings to remove
#' @param densProfile Density profile
#' @param im Density matrix
#'
#' @return Corrected density profile with ring(s) removed and red bar in plot of deleted ring
#' @export
#'
#' @examples
deleteRingFromImage <- function(n = 1, densProfile, im) {
  ringXY <- data.frame(x = densProfile$xx[densProfile$ring_limits],
                       y = densProfile$yy[densProfile$ring_limits])

  print(paste("Select", n, "ring(s) on graph delete"))
  newCoord <- locator(n)
  newCoord$x <- newCoord$x * dim(im)[1]
  newCoord$y <- newCoord$y * dim(im)[2]

  for (i in 1:n){
    ringXY$dist <- sqrt((ringXY$x - newCoord$x[i])^2 + (ringXY$y - newCoord$y[i])^2)
    newIndex <- which.min(ringXY$dist)
    deletedRingIndex <- densProfile$ring_limits[newIndex]
    densProfile$ring_limits <- densProfile$ring_limits[-newIndex]
    densProfile$distRingChange <- densProfile$distRingChange[-newIndex]
    points(x = densProfile$xx[deletedRingIndex]/dim(im)[1], y = densProfile$yy[deletedRingIndex]/dim(im)[2],pch = 10, col = 'red')
  }

  return(densProfile)
}
