#' Plot scan image, profile path and ring limits
#'
#' @param densProfile Density profile
#' @param im Density matrix
#'
#' @return
#' @export
#'
#' @examples
plotImageProfile <- function(densProfile, im) {
  xSeg <- c(densProfile$xx[1], densProfile$xx[length(densProfile$xx)])/dim(im)[1]
  ySeg <- c(densProfile$yy[1], densProfile$yy[length(densProfile$xx)])/dim(im)[2]

  ringXY <- data.frame(x = densProfile$xx[densProfile$ring_limits],
                       y = densProfile$yy[densProfile$ring_limits])
  #### plot image
  dev.new()
  image(im)
  points(x = xSeg, y = ySeg, pch = 19)
  segments(xSeg[1], ySeg[1], xSeg[2], ySeg[2])
  points(x = ringXY$x/dim(im)[1], y = ringXY$y/dim(im)[2], pch = 10)
}
