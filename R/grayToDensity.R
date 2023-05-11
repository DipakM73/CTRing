#' Convert from 8bit gray scale to density
#'
#' @param im Matrix of CT scan image in 8bit gray scale
#' @param a Intercept of the calibration curve
#' @param b Slope of the calibration curve
#'
#' @return Matrix of density values
#' @export
#'
#' @examples
grayToDensity <- function(im, a = -0.1321, b = 0.01834) {
  im <- a + b*im
  im <- pmax(im, 0)
  return(im)
}
