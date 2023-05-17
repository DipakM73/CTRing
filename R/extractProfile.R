#' Get profile between two points of the CTScan image matrix
#'
#' @param im Density matrix
#' @param x Values of X for points 1 and 2 (x1, x2)
#' @param y Values of y for points 1 and 2 (y1, y2)
#' @param r Profile width
#' @param k Rolling window width, integer
#' @param threshold Threshold value between maximum and minimum density to establish change of ring
#'
#' @return
#'
#' @export
#' @import xRing
#'
#' @examples
extractProfile <- function(im,
                           imHeader,
                           beginPath,
                           endPath,
                           r = 10,
                           k = 2,
                           threshold = 0.01) {
  x <- c(beginPath[1], endPath[1])
  y <- c(beginPath[2], endPath[2])
  rings <- ringLimits(im, x, y, r, k, threshold, imHeader)
  t1 <- list(distRingChange = rings$distFromPith[rings$ring_limits])
  rings <- append(rings, t1)
  rings <- removeDuplicates(rings)

  return(rings)
}
