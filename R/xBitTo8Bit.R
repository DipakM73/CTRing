#' Convert gray scale from measured bits to 8bit
#'
#' @param im Matrix of values in x bits
#' @param bits Number of bits of the original gray scale
#'
#' @return Matrix of gray scale values in 8bits
#' @export
#'
#' @examples
xBitTo8Bit <- function(im, bits) {
  nGray <- 2^bits
  im_8bit <- im/ (nGray/2^8)
  return(im_8bit)
}
