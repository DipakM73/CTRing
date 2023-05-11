#' Extract from header of CT scan image grayscale number of bits and pixel size
#'
#' @param hdr Header dataframe
#'
#' @return List with grayscale values, and pixel size
#' @export
#'
#' @examples
getImageInfo <- function(hdr) {
  #gray scale of CT scan
  grayScale <- as.numeric(hdr$value[hdr$name == "BitsStored"])
  size <- hdr$value[hdr$name == "PixelSpacing"]

  # pixel size
  size <- as.numeric(unlist(strsplit(size, " +")))
  pixel_size_x <- size[1]
  pixel_size_y <- size[2]

  out <- list(grayScale = grayScale,
              pixel_size_x = pixel_size_x,
              pixel_size_y = pixel_size_y)
}
