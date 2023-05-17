#' Get coordinates of the end of the path on a CT scan image
#'
#' @param im CT scan image
#'
#' @return Coordinates of the end of the path
#' @export
#'
#' @examples
locatePathEnd <- function(im, pithCoord){
  dev.new()
  image(im)
  points(x =  pith_coord["x"]/dim(im)[1], y =  pith_coord["y"]/dim(im)[2], pch = 19)

  coord <- locator(1)
  coord$x = coord$x * dim(im)[1]
  coord$y = coord$y * dim(im)[2]

  out <- c(x = coord$x, y = coord$y)
  return(out)
}
