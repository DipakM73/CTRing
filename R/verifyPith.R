#' Check if pith location is correct
#'
#' @param im Density matrix of image
#' @param pith_coord Pith coordinates
#'
#' @return Corrected pith coordinates
#' @export
#'
#' @examples
verifyPith <- function(im, pith_coord) {
  dev.new()
  image(im)
  abline(v = pith_coord["x"], col = 'blue')
  abline(h = pith_coord["y"], col = 'blue')

  print("Is pith correctly located (y/n)? ")
  correct <- readline()

  if (correct == "n"){
    print("Please click on pith")
    pith <- locator(1)
    out <- c(x = pith$x, y = pith$y)
    return(out)
  } else if (correct == "y") {
    return(pith_coord)
  } else {
    print("Incorrect answer. No changes to pith coordinates")
    return(pith_coord)
  }
}
