#' Convert dicom image to matrix
#'
#' @param img Dicom image
#'
#' @return Matrix of image
#' @export
#'
#' @examples
imageToMatrix <- function(img){
  matImage <- lst2arr(img)[,,1]
  return(matImage)
}


