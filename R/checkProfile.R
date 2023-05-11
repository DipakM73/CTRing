#' Verify position of ring transitions of a density profile
#'
#' @param profile_with_borders xRing profile with transitions between rings located
#' @param totRings Total number of rings of the disk
#'
#' @return xRing profile with corrected ring location
#' @export
#'
#' @import xRing
#'
#' @examples
#' t2 <- checkProfile(t1, 30)
#'
checkProfile <- function(profile_with_borders, totRings) {
  # profile_with_borders <- t1
  # totRings <- nRings
  numRingsFound <- dim(profile_with_borders$trw)[1]
  deltaRingNumber <- totRings - numRingsFound

  # if rings are missing
  if (deltaRingNumber > 0 ){
    print(paste("Add ", deltaRingNumber, " rings to the profile"))

    x11()
    plot(profile_with_borders)

    profile_with_borders <- addRing(profile_with_borders, locator(deltaRingNumber)$x)
  }

  # if too many rings
  if (deltaRingNumber < 0 ){
    print(paste("Remove ", deltaRingNumber, " rings from profile"))

    x11()
    plot(profile_with_borders)

    profile_with_borders <- removeRing(profile_with_borders, locator(deltaRingNumber)$x)
  }

  print("Check profile")
  x11()
  plot(profile_with_borders)

  return(profile_with_borders)
}
