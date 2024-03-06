#' Establish the transition point from earlywood to latewood for a series of rings
#'
#' @param xRingList A list of xRing profiles
#' @param dist A vector of distances of each point from pith center
#'
#' @return xRingList with EW to LW transition points with transition type added (1: low number of points in ring; 2: inflexion point estimated by polynomial; 3: min or max are out of range; 4: inflexion point close to min or max; 5: convex-concave)
#'
#' @export
#'
#' @examples
getEwLw <- function(densProfile) {

  # split to individual rings
  dist <- densProfile$distFromPith
  dens <- densProfile$dens
  cutPoints <- densProfile$ring_limits
  ringList <- splitAt(dens, cutPoints)
  distList <- splitAt(dist, cutPoints)

  ringDistList <- mapply(cbind, distList, ringList)

  ewList <- lapply(ringDistList, findEwToLwTransition)

  # transfer from intra-ring index to profile index
  ewDataFrame <- do.call(rbind.data.frame, ewList)
  names(ewDataFrame) <- names(ewList[[1]])

  ewDataFrame$totPoints <- c(0, cumsum(ewDataFrame$nPoints)[-length(ewDataFrame$nPoints)])
  ewDataFrame$limits.EW <- ewDataFrame$totPoints + floor(ewDataFrame$EW)
  # ewDataFrame$limits.LW <- ewDataFrame$totPoints + ceiling(ewDataFrame$EW)

  densProfile$ew_limits <- ewDataFrame$limits.EW[1:length(ewDataFrame$limits.EW)]
  # densProfile$limits.lw <- ewDataFrame$limits.LW[2:length(ewDataFrame$limits.LW)]

  densProfile$transitionType <- ewDataFrame$type

  return(densProfile)
}
