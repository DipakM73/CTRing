#' Establish the transition point from earlywood to latewood for a series of rings
#'
#' @param xRingList A list of xRing profiles
#' @param dist A vector of distances of each point from pith center
#'
#' @return xRingList with EW to LW transition points
#'
getEwLw <- function(xRingList, dist) {

  # xRingList <- t2
  # dist <- path_profile_noNA$pith_distance

  # split to individual rings
  dens <- xRingList$profile.raw
  cutPoints <- xRingList$limits
  ringList <- splitAt(dens, cutPoints)
  distList <- splitAt(dist, cutPoints)

  ringDistList <- mapply(cbind, distList, ringList)

  # find index for EW to LW transition within each ring
  # ewList <- lapply(ringDistList[3], findEwToLwTransition)
  # for (i in 1:length(ringDistList)){
  #   print(i)
  #   test <- lapply(ringDistList[i], findEwToLwTransition)
  # }

  ewList <- lapply(ringDistList, findEwToLwTransition)

  # transfer from intra-ring index to profile index
  ewDataFrame <- do.call(rbind.data.frame, ewList)
  names(ewDataFrame) <- names(ewList[[1]])

  ewDataFrame$totPoints <- c(0, cumsum(ewDataFrame$nPoints)[-length(ewDataFrame$nPoints)])
  ewDataFrame$limits.EW <- ewDataFrame$totPoints + floor(ewDataFrame$EW)
  ewDataFrame$limits.LW <- ewDataFrame$totPoints + ceiling(ewDataFrame$EW)

  xRingList$limits.ew <- ewDataFrame$limits.EW[2:length(ewDataFrame$limits.EW)]
  xRingList$limits.lw <- ewDataFrame$limits.LW[2:length(ewDataFrame$limits.LW)]

  return(xRingList)
}
