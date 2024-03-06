#' Convert to dataframe
#'
#' @param densProfile Density profile
#' @param sampleID Sample ID
#' @param addTransitionType add transition type to dataframe
#'
#' @return Dataframe with cambial age, density, years, transition type
#' @export
#'
#' @examples
densityDataFrame <- function(densProfile, sampleID = "NoID", addTransitionType = FALSE) {

  if ("avgDens" %in% names(densProfile)){
    dens <- densProfile$avgDens
    ringAge <- c(1:length(dens))
    ID = rep(sampleID, length(dens))

    out <- data.frame(ID = ID,
                      cambialAge = ringAge,
                      avgDens = dens)

    if ("avgDensEw" %in% names(densProfile))
      out$avgDensEw <- densProfile$avgDensEw
    if ("avgDensLw" %in% names(densProfile))
      out$avgDensLw <- densProfile$avgDensLw
    if ("years" %in% names(densProfile))
      out$years <- densProfile$years
    if (addTransitionType == TRUE)
      out$transitionType = densProfile$transitionType

    return(out)
  } else {
    print("Average density needs to be calculated before generating dataframe")
  }
}
