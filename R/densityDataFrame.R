#' Convert to dataframe
#'
#' @param densProfile Density profile
#' @param sampleID Sample ID
#'
#' @return Dataframe with cambial age, density, years
#' @export
#'
#' @examples
densityDataFrame <- function(densProfile, sampleID = "NoID") {

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

    return(out)
  } else {
    Print("Average density needs to be calculated before generating dataframe")
  }
}
