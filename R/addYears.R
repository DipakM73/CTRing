#' Add years to series
#'
#' @param lastYear Last year of series
#' @param densProfile Density profile
#'
#' @return Density profile with years
#' @export
#'
#' @examples
addYears <- function(lastYear, densProfile) {
  densProfile$years <- seq(from = lastYear-length(densProfile$ring_limits),
                           to = lastYear)
  return(densProfile)
}
