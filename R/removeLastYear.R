#' Remove the last year of a profile
#'
#' @param densProfile Density profile
#'
#' @return Density profile with the last year removed
#' @export
#'
#' @examples
removeLastYear <- function(densProfile) {
  densProfile$xx <- densProfile$xx[-c(max(densProfile$ring_limits):length(densProfile$xx))]
  densProfile$yy <- densProfile$yy[-c(max(densProfile$ring_limits):length(densProfile$yy))]
  densProfile$dens <- densProfile$dens[-c(max(densProfile$ring_limits):length(densProfile$dens))]
  densProfile$distFromPith <- densProfile$distFromPith[-c(max(densProfile$ring_limits):length(densProfile$distFromPith))]
  densProfile$ring_limits <- densProfile$ring_limits[-length(densProfile$ring_limits)]
  densProfile$distRingChange <- densProfile$distRingChange[-length(densProfile$distRingChange)]
  densProfile$transitionType <- densProfile$transitionType[-length(densProfile$transitionType)]


  if ("ew_limits" %in% names(densProfile))
    densProfile$ew_limits <- densProfile$ew_limits[-length(densProfile$ew_limits)]
  if ("avgDens" %in% names(densProfile))
    densProfile$avgDens <- densProfile$avgDens[-length(densProfile$avgDens)]
  if ("avgDensEw" %in% names(densProfile))
    densProfile$avgDensEw <- densProfile$avgDensEw[-length(densProfile$avgDensEw)]
  if ("avgDensLw" %in% names(densProfile))
    densProfile$avgDensLw <- densProfile$avgDensLw[-length(densProfile$avgDensLw)]
  if ("years" %in% names(densProfile))
    densProfile$years <- densProfile$years[-length(densProfile$years)]

  return(densProfile)
}
