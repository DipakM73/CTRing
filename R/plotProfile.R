#' Plot density profile
#'
#' @param densProfile Density profile
#'
#' @return Figure
#' @export
#'
#' @examples
plotProfile <- function(densProfile) {
  dev.new()
  plot(x = densProfile$distFromPith,
       y = densProfile$dens,
       type = 'l',
       xlab = "Distance from pith",
       ylab = "Density")
  abline(v = densProfile$distRingChange)

  if ("ew_limits" %in% names(densProfile)) {
    points(x = densProfile$distFromPith[densProfile$ew_limits],
           y = densProfile$dens[densProfile$ew_limits],
           pch = 10,
           col = 'blue')
  }
}
