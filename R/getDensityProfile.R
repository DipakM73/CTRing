#' Obtain pith to bark density profile
#'
#' @param theta.deg Angle of the path (0-360 deg)
#' @param pith_coord_units Coordinates of the pith in vector (x, y)
#' @param im_raster Raster image of the CT scan image
#' @param toPlot Boolean to determine if a plot of the path on the image is returned
#' @param density_cutoff Density value below which values are not considered
#'
#' @return Dataframe with profile values (distance from pith, density)
#'
#' @import terra
#'
#' @export
#'
#' @examples
#' # path_profile <- getDensityProfile(theta = 0,
#' #   pith_coord_units = pith_coord_units,
#' #   im_raster = im_raster,
#' #   toPlot = TRUE)
#'
getDensityProfile <- function(pith_coord_units, im_raster, theta.deg = 0,
                              toPlot = FALSE, density_cutoff = 0.1,
                              name = "t1") {
  # convert to rad
  theta.rad <- theta.deg * pi / 180

  # ending points of path to far out of image
  long_path <- c(x = 10000*cos(theta.rad),
                 y = 10000*sin(theta.rad))

  path_coord <- rbind(
    cbind(pith_coord_units["x"], pith_coord_units["y"]),
    cbind(long_path["x"], long_path["y"])
  )
  path_line <- vect(path_coord, type = "lines")

  # clip line to within raster boundaries
  path_line <- crop(path_line, im_raster)

  # plot
  if (toPlot == TRUE) {
    plot(im_raster)
    plot(path_line, add = TRUE)
  }

  # extract profil
  path <- extract(im_raster, path_line, xy=TRUE)

  # delete values with low measurements
  path$lyr.1[path$lyr.1 < density_cutoff] <- NA

  # calculate distance from pith
  calcDistance <- function(ref_point, interest_point) {
    dist <- sqrt((ref_point["x"] - interest_point$x)^2 + (ref_point["y"] - interest_point$y)^2)
    return(dist)
  }

  path$pith_distance <- calcDistance(pith_coord_units, path)
  path <- path[order(path$pith_distance),]

  colnames(path)[colnames(path) == "lyr.1"] = name

  return(path)
}
