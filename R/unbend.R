#' Unbend
#'
#' Remove torsion and bending of a bent beam, i.e. transform it
#' into a straight line, while keeping its insertion angle (inclination angle of the first segment).
#'
#' @note Mainly used to compute the matrix for input to [bend()] from experimental points.
#'
#' @param df Experimental data (usually read using [read_mat()], see details)
#'
#' @details df should be a formatted [data.frame()], with each row being a point and each columns being:
#' - distance (m): distance between the previous point and this point (first value should be positive)
#' - inclination (degree): insertion angle of the first point (only first value is used)
#'
#' @return The input [data.frame()] with modified (or enriched with):
#' - x: x coordinate
#' - y: y coordinate
#' - z: z coordinate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' df = read_mat(filepath)
#' unbend(df = df)
#' }
unbend = function(df){

  # The distance between points can't be 0
  iD0 = which(df$distance == 0)
  if(length(iD0)>0){
    df$distance[iD0] = 1e-3 # (m)
  }

  Distance = df$distance

  # cumulated distance of each segment:
  XDistance = cumsum(unlist(Distance))

  # keeping the inclination of the first segment (i.e. insertion angle):
  Agl_Y = df$inclination[1] * pi / 180
  Agl_Z = 0

  # Initializing in case they are missing:
  df$x = df$y = df$z = 0

  # Coordinates of the points (un-bent state):
  for(iter in 1 : nrow(df)){
    OP = c(XDistance[[iter]], 0, 0)
    vecRot = Rota_YZ(OP, Agl_Y, Agl_Z)

    df$x[iter] = vecRot[1]
    df$y[iter] = vecRot[2]
    df$z[iter] = vecRot[3]
  }

  df
}


