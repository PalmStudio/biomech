#' Title
#'
#' @param vecX X coordinates (m)
#' @param vecY Y coordinates (m)
#' @param vecZ Z coordinates (m)
#'
#' @return A list:
#' - dist_P2P1: Length of the segment;
#' - vAngle_XY: Angle between the segment and the XY plane (radian);
#' - vAngle_XZ: Angle between the segment and the XZ plane (radian)
#'
#' @export
#'
#' @examples
#' library(data.table)
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "deformation")
#' matExp = data.table::fread(filepath, data.table = FALSE)
#' matExp = CreaPoints(2000,400,matExp)
#' XYZ_Vers_Agl(matExp[1,],matExp[2,],matExp[3,])
#'
XYZ_Vers_Agl = function(vecX, vecY, vecZ){
  N = length(vecX)

  if(length(vecY) != N) stop('Length of Y coordinates not equal to X coordinates')
  if(length(vecZ) != N) stop('Length of Z coordinates not equal to X coordinates')

  dist_P2P1 = vAngle_XY = vAngle_XZ = rep(0,N)

  for(iter in 1 : N){
    P2 = matrix(c(vecX[iter], vecY[iter], vecZ[iter]), ncol = 1, byrow = TRUE)

    if(iter == 1){
      P1 = matrix(rep(0,3), ncol = 1, byrow = TRUE)
    }else{
      P1 = matrix(c(vecX[iter-1], vecY[iter-1], vecZ[iter-1]), ncol = 1, byrow = TRUE)
    }

    P2P1 = P2 - P1

    if(P2P1[1] != 0){
      # Distances
      dist_P2P1[iter] = sqrt(P2P1[1]^2 + P2P1[2]^2 + P2P1[3]^2)

      # Angles
      vAngle_XY[iter] = atan(P2P1[3] / sqrt(P2P1[1]^2 + P2P1[2]^2))
      vAngle_XZ[iter] = atan(P2P1[2] / P2P1[1])
    }
  }

  list(dist_P2P1 = dist_P2P1, vAngle_XY = vAngle_XY, vAngle_XZ = vAngle_XZ)
}
