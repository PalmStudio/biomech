#' Distances and angles to coordinates
#'
#' Transform distances and angles into point coordinates
#'
#' @param dist_P2P1 Segment length (m)
#' @param vAngle_XY Angle between the segment and the XY plane (radian)
#' @param vAngle_XZ Angle between the segment and the XZ plane (radian)
#'
#' @return Point coordinates
#' @export
#'
Agl_Vers_XYZ = function(dist_P2P1, vAngle_XY, vAngle_XZ){

  N = length(dist_P2P1)

  if(length(vAngle_XY) != N) stop('length of vAngle_XY != N')
  if(length(vAngle_XZ) != N) stop('length of vAngle_XZ != N')

  vecX = vecY = vecZ = rep(0,N)

  for(iter in 1 : N){
    dZ = dist_P2P1[iter] * sin(vAngle_XY[iter])
    dist_XY = dist_P2P1[iter] * cos(vAngle_XY[iter])

    dX = dist_XY * cos(vAngle_XZ[iter])
    dY = dist_XY * sin(vAngle_XZ[iter])

    if(iter == 1){
      vecX[iter] = dX
      vecY[iter] = dY
      vecZ[iter] = dZ
    }else{
      vecX[iter] = vecX[iter-1] + dX
      vecY[iter] = vecY[iter-1] + dY
      vecZ[iter] = vecZ[iter-1] + dZ
    }
  }
  list(vecX = vecX, vecY = vecY, vecZ = vecZ)
}

