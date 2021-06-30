#' Rotate
#'
#' Rotate point around Y and (then) Z axes.
#'
#' @param OP A 3d point (x,y,z)
#' @param Agl_Y Rotation angle around Y (radian)
#' @param Agl_Z Rotation angle around Z (radian)
#'
#' @return The rotated point
#' @export
#'
#' @examples
#' Rota_YZ(c(2.681,0,0),0.85172,0)
Rota_YZ = function(OP, Agl_Y, Agl_Z){

  # Rotation around OY
  cs = cos(Agl_Y)
  sn = sin(Agl_Y)

  matRotY = matrix(c(cs,0,-sn,0,1,0,sn,0,cs), nrow = 3, byrow = TRUE)

  vecRotY = matRotY %*% OP

  # Rotation around OZ
  cs = cos(Agl_Z)
  sn = sin(Agl_Z)

  matRotZ =	matrix(c(cs,-sn,0,sn,cs,0,0,0,1), nrow = 3, byrow = TRUE)

  matRotZ %*% vecRotY
}
