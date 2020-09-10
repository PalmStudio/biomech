#' Inverse rotation
#'
#' Rotate around the Z axis and then the Y axis
#'
#' @param OP x, y, z coordinates
#' @param Agl_Y Rotation angle around Y (radian)
#' @param Agl_Z Rotation angle around Z (radian)
#'
#' @return The rotated point
#' @export
#'
#' @examples
#' Rota_Inverse_YZ(matrix(rep(0,3), ncol = 1), 0.8517207, 0)
Rota_Inverse_YZ = function(OP, Agl_Y, Agl_Z){

  gl_Y = -Agl_Y
  Agl_Z = -Agl_Z

  # Rotation autour de OZ
  cs = cos(Agl_Z)
  sn = sin(Agl_Z)

  matRotZ = matrix(c(cs,-sn,0,sn,cs,0,0,0,1), ncol = 3, byrow = TRUE)

  vecRotZ = matRotZ %*% OP

  # Rotation autour de OY
  cs = cos(Agl_Y)
  sn = sin(Agl_Y)

  matRotY = matrix(c(cs,0,-sn,0,1,0,sn,0,cs), ncol = 3, byrow = TRUE)

  matRotY %*% vecRotZ
}
