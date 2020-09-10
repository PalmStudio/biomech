#' Point interpolation
#'
#' @param matPoints Point matrix (from [CreaPoints()])
#' @param pas       Distance needed between interpolated points (m)
#'
#' @return A list.
#' @export
#'
#' @examples
InterpPoints = function(matPoints, pas){
  # Identification des lignes de la matrice
  iX = 1
  iY = 2
  iZ = 3

  # Distance et angles de chaque segment P2P1
  vX = matPoints[iX,]
  vY = matPoints[iY,]
  vZ = matPoints[iZ,]

  # vDist_P2P1 : longueur de chaque segment
  # vAngle_XY : angle du segment avec le plan XY (radian)
  # vAngle_XZ : angle du segment avec le plan XZ (radian)
  XYZ_Agl = XYZ_Vers_Agl(vX, vY, vZ)
  vDist_P2P1 = XYZ_Agl$dist_P2P1
  vAngle_XY = XYZ_Agl$vAngle_XY
  vAngle_XZ = XYZ_Agl$vAngle_XZ

  distLineique = cumsum(vDist_P2P1)
  distTotale = tail(distLineique,1)

  # Les distances des segments ne peuvent pas etre nulles
  # Le point origine (0,0,0) ne peut etre dans matPoints
  if(any(vDist_P2P1 == 0)) stop('Found distances between segments equal to 0.')

  # Construction des points interpoles
  Nlin = round(distTotale / pas + 1)
  pas = distTotale / (Nlin - 1)

  NpointsExp = ncol(matPoints)

  vecDist = c(0, rep(pas, (Nlin - 1)))
  distInterp = cumsum(vecDist)

  # Evite les erreurs d'arrondi
  distLineique[NpointsExp] = distLineique[NpointsExp] + 1

  for (iter in 1 : NpointsExp){
    if (iter == 1){
      indPoints = (distInterp <= distLineique[iter])
    }else{
      indPoints = (distInterp > distLineique[iter - 1]) & (distInterp <= distLineique[iter])
    }

    if(!any(indPoints)) stop('No point found')

    distPoints = vecDist[indPoints]
    OP = matrix(c(distPoints,rep(0, length(distPoints)),rep(0, length(distPoints))), nrow = 3, byrow = TRUE)
    vecRot = Rota_YZ(OP, vAngle_XY[iter], vAngle_XZ[iter])

    if (iter > 1){
      vecPoints = t(apply(vecRot, 1, cumsum)) + matXYZ[,ncol(matXYZ)]
      matXYZ = cbind(matXYZ, vecPoints)
    }else{
      matXYZ = matrix(apply(vecRot, 1, cumsum), nrow = 3, byrow = TRUE)
    }
  }

  vecX = matXYZ[1,]
  vecY = matXYZ[2,]
  vecZ = matXYZ[3,]

  #===============================================================================
  # Identification des points experimentaux
  # dans la discretisation lineique
  iDiscretPtsExp = rep(0,NpointsExp)

  for(iter in 1 : NpointsExp){
    equad = sqrt((vecX - vX[iter])^2 + (vecY - vY[iter])^2 + (vecZ - vZ[iter])^2)
    ind = which(equad == min(equad))
    iDiscretPtsExp[iter] = ind[1]
  }

  #===============================================================================
  # Distance et angles des points interpoles
  XYZ_Agl = XYZ_Vers_Agl(vecX, vecY, vecZ)
  vecDist_P2P1 = XYZ_Agl$dist_P2P1
  vecAngle_XY = XYZ_Agl$vAngle_XY
  vecAngle_XZ = XYZ_Agl$vAngle_XZ

  list(vecX = vecX, vecY = vecY, vecZ = vecZ, iDiscretPtsExp = iDiscretPtsExp, vecDist_P2P1 = vecDist_P2P1,
       vecAngle_XY = vecAngle_XY, vecAngle_XZ = vecAngle_XZ)
}

