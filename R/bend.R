#' Deform
#'
#' Compute the deformation by applying both bending and torsion
#'
#' @param data Point data.frame (see details).
#' @param step Length of the segments that discretize the object (m).
#' @param points Number of points used in the grid discretizing the section.
#' @param iterations Number of iterations to compute the torsion and bending
#' @param verbose Boolean. Return information during procress?
#'
#' @details The data argument is a [data.frame()] with each row being a point and each column being:
#' - x: x coordinate
#' - y: y coordinate
#' - z: z coordinate
#' - type: section type. 1: triangle (bottom-oriented); 2 : rectangle; 3 : triangle (top-oriented);
#' 4 : ellipsis; 5 : circle.
#' - width (m): section width
#' - height (m): section height
#' - mass (kg): mass of the section at the point
#' - mass_right (kg): mass carried by the object, on the right side
#' - mass_left (kg): mass carried by the object, on the left side
#' - elastic_modulus (MPa): elasticity modulus (bending)
#' - shear_modulus (MPa): shear modulus (torsion)
#' - inclination (degree): insertion angle of the first section (only first value is used)
#' - distance_application (m): application distances for the left and right weights
#'
#' @return A [data.frame()]:
#' - x (m): X coordinate
#' - y (m): Y coordinate
#' - z (m): Z coordinate
#' - length (m): length of the section
#' - angle_xy (degree): angle of the segment with the XY plane
#' - angle_xz (degree): angle of the segment with the XZ plane
#' - torsion (degree): rotation angle of the segment
#'
#' @export
#'
#' @examples
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' # Un-bending the field measurements:
#' df = unbend(2000,400, read_mat(filepath))
#'
#' # (Re-)computing the deformation:
#' matRes = bend(df, step = 0.02, points = 100, iterations = 15, verbose = TRUE)
bend = function(data, step = 0.02, points = 100, iterations = 15, verbose = TRUE){

  vecRotFlex = matrix(0, ncol = 1, nrow = 3)

  # Nombre de points experimentaux
  NpointsExp = nrow(data)

  # Distance et angles de chaque segment P2P1
  vX = data$x
  vY = data$y
  vZ = data$z

  # vDist_P2P1 : longueur de chaque segment
  # vAngle_XY : angle du segment avec le plan XY (radian)
  # vAngle_XZ : angle du segment avec le plan XZ (radian)
  XYZangles = XYZ_Vers_Agl(vX, vY, vZ)

  vDist_P2P1 = XYZangles$dist_P2P1
  vAngle_XY  = XYZangles$vAngle_XY
  vAngle_XZ  = XYZangles$vAngle_XZ

  distLineique = cumsum(vDist_P2P1) # Pour interpolation
  distTotale = utils::tail(distLineique,1)

  # Les distances des segments ne peuvent pas etre nulles
  # Le point origine (0,0,0) ne peut etre dans data
  if(any(vDist_P2P1 == 0)) stop('Found distances between segments equal to 0.')

  # Poids lineique des segments
  poidsTige = data$mass
  poidsFeuilD = data$mass_right
  poidsFeuilG = data$mass_left

  poidsLinTige = poidsTige / vDist_P2P1
  poidsLinFeuilD = poidsFeuilD / vDist_P2P1
  poidsLinFeuilG = poidsFeuilG / vDist_P2P1

  vPoidsFlexion = poidsLinTige + poidsLinFeuilD + poidsLinFeuilG
  vPoidsFeuillesD = poidsLinFeuilD
  vPoidsFeuillesG = poidsLinFeuilG

  # Interpolations lineaires
  # Discretisation lineique
  # Logueur d'un segment = step
  Nlin = round(distTotale / step + 1)
  step = distTotale / (Nlin - 1)
  vecDist = (0 : (Nlin - 1)) * step

  vMOE = data$elastic_modulus
  vecMOE = stats::approx(c(0,distLineique), c(vMOE[1], vMOE), vecDist, method = 'linear')$y

  vG = data$shear_modulus
  vecG = stats::approx(c(0,distLineique), c(vG[1], vG), vecDist, method = 'linear')$y

  vAngle_Tor = data$inclination * pi / 180 # (radian)
  vecAglTor = stats::approx(c(0, distLineique), c(vAngle_Tor[1], vAngle_Tor), vecDist, method = 'linear')$y

  vDAppliPoidsFeuil = data$distance_application
  vecDAppliPoidsFeuil = stats::approx(c(0, distLineique), c(vDAppliPoidsFeuil[1], vDAppliPoidsFeuil),
                                      vecDist, method = 'linear')$y

  # Interpolation des coordonnees dans le repere origine
  # Identification des points experimentaux dans la discretisation lineique
  interp_list = InterpPoints(data, step)

  vecX = interp_list$vecX
  vecY = interp_list$vecY
  vecZ = interp_list$vecZ
  iDiscretPtsExp = interp_list$iDiscretPtsExp
  vecDist_P2P1 = interp_list$vecDist_P2P1
  vecAngle_XY = interp_list$vecAngle_XY
  vecAngle_XZ = interp_list$vecAngle_XZ

  valEpsilon = 1e-6
  if((vecDist_P2P1[2] > (step + valEpsilon)) | (vecDist_P2P1[2] < (step - valEpsilon))) stop('Point distance too narrow')
  if((length(vecX) != Nlin)) stop('length(vecX) != Nlin')

  oriX = vecX
  oriY = vecY
  oriZ = vecZ

  oriAglXY = vecAngle_XY
  oriAglXZ = vecAngle_XZ
  oriTor = vecAglTor

  # Increment de poids pour le calcul iteratif ===
  vecMOE = vecMOE * 1e6
  vecG = vecG * 1e6

  matDistPtsExp = matrix(0, nrow = iterations, ncol = NpointsExp)

  vPoidsFlexion = vPoidsFlexion / iterations
  vPoidsFeuillesD = vPoidsFeuillesD / iterations
  vPoidsFeuillesG = vPoidsFeuillesG / iterations

  SomCum_vecAglTor = oriTor # rotation geometrique et de la section

  for(iterPoids in 1 : iterations){

    # Inerties et surfaces des points experimentaux
    vIgFlex = vIgTor = vSr = rep(0, NpointsExp)

    for(iter in 1 : NpointsExp){
      b = data$width[iter]
      h = data$height[iter]
      sct = data$type[iter]
      agDeg = SomCum_vecAglTor[iDiscretPtsExp[iter]] * 180 / pi # orientation section (degres)

      InertiaFlexRot = InertieFlexRota(b, h, agDeg, sct, points)
      IgFlex = InertiaFlexRot$IgFlex
      IgTor = InertiaFlexRot$IgTor
      Sr = InertiaFlexRot$Sr

      vIgFlex[iter] = IgFlex
      vIgTor[iter] = IgTor
      vSr[iter] = Sr
    }

    # Interpolation lineique des inerties
    vecInertieFlex = stats::approx(c(0,distLineique), c(vIgFlex[1], vIgFlex), vecDist, method = 'linear')$y
    vecInertieTor = stats::approx(c(0,distLineique), c(vIgTor[1], vIgTor), vecDist, method = 'linear')$y

    # Ecriture des angles a partir des nouvelles coordonnees
    # Distance et angles de chaque segment P2P1
    XYZangles = XYZ_Vers_Agl(vecX, vecY, vecZ)

    vecDist_P2P1 = XYZangles$dist_P2P1
    vecAngle_XY  = XYZangles$vAngle_XY
    vecAngle_XZ  = XYZangles$vAngle_XZ


    vecDist_P2P1[1] = 0
    vecAngle_XY[1] = vecAngle_XY[2]
    vecAngle_XZ[1] = vecAngle_XZ[2]

    # Flexion
    # Forces lineiques de flexion
    # et Interpolation lineique
    pesanteur = 9.8

    # Force lineique
    # Code avec invariance par 'iterations'
    vForce = vPoidsFlexion * cos(vecAngle_XY[iDiscretPtsExp]) * pesanteur

    vecForce = stats::approx(c(0,distLineique), c(vForce[1], vForce), vecDist, method = 'linear')$y

    # Efforts tranchants et moments de flexion
    vecTranchant = cumsum(vecForce[seq(Nlin,1,-1)] * step)
    vecTranchant = vecTranchant[seq(Nlin,1,-1)]

    vecMoment = -cumsum(vecTranchant[seq(Nlin,1,-1)] * step)
    vecMoment = vecMoment[seq(Nlin,1,-1)]

    # Calcul classique de la deformee (delta de distance)
    fct = vecMoment / (vecMOE * vecInertieFlex)

    vecInteg = rep(0, Nlin)
    vecInteg = cumsum(fct[seq(Nlin,1,-1)] * step)
    vecInteg = vecInteg[seq(Nlin,1,-1)]

    # Condition encastree (derivee 1 = 0)
    vecInteg = vecInteg[1] - vecInteg
    vecAngleFlexion = vecInteg

    # Test de l'hypothese des petits deplacements
    AngleMax = 21 * pi / 180 # 21 degrees is the limit

    if(verbose && max(abs(vecAngleFlexion)) > AngleMax){
      cat('Maximum bending angle (degree) = ', max(abs(vecAngleFlexion)) * 180 / pi)
      cat("(!) Hypothesis of small displacements not verified for BENDING (!)")
    }

    # Torsion
    pesanteur = 9.8
    vMTor = rep(0, NpointsExp)

    for(iter in 1 : NpointsExp){
      FDr = matrix(c(0, 0, -vPoidsFeuillesD[iter] * vDist_P2P1[iter] * pesanteur), ncol = 1)
      # Code avec invariance par 'iterations'
      ForceFeuilDr = Rota_Inverse_YZ(FDr, vecAngle_XY[iter], vecAngle_XZ[iter])

      FGa = matrix(c(0,0,-vPoidsFeuillesG[iter] * vDist_P2P1[iter] * pesanteur), nrow = 3, byrow = TRUE)
      # Code avec invariance par 'iterations'
      # ForceFeuilGa = Rota_Inverse_YZ(FGa, oriAglXY[iter], oriAglXZ[iter])
      ForceFeuilGa = Rota_Inverse_YZ(FGa, vecAngle_XY[iter], vecAngle_XZ[iter])

      DistPoint = vecDAppliPoidsFeuil[iDiscretPtsExp[iter]]
      AnglePoint = SomCum_vecAglTor[iDiscretPtsExp[iter]]

      # Hypothese de contribution partie D ou G
      if(AnglePoint > 0){
        kD = 0
        kG = 1
      }else if(AnglePoint < 0){
        kD = 1
        kG = 0
      }else if(AnglePoint == 0){
        kD = 0
        kG = 0
      }

      MD = DistPoint * kD * cos(AnglePoint) * ForceFeuilDr[3]
      MG = DistPoint * kG * cos(AnglePoint + pi) * ForceFeuilGa[3]

      vMTor[iter] = MD + MG
    }

    vecMTor = stats::approx(c(0,distLineique), c(vMTor[1], vMTor), vecDist, method = 'linear')$y

    vecDerivAglTor = vecMTor / (vecG * vecInertieTor)

    vecAngleTorsion = cumsum(vecDerivAglTor * step) # integration le long de la tige

    if(max(abs(vecAngleTorsion)) > AngleMax){
      cat('Maximum torsion angle (degree) = ', max(abs(vecAngleTorsion)) * 180 / pi)
      cat("(!) Hypothesis of small displacements not verified for TORSION(!)")
    }

    SomCum_vecAglTor = SomCum_vecAglTor + vecAngleTorsion # cumul par increment de poids

    if(verbose && iterPoids == iterations){
      cat(' ')
      cat('Final torsion angle at the tip (degree) = ',
          SomCum_vecAglTor[length(SomCum_vecAglTor)] * 180 / pi)
    }

    # Nouvelles coordonnees des points
    neoVecX = neoVecY = neoVecZ = rep(0, Nlin)

    for(iter in 1 : Nlin){
      # Origine P1
      P2 = matrix(c(vecX[iter], vecY[iter], vecZ[iter]), nrow = 3, byrow = TRUE)

      if (iter == 1){
        P1 = matrix(rep(0,3), ncol = 1)
      }else{
        P1 = matrix(c(vecX[iter-1], vecY[iter-1], vecZ[iter-1]), nrow = 3, byrow = TRUE)
      }

      P2P1 = P2 - P1

      # Changement de base
      # Segment devient colineaire a l'axe OX
      vecRotInv = Rota_Inverse_YZ(OP = P2P1, Agl_Y = vecAngle_XY[iter],
                                  Agl_Z = vecAngle_XZ[iter])

      # Flexion
      # Equivalent a une rotation autour de OY
      # Rotation autour de OY
      # La rotation est fausse pour les forts angles
      # Code remplace par :
      vecRotFlex[1,1] = vecRotInv[1]
      vecRotFlex[2,1] = vecRotInv[2]
      vecRotFlex[3,1] = step * vecAngleFlexion[iter]

      # Torsion
      # Equivalent a une rotation autour de OX
      # Initialement la section est tournee mais sans torsion
      aglTorGeom = SomCum_vecAglTor[iter] - oriTor[iter]

      cs = cos(aglTorGeom)
      sn = sin(aglTorGeom)

      matRotX =	matrix(c(1,0,0,0,cs,-sn,0,sn,cs), nrow = 3, byrow = TRUE)

      vecRotTor = matRotX %*% vecRotFlex

      # Base d'origine
      vecRot = Rota_YZ(vecRotTor, vecAngle_XY[iter], vecAngle_XZ[iter])

      # Point dans la base origine
      if(iter == 1){
        neoX = vecRot[1]
        neoY = vecRot[2]
        neoZ = vecRot[3]
      }else{
        neoX = neoVecX[iter - 1] + vecRot[1]
        neoY = neoVecY[iter - 1] + vecRot[2]
        neoZ = neoVecZ[iter - 1] + vecRot[3]
      }

      # Re-ecriture des points
      neoVecX[iter] = neoX
      neoVecY[iter] = neoY
      neoVecZ[iter] = neoZ

    }

    # Mise a jour des variables
    vecX = neoVecX
    vecY = neoVecY
    vecZ = neoVecZ

    # Conservation des distances
    # step = distance entre les points
    XYZangles = XYZ_Vers_Agl(vecX, vecY, vecZ)

    coords = Agl_Vers_XYZ(c(0,rep(step,Nlin-1)), XYZangles$vAngle_XY, XYZangles$vAngle_XZ)

    vecX = coords$vecX
    vecY = coords$vecY
    vecZ = coords$vecZ

    # Calcul des distances des points experimentaux
    # Entre avant et apres deformation
    for(iter in 1 : NpointsExp){
      c1 = (vX[iter] - vecX[iDiscretPtsExp[iter]])^2
      c2 = (vY[iter] - vecY[iDiscretPtsExp[iter]])^2
      c3 = (vZ[iter] - vecZ[iDiscretPtsExp[iter]])^2

      matDistPtsExp[iterPoids, iter] = sqrt(c1 + c2 + c3)
    }
  } # iterPoids

  PtsX = vecX[iDiscretPtsExp]
  PtsY = vecY[iDiscretPtsExp]
  PtsZ = vecZ[iDiscretPtsExp]

  Points = XYZ_Vers_Agl(PtsX, PtsY, PtsZ)

  PtsDist = Points$dist_P2P1
  PtsAglXY = Points$vAngle_XY
  PtsAglXZ = Points$vAngle_XZ

  PtsAglXY = PtsAglXY * 180 / pi
  PtsAglXZ = PtsAglXZ * 180 / pi
  PtsAglTor = SomCum_vecAglTor[iDiscretPtsExp] * 180 / pi

  data.frame(x = PtsX, y = PtsY, z = PtsZ, length = PtsDist,
             angle_xy = PtsAglXY, angle_xz = PtsAglXZ,
             torsion = PtsAglTor)
}
