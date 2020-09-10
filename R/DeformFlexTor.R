#' Deform
#'
#' Compute the deformation by applying both bending and torsion
#'
#' @return A matrix: [PtsX; PtsY; PtsZ; PtsDist; PtsAglXY; PtsAglXZ; PtsAglTor]
#' @export
#'
#' @examples
#' library(data.table)
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "deformation")
#' matExp = data.table::fread(filepath, data.table = FALSE)
#' matPoints = CreaPoints(2000,400,matExp)
#' matPoints is the matrix of points for a straight rachis. So it reads
#' measured information that is already bent, and put it back to a straight line.
#' pas= 0.02 # in meter. -> Length of the segments that discretize the object.
#' Ncalc= 100 # number of points used in the grid that discretized the section.
#' Nboucle= 15 # if we want to compute the torsion after the bending step by step instead of
#' all bending and then torsion.
#' matRes = DeformFlexTor(matPoints, pas, Ncalc, Nboucle, 1); # Computes the deformation -> this is the actual model
DeformFlexTor = function(matPoints){

  # Constantes de calcul
  # Nlin est calcule selon le pas
  pas = 0.02 # distance entre points interpoles de 'pas' en (m)
  # Discretisation pour le calcul des inerties et surfaces
  Ncalc = 100
  # Iterations de chargement pour le calcul
  Nboucle = 15

  # Identification des lignes de la matrice
  iX = 1
  iY = 2
  iZ = 3
  iTypeSection = 4
  iBase = 5
  iHauteur = 6
  iPoidsTige = 7
  iPoidsFeuillesDroite = 8
  iPoidsFeuillesGauche = 9
  iModuleElasticite = 10
  iModuleCisaillement = 11
  iAngleSection = 12
  iDAppliPoidsFeuil = 13

  # Nombre de points experimentaux
  NpointsExp = ncol(matPoints)

  # Distance et angles de chaque segment P2P1 ===
  vX = matPoints[iX,]
  vY = matPoints[iY,]
  vZ = matPoints[iZ,]

  # vDist_P2P1 : longueur de chaque segment
  # vAngle_XY : angle du segment avec le plan XY (radian)
  # vAngle_XZ : angle du segment avec le plan XZ (radian)
  XYZangles = XYZ_Vers_Agl(vX, vY, vZ)

  vDist_P2P1 = XYZangles$dist_P2P1
  vAngle_XY  = XYZangles$vAngle_XY
  vAngle_XZ  = XYZangles$vAngle_XZ

  distLineique = cumsum(vDist_P2P1) # Pour interpolation
  distTotale = tail(distLineique,1)

  # Les distances des segments ne peuvent pas etre nulles
  # Le point origine (0,0,0) ne peut etre dans matPoints
  if(any(vDist_P2P1 == 0)) stop('Found distances between segments equal to 0.')

  # Poids lineique des segments
  poidsTige = matPoints[iPoidsTige,]
  poidsFeuilD = matPoints[iPoidsFeuillesDroite,]
  poidsFeuilG = matPoints[iPoidsFeuillesGauche,]

  poidsLinTige = poidsTige / vDist_P2P1
  poidsLinFeuilD = poidsFeuilD / vDist_P2P1
  poidsLinFeuilG = poidsFeuilG / vDist_P2P1

  vPoidsFlexion = poidsLinTige + poidsLinFeuilD + poidsLinFeuilG
  vPoidsFeuillesD = poidsLinFeuilD
  vPoidsFeuillesG = poidsLinFeuilG

  # Interpolations lineaires
  # Discretisation lineique
  # Logueur d'un segment = pas
  Nlin = round(distTotale / pas + 1)
  pas = distTotale / (Nlin - 1)
  vecDist = (0 : (Nlin - 1)) * pas

  vMOE = matPoints[iModuleElasticite,]
  vecMOE = approx(c(0,distLineique), c(vMOE[1], vMOE), vecDist, method = 'linear')$y

  vG = matPoints[iModuleCisaillement,]
  vecG = approx(c(0,distLineique), c(vG[1], vG), vecDist, method = 'linear')$y

  vAngle_Tor = matPoints[iAngleSection,] * pi / 180 # (radian)
  vecAglTor = approx(c(0, distLineique), c(vAngle_Tor[1], vAngle_Tor), vecDist, method = 'linear')$y

  vDAppliPoidsFeuil = matPoints[iDAppliPoidsFeuil,]
  vecDAppliPoidsFeuil = approx(c(0, distLineique), c(vDAppliPoidsFeuil[1], vDAppliPoidsFeuil), vecDist, method = 'linear')

  # Interpolation des coordonnees dans le repere origine
  # Identification des points experimentaux dans la discretisation lineique
  interp_list = InterpPoints(matPoints, pas)

  vecX = interp_list$vecX
  vecY = interp_list$vecY
  vecZ = interp_list$vecZ
  iDiscretPtsExp = interp_list$iDiscretPtsExp
  vecDist_P2P1 = interp_list$vecDist_P2P1
  vecAngle_XY = interp_list$vecAngle_XY
  vecAngle_XZ = interp_list$vecAngle_XZ

  valEpsilon = 1e-6
  if((vecDist_P2P1[2] > (pas + valEpsilon)) | (vecDist_P2P1[2] < (pas - valEpsilon))) stop('Point distance too narrow')
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

  matDistPtsExp = matrix(0, nrow = Nboucle, ncol = NpointsExp)

  vPoidsFlexion = vPoidsFlexion / Nboucle
  vPoidsFeuillesD = vPoidsFeuillesD / Nboucle
  vPoidsFeuillesG = vPoidsFeuillesG / Nboucle

  SomCum_vecAglTor = oriTor # rotation geometrique et de la section

  for(iterPoids in 1 : Nboucle){

    # Inerties et surfaces des points experimentaux
    vIgFlex = vIgTor = vSr = rep(0, NpointsExp)

    for(iter in 1 : NpointsExp){
      b = matPoints[iBase, iter]
      h = matPoints[iHauteur, iter]
      sct = matPoints[iTypeSection, iter]
      agDeg = SomCum_vecAglTor[iDiscretPtsExp[iter]] * 180 / pi # orientation section (degres)

      ########################################################################################
      # Continuer ici
      [IgFlex, IgTor, Sr] = InertieFlexRota(b, h, agDeg, sct, Ncalc, 0)
      ########################################################################################

      vIgFlex(iter) = IgFlex
      vIgTor(iter) = IgTor
      vSr(iter) = Sr
    }

    # Interpolation lineique des inerties
    vecInertieFlex = interp1([0 distLineique], [vIgFlex(1) vIgFlex], vecDist, 'linear')
    vecInertieTor = interp1([0 distLineique], [vIgTor(1) vIgTor], vecDist, 'linear')

    # Ecriture des angles a partir des nouvelles coordonnees
    # Distance et angles de chaque segment P2P1
    [vecDist_P2P1, vecAngle_XY, vecAngle_XZ] = XYZ_Vers_Agl(vecX, vecY, vecZ)

    vecDist_P2P1(1) = 0
    vecAngle_XY(1) = vecAngle_XY(2)
    vecAngle_XZ(1) = vecAngle_XZ(2)

    # Flexion
    # Forces lineiques de flexion
    # et Interpolation lineique
    pesanteur = 9.8

    # Force lineique
    # Code avec invariance par 'Nboucle'
    # vForce = vPoidsFlexion .* cos(oriAglXY(iDiscretPtsExp)) .* pesanteur
    vForce = vPoidsFlexion .* cos(vecAngle_XY(iDiscretPtsExp)) .* pesanteur

    vecForce = interp1([0 distLineique], [vForce(1) vForce], vecDist, 'linear')

    # Efforts tranchants et moments de flexion
    vecTranchant = cumsum(vecForce(Nlin : -1 : 1) .* pas)
    vecTranchant = vecTranchant(Nlin : -1 : 1)

    vecMoment = -cumsum(vecTranchant(Nlin : -1 : 1) .* pas)
    vecMoment = vecMoment(Nlin : -1 : 1)

    # Calcul classique de la deformee (delta de distance)
    fct = vecMoment ./ (vecMOE .* vecInertieFlex)

    vecInteg = zeros(1, Nlin)
    vecInteg = cumsum(fct(Nlin : -1 : 1).*pas)
    vecInteg = vecInteg(Nlin : -1 : 1)

    # Condition encastree (derivee 1 = 0)
    vecInteg = vecInteg(1) - vecInteg
    vecAngleFlexion = vecInteg

    # Test de l'hypothese des petits deplacements
    AngleMax = 21 .* pi ./ 180 # 21° est la limite

    if (max(abs(vecAngleFlexion)) > AngleMax)
      disp(['Angle de flexion maximum (degres) = ' num2str(max(abs(vecAngleFlexion)) .* 180 ./ pi)])
    disp("(!) hypothese des petits deplacements non verifiee en FLEXION (!)")
    end

    # Torsion
    pesanteur = 9.8
    vMTor = zeros(1, NpointsExp)

    for iter = 1 : NpointsExp
    FDr = [0; 0; -vPoidsFeuillesD(iter) .* vDist_P2P1(iter) .* pesanteur]
    # Code avec invariance par 'Nboucle'
    # ForceFeuilDr = Rota_Inverse_YZ(FDr, oriAglXY(iter), oriAglXZ(iter))
    ForceFeuilDr = Rota_Inverse_YZ(FDr, vecAngle_XY(iter), vecAngle_XZ(iter))

    FGa = [0; 0; -vPoidsFeuillesG(iter) .* vDist_P2P1(iter) .* pesanteur]
    # Code avec invariance par 'Nboucle'
    # ForceFeuilGa = Rota_Inverse_YZ(FGa, oriAglXY(iter), oriAglXZ(iter))
    ForceFeuilGa = Rota_Inverse_YZ(FGa, vecAngle_XY(iter), vecAngle_XZ(iter))

    DistPoint = vecDAppliPoidsFeuil(iDiscretPtsExp(iter))
    AnglePoint = SomCum_vecAglTor(iDiscretPtsExp(iter))

    # Hypothese de contribution partie D ou G
    if (AnglePoint > 0), kD = 0; kG = 1;
    elseif (AnglePoint < 0), kD = 1; kG = 0;
    elseif (AnglePoint == 0), kD = 0; kG = 0; end

    MD = DistPoint .* kD .* cos(AnglePoint) .* ForceFeuilDr(3)
    MG = DistPoint .* kG .* cos(AnglePoint + pi) .* ForceFeuilGa(3)

    vMTor(iter) = MD + MG
    end

    vecMTor = interp1([0 distLineique], [vMTor(1) vMTor], vecDist, 'linear')
    vecDerivAglTor = vecMTor ./ (vecG .* vecInertieTor)

    vecAngleTorsion = cumsum(vecDerivAglTor .* pas) # integration le long de la tige

    if (max(abs(vecAngleTorsion)) > AngleMax)
      disp(['Angle de torsion maximum (degres) = ' num2str(max(abs(vecAngleTorsion)) .* 180 ./ pi)])
    disp("(!) hypothese des petits deplacements non verifiee en TORSION (!)")
    end

    SomCum_vecAglTor = SomCum_vecAglTor + vecAngleTorsion # cumul par increment de poids

    if and((gph == 1), (iterPoids == Nboucle))
    disp(' ')
    disp(['Angle final de torsion en bout (degres) = ' num2str(SomCum_vecAglTor(end) .* 180 ./ pi)])
    end

    %=============================================================================
      # Nouvelles coordonnees des points
      neoVecX = zeros(1, Nlin)
    neoVecY = zeros(1, Nlin)
    neoVecZ = zeros(1, Nlin)

    for iter = 1 : Nlin
    # Origine P1
    if iter == 1
    P2 = [vecX(iter); vecY(iter); vecZ(iter)]
    P1 = [0; 0; 0]
    else
      P2 = [vecX(iter); vecY(iter); vecZ(iter)]
    P1 = [vecX(iter-1); vecY(iter-1); vecZ(iter-1)]
    end

    P2P1 = P2 - P1

    # Changement de base
    # Segment devient colineaire a l'axe OX
    vecRotInv = Rota_Inverse_YZ(P2P1, vecAngle_XY(iter), vecAngle_XZ(iter));

    # Flexion ===
    # Equivalent a une rotation autour de OY
    # Rotation autour de OY
    # La rotation est fausse pour les forts angles
    # Code remplace par :
    vecRotFlex(1,1)  = vecRotInv(1)
    vecRotFlex(2,1)  = vecRotInv(2)
    vecRotFlex(3,1)  = pas .* vecAngleFlexion(iter)

    # Torsion
    # Equivalent a une rotation autour de OX
    # Initialement la section est tournée mais sans torsion
    aglTorGeom = SomCum_vecAglTor(iter) - oriTor(iter)

    cs = cos(aglTorGeom)
    sn = sin(aglTorGeom)

    matRotX =	[1   0   0;
               0  cs -sn;
               0  sn  cs];

    vecRotTor = matRotX * vecRotFlex

    # Base d'origine
    vecRot = Rota_YZ(vecRotTor, vecAngle_XY(iter), vecAngle_XZ(iter))

    # Point dans la base origine
    if iter == 1
    neoX = vecRot(1)
    neoY = vecRot(2)
    neoZ = vecRot(3)
    else
      neoX = neoVecX(iter - 1) + vecRot(1)
    neoY = neoVecY(iter - 1) + vecRot(2)
    neoZ = neoVecZ(iter - 1) + vecRot(3)
    end

    # Re-ecriture des points
    neoVecX(iter) = neoX
    neoVecY(iter) = neoY
    neoVecZ(iter) = neoZ

    end

    # Mise a jour des variables
    vecX = neoVecX
    vecY = neoVecY
    vecZ = neoVecZ

    # Conservation des distances
    # pas = distance entre les points
    [v1, v2, v3] = XYZ_Vers_Agl(vecX, vecY, vecZ)
    [vecX, vecY, vecZ] = Agl_Vers_XYZ([0 repmat(pas,1,(Nlin-1))], v2, v3)

    clear v1 v2 v3

    # Calcul des distances des points experimentaux
    # Entre avant et apres deformation
    for iter = 1 : NpointsExp
    c1 = (vX(iter) - vecX(iDiscretPtsExp(iter))).^2
    c2 = (vY(iter) - vecY(iDiscretPtsExp(iter))).^2
    c3 = (vZ(iter) - vecZ(iDiscretPtsExp(iter))).^2

    matDistPtsExp(iterPoids, iter) = sqrt(c1 + c2 + c3)
    end

    if (gph == 1)
      # Affichage graphique
      plot3(vecX, vecY, vecZ, 'b-')
    plot3(vecX(iDiscretPtsExp), vecY(iDiscretPtsExp), vecZ(iDiscretPtsExp), 'bo')
    end

  } # iterPoids
  %=== Fin de boucle ===

    if (gph == 1)
      hold off

  disp(' ')
  disp('Distances des points experimentaux entre avant et apres deformation (mm) :')
  disp(matDistPtsExp.*1e3)
  end

  %=== Resultats ===
    PtsX = vecX(iDiscretPtsExp)
  PtsY = vecY(iDiscretPtsExp)
  PtsZ = vecZ(iDiscretPtsExp)

  [PtsDist, PtsAglXY, PtsAglXZ] = XYZ_Vers_Agl(PtsX, PtsY, PtsZ)

  PtsAglXY = PtsAglXY .* 180 ./ pi
  PtsAglXZ = PtsAglXZ .* 180 ./ pi
  PtsAglTor = SomCum_vecAglTor(iDiscretPtsExp) .* 180 ./ pi

  matRes = [PtsX; PtsY; PtsZ; PtsDist; PtsAglXY; PtsAglXZ; PtsAglTor]
}
