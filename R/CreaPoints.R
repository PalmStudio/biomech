
#' Experimental matrix
#'
#' Build the matrix from the experimental points
#'
#' @param MOE Elastic modulus
#' @param CIS Shear modulus
#' @param matExp Experimental matrix
#'
#' @note Lengths in meters, angles in degrees and mass in kg in matExp.
#' @return The point matrix
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "deformation")
#' matExp = data.table::fread(filepath, data.table = FALSE)
#' CreaPoints(2000,400,matExp)
#' }
CreaPoints = function(MOE, CIS, matExp){


  #===============================================================================
  # Identification des lignes de 'matExp'
  iDistance = 1
  iSection_Type = 2
  iSection_b = 3
  iSection_h = 4
  iInclinaison_aglY = 5
  iSection_aglTor = 6
  iX = 7
  iY = 8
  iZ = 9
  iPoids_Tige = 10
  iPoidsF_Dr = 11
  iPoidsF_Ga = 12

  #===============================================================================
  # Creation de 'matPoints' (etat non deforme)
  # (1,) Coordonnees des points (x) (m)
  # (2,) Coordonnees des points (y) (m)
  # (3,) Coordonnees des points (z) (m)
  # (4,) Type de section
  # (5,) b (m)
  # (6,) h (m)
  # (7,) poids tige (kg)
  # (8,) poids feuilles D (kg)
  # (9,) poids feuilles G (kg)
  # (10,) Module elasticite (Mpa)
  # (11,) Module cisaillement (MPa)
  # (12,) Angle d'orientation (torsion) de la section (degre)
  # (13,) Distance application du poids des feuilles (m)

  # La distance entre les points ne peut pas etre nulle
  iD0 = which(matExp[iDistance,] == 0)
  if(length(iD0)>0){
    matExp[iDistance, iD0] = 1e-3 # (m)
  }
  # Coordonnees des points (etat non deforme)
  Distance = matExp[iDistance,]
  XDistance = cumsum(unlist(Distance))

  Agl_Y = matExp[iInclinaison_aglY, 1] * pi / 180
  Agl_Z = 0

  NpointsExp = length(Distance)

  X = Y = Z = rep(0, NpointsExp)

  # Segment de droite oriente dans un espace 3D
  for(iter in 1 : NpointsExp){
    OP = c(XDistance[[iter]], 0, 0)
    vecRot = Rota_YZ(OP, Agl_Y, Agl_Z)

    X[iter] = vecRot[1]
    Y[iter] = vecRot[2]
    Z[iter] = vecRot[3]
  }

  # Affichage graphique
  # if (gph == 1)
  #   figure, hold on
  # plot3(matExp(iX,:), matExp(iY,:), matExp(iZ,:), 'bo--')
  # plot3(X, Y, Z, 'ko--')
  # xlabel('X (m)'), ylabel('Y (m)'), zlabel('Z (m)')
  # title('Etat deforme (mesures experimentales) et non deforme')
  # axis('equal'), grid on, view(45, 25)
  # end

  # Section
  TypeSection = matExp[iSection_Type,]
  Base = matExp[iSection_b,]
  Hauteur = matExp[iSection_h,]

  # Poids
  PoidsTige = matExp[iPoids_Tige,]
  PoidsFeuillesDroite = matExp[iPoidsF_Dr,]
  PoidsFeuillesGauche = matExp[iPoidsF_Ga,]

  # Elasticite
  # Valeur constante
  ModuleElasticite = rep(MOE, NpointsExp)
  ModuleCisaillement = rep(CIS, NpointsExp)

  # Orientations initales des sections
  AngleSection = matExp[iSection_aglTor,]

  # Distance d'application du poids des feuilles
  # Evolution sinus, valeur arbitraire de dF
  # Le ratio dF/CIS est optimise
  dF = 0.1
  DAppliPoidsFeuil = sin(XDistance / tail(XDistance,1) * pi) * dF

  #===============================================================================
  # Resultats
  matrix(unlist(c(X,Y,Z,TypeSection,Base,Hauteur,
                  PoidsTige,PoidsFeuillesDroite,PoidsFeuillesGauche,
                  ModuleElasticite,ModuleCisaillement,AngleSection,
                  DAppliPoidsFeuil)), ncol = NpointsExp, byrow = TRUE)
  # x	y	z	type	b	h	mass	leaves_mass_right	leaves_mass_left	elastic_modulus	shear_modulus	torsion distance
}
