#' Unbend
#'
#' Takes measurements from field that are already bent (torsion + bending) and transform them
#' to a straight line (un-bending).
#' Mainly used to compute the matrix for input to [bend()] from experimental points.
#'
#' @param MOE Elastic modulus
#' @param CIS Shear modulus
#' @param df Experimental data (usually read using [read_mat()], see details)
#'
#' @details df should be a formatted [data.frame()], with each row being a point and each columns being:
#' - distance (m): distance between the previous point and this point (first value should be positive)
#' - type: section type. 1: triangle (bottom-oriented); 2 : rectangle; 3 : triangle (top-oriented);
#' 4 : ellipsis; 5 : circle.
#' - width (m): section width
#' - height (m): section height
#' - inclination (degree): insertion angle of the first point (only first value is used)
#' - torsion (degree): torsion angle
#' - x: x coordinate (optional, only for plotting)
#' - y: y coordinate (optional, only for plotting)
#' - z: z coordinate (optional, only for plotting)
#' - mass (kg): mass of the section at the point
#' - mass_right (kg): mass carried by the object, on the right side
#' - mass_left (kg): mass carried by the object, on the left side
#'
#' @return A [data.frame()]:
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
#' @export
#'
#' @examples
#' \dontrun{
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' df = read_mat(filepath)
#' unbend(MOE = 2000, CIS = 400, df = df)
#' }
unbend = function(MOE, CIS, df){

  # La distance entre les points ne peut pas etre nulle
  iD0 = which(df$distance == 0)
  if(length(iD0)>0){
    df$distance[iD0] = 1e-3 # (m)
  }
  # Coordonnees des points (etat non deforme)
  Distance = df$distance
  XDistance = cumsum(unlist(Distance))

  Agl_Y = df$inclination[1] * pi / 180
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
  # plot3(df(iX,:), df(iY,:), df(iZ,:), 'bo--')
  # plot3(X, Y, Z, 'ko--')
  # xlabel('X (m)'), ylabel('Y (m)'), zlabel('Z (m)')
  # title('Etat deforme (mesures experimentales) et non deforme')
  # axis('equal'), grid on, view(45, 25)
  # end

  # Section
  TypeSection = df$type
  Base = df$width
  Hauteur = df$height

  # Poids
  PoidsTige = df$mass
  PoidsFeuillesDroite = df$mass_right
  PoidsFeuillesGauche = df$mass_left

  # Elasticite
  # Valeur constante
  ModuleElasticite = rep(MOE, NpointsExp)
  ModuleCisaillement = rep(CIS, NpointsExp)

  # Orientations initales des sections
  AngleSection = df$torsion

  # Distance d'application du poids des feuilles
  # Evolution sinus, valeur arbitraire de dF
  # Le ratio dF/CIS est optimise
  dF = 0.1
  DAppliPoidsFeuil = sin(XDistance / utils::tail(XDistance,1) * pi) * dF

  #===============================================================================
  # Resultats
  df = data.frame(x= X, y = Y, z = Z, type = TypeSection, width = Base,
                  height = Hauteur, mass = PoidsTige, mass_right = PoidsFeuillesDroite,
                  mass_left = PoidsFeuillesGauche,
                  elastic_modulus = ModuleElasticite,
                  shear_modulus = ModuleCisaillement,
                  torsion = AngleSection,
                  distance_application = DAppliPoidsFeuil)
  class(df) = append("unbent",class(df))
  df
}

