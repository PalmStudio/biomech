#' Inertia and surface
#'
#' Computes the inertia of bending and torsion, and the cross-section area
#'
#' @param b Dimesion of the base
#' @param h Dimension of the height
#' @param agDeg section orientation angle (torsion, in degrees)
#' @param sct Section type (see details)
#' @param N Number of discretizations (default to 100)
#'
#' @details For the section type, possible values are:
#' - sct = 1 : triangle (bottom-oriented)
#' - sct = 2 : rectangle
#' - sct = 3 : triangle (top-oriented)
#' - sct = 4 : ellipsis
#' - sct = 5 : circle
#'
#' @return A list of three:
#' - IgFlex : bending inertia
#' - IgTor : torsion inertia
#' - Sr : cross-section surface
#'
#' @export
#'
InertieFlexRota = function(b, h, agDeg, sct, N = 100){
  pas = min(b, h) / N
  n = round(h / pas) + 1
  m = round(b / pas) + 1

  # Creation de la section
  section = matrix(0, nrow = n, ncol = m)
  section = Remplir(section, sct)

  # Construction des variables vectorisees
  iterN = matrix(1 : nrow(section), nrow = 1)
  iterM = matrix(1 : ncol(section), nrow = 1)

  matIndLigne = matrix(rep(t(iterN), iterM[length(iterM)]), ncol = iterM[length(iterM)])
  matIndColonne = matrix(rep(iterM, iterN[length(iterN)]), nrow = iterN[length(iterN)], byrow = TRUE)

  vecPresenceSection = section
  vecIndLigne = matIndLigne
  vecIndColonne = matIndColonne

  # Centre de gravite
  ng = sum(vecPresenceSection * vecIndLigne) / sum(vecPresenceSection)
  mg = sum(vecPresenceSection * vecIndColonne) / sum(vecPresenceSection)

  # Inerties et surface
  angleRadian = agDeg * pi / 180
  rotMatrix = matrix(c(cos(angleRadian), -sin(angleRadian), sin(angleRadian), cos(angleRadian)), nrow = 2, byrow = TRUE)

  Point_x = vecPresenceSection * ((vecIndColonne - mg) * pas)
  Point_y = vecPresenceSection * ((vecIndLigne - ng) * pas)

  Point = matrix(c(Point_x, Point_y), nrow = 2, byrow = TRUE)
  rotPoint = rotMatrix %*% Point

  x = rotPoint[1,]
  y = rotPoint[2,]

  dS  = pas^2
  IgFlex = sum(y^2) * dS
  IgTor = sum(x^2 + y^2) * dS
  Sr = sum(vecPresenceSection) * dS

  list(IgFlex = IgFlex, IgTor = IgTor, Sr = Sr)
}

#' Fill in the matrix
#'
#' Compute the matrix according to the section shape
#'
#' @param section section matrix
#' @param sct Section type (see details)
#'
#' @return The matrix
#' @export
#'
#' @examples
#' Remplir(matrix(0, nrow = 6, ncol = 10), 1)
Remplir= function(section, sct){

  # sct = 1 : triangle bas
  if(sct == 1){

    b13 = solve(matrix(c(1,1,ncol(section)/2,1), nrow = 2, byrow = TRUE)) %*% matrix(c(1,nrow(section)), ncol = 1)
    b23 = solve(matrix(c(ncol(section),1,ncol(section)/2,1), nrow = 2, byrow = TRUE)) %*% matrix(c(1,nrow(section)), ncol = 1)

    iterN = matrix(1 : nrow(section), nrow = 1)
    iterM = matrix(1 : ncol(section), nrow = 1)

    matIndLigne = matrix(rep(t(iterN), iterM[length(iterM)]), ncol = iterM[length(iterM)])
    matIndColonne = matrix(rep(iterM, iterN[length(iterN)]), nrow = iterN[length(iterN)], byrow = TRUE)

    vecIndLigne = matIndLigne
    vecIndColonne = matIndColonne

    n13 = vecIndColonne * b13[1] + b13[2]
    n23 = vecIndColonne * b23[1] + b23[2]

    section = vecIndLigne <= n13 & vecIndLigne <= n23

  }

  # sct = 2 : rectangle
  if(sct == 2){
    section = matrix(rep(1,length(section)), nrow = nrow(section))
  }

  # sct = 3 : triangle haut
  if(sct == 3){

    b13 = solve(matrix(c(1,1,ncol(section)/2,1), nrow = 2, byrow = TRUE)) %*% matrix(c(1,nrow(section)), ncol = 1)
    b23 = solve(matrix(c(ncol(section),1,ncol(section)/2,1), nrow = 2, byrow = TRUE)) %*% matrix(c(1,nrow(section)), ncol = 1)

    iterN = matrix(1 : nrow(section), nrow = 1)
    iterM = matrix(1 : ncol(section), nrow = 1)

    matIndLigne = matrix(rep(t(iterN), iterM[length(iterM)]), ncol = iterM[length(iterM)])
    matIndColonne = matrix(rep(iterM, iterN[length(iterN)]), nrow = iterN[length(iterN)], byrow = TRUE)

    vecIndLigne = matIndLigne
    vecIndColonne = matIndColonne

    n13 = vecIndColonne * b13[1] + b13[2]
    n23 = vecIndColonne * b23[1] + b23[2]

    section = vecIndLigne >= n13 & vecIndLigne >= n23
  }

  # sct = 4 : ellipse
  if(sct == 4){

    a = max(dim(section)) / 2
    b = min(dim(section)) / 2

    c = sqrt(a^2 - b^2)

    iterN = matrix(1 : nrow(section), nrow = 1)
    iterM = matrix(1 : ncol(section), nrow = 1)

    matIndLigne = matrix(rep(t(iterN), iterM[length(iterM)]), ncol = iterM[length(iterM)])
    matIndColonne = matrix(rep(iterM, iterN[length(iterN)]), nrow = iterN[length(iterN)], byrow = TRUE)

    vecIndLigne = matIndLigne
    vecIndColonne = matIndColonne

    if(nrow(section) >= ncol(section)){

      mf = ncol(section) / 2

      nf1 = (a - c)
      nf2 = 2 * c + (a - c)

      dist1 = sqrt((vecIndLigne - nf1)^2 + (vecIndColonne - mf)^2)
      dist2 = sqrt((vecIndLigne - nf2)^2 + (vecIndColonne - mf)^2)

      section = ((dist1 + dist2) <= (2 * a))
    }

    if(nrow(section) < ncol(section)){

      nf = nrow(section) / 2

      mf1 = (a - c)
      mf2 = 2 * c + (a - c)

      dist1 = sqrt((vecIndLigne - nf)^2 + (vecIndColonne - mf1)^2)
      dist2 = sqrt((vecIndLigne - nf)^2 + (vecIndColonne - mf2)^2)

      section = ((dist1 + dist2) <= (2 * a))
    }

  }

  # sct = 5 : cercle
  if(sct == 5){

    rayon = min(dim(section)) / 2

    n0 = nrow(section) / 2
    m0 = ncol(section) / 2

    iterN = matrix(1 : nrow(section), nrow = 1)
    iterM = matrix(1 : ncol(section), nrow = 1)

    matIndLigne = matrix(rep(t(iterN), iterM[length(iterM)]), ncol = iterM[length(iterM)])
    matIndColonne = matrix(rep(iterM, iterN[length(iterN)]), nrow = iterN[length(iterN)], byrow = TRUE)

    vecIndLigne = matIndLigne
    vecIndColonne = matIndColonne

    dist = sqrt((vecIndLigne - n0)^2 + (vecIndColonne - m0)^2)
    section = dist <= rayon
  }
  section
}
