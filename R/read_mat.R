#' Read field matrix
#'
#' Read the matrix from field measurements.
#'
#' @param path Path to the file
#'
#' @return A formatted [data.frame()], with each row being a point and with columns being:
#' - distance (m): distance between the previous point and this point (first value should be positive)
#' - type: section type. 1: triangle (bottom-oriented); 2 : rectangle; 3 : triangle (top-oriented);
#' 4 : ellipsis; 5 : circle.
#' - width (m): section width
#' - height (m): section height
#' - inclination (degree): insertion angle of the first point (only first value is used)
#' - torsion (degree): torsion angle
#' - x: x coordinate
#' - y: y coordinate
#' - z: z coordinate
#' - mass (kg): mass of the section at the point
#' - mass_right (kg): mass carried by the object, on the right side
#' - mass_left (kg): mass carried by the object, on the left side
#'
#' @export
#'
#' @examples
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' df = read_mat(filepath)
read_mat = function(path){
  df = data.table::fread(path, data.table = FALSE)
  if(nrow(df) != 12){
    stop("File not in the right format, expected 12 columns, found ",
         nrow(df), ". Did you try to read a data.frame ? In that ",
         'case use e.g.:\n`data.table::fread("',path,'"',", data.table = FALSE)`")
  }
  df = data.frame(t(df))

  colnames(df) = c("distance","type","width","height","inclination",
                   "torsion","x","y","z","mass","mass_right",
                   "mass_left")
  df
}
