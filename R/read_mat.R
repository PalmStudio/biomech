#' Read field matrix
#'
#' Read the matrix from field measurements.
#'
#' @param path Path to the file
#'
#' @return A formatted [data.frame()]
#' @export
#'
#' @examples
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "deformation")
#' matExp = read_mat(filepath)
read_mat = function(path){
  matExp = data.table::fread(path, data.table = FALSE)
  if(nrow(matExp) != 12){
    stop("File not in the right format, expected 12 columns, found ",
         nrow(matExp), ". Did you try to read a data.frame ? In that ",
         'case use e.g.:\n`data.table::fread("',path,'"',", data.table = FALSE)`")
  }
  df = data.frame(t(matExp))

  colnames(df) = c("distance","type","width","heigth","inclination",
                   "torsion","x","y","z","mass","mass_right",
                   "mass_left")
  df
}
