
#' Optimize bending parameters
#'
#' Optimize the elastic_modulus, the shear_modulus or both using
#' observations.
#'
#' @param field_data      Field data (data.frame of x, y and z coordinates of the points)
#' @param elastic_modulus Elasticity modulus (bending, MPa) vector: c(min,start,max)
#' @param shear_modulus   Shear modulus (torsion, MPa) vector: c(min,start,max)
#' @param type            Type of optimization (either "bending","torsion" or "all")
#' @param tol             Tolerance for optimization accuracy
#' @param ...             Further parameters to pass to [bend()]
#'
#' @details The minimim and maximum values are not used when a parameter is not
#' optimized, but they still should be provided by the user though.
#'
#' @return The optimized values
#' @export
#'
#' @examples
#' file_path = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' field_data = read_mat(file_path)
#' # optimize_bend(field_data, type = "all")
#' # Gives:
#' # $elastic_modulus
#' # [1] 1209.396
#' # $shear_modulus
#' # [1] 67.44096
optimize_bend = function(field_data,
                         elastic_modulus = c(1,100,10000),
                         shear_modulus = c(1,1000,10000),
                         type = c("all","bending","torsion"),
                         tol = .Machine$double.eps^0.25,
                         ...){

  type = match.arg(type, c("all","bending","torsion"), several.ok = FALSE)

  required_columns =
    c("x","y","z","distance",
      "type","width","height","inclination",
      "torsion","mass","mass_right","mass_left")

  if(!all(required_columns %in% colnames(field_data))){
    stop("Missing columns in field_data: ",
         paste(required_columns[!required_columns %in% colnames(field_data)],
               collapse = ", "))
  }

  df_unbent = unbend(field_data)
  # Adding the distance of application of the left and right weight:
  df_unbent$distance_application = distance_weight_sine(df_unbent$x)

  if(type != "all"){
    # univariate optimization:
    if(type == "bending"){
      interval = elastic_modulus[-2]
      y = elastic_modulus[2]
      param_name = "elastic_modulus"
    }else{
      interval = shear_modulus[-2]
      y = shear_modulus[2]
      param_name = "shear_modulus"
    }


    out_opti= stats::optimize(f = compute_error,
                              interval= interval,
                              y = y,
                              field_data = field_data,
                              unbent_data = df_unbent,
                              type = type,
                              verbose = FALSE,
                              ...,
                              tol = tol)
    params= as.list(out_opti$minimum)
    names(params)= param_name
  }else{
    # multivariate optimization:
    out_opti= dfoptim::nmkb(fn= compute_error,
                            par= c(elastic_modulus[2],
                                   shear_modulus[2]),
                            lower= c(elastic_modulus[1],
                                     shear_modulus[1]),
                            upper= c(elastic_modulus[3],
                                     shear_modulus[3]),
                            y = NULL,
                            field_data = field_data,
                            unbent_data = df_unbent,
                            type = type,
                            verbose = FALSE,
                            ...)
    params= as.list(out_opti$par)
    names(params)= c("elastic_modulus","shear_modulus")
  }
  return(params)
}

#' Compute error
#'
#' Compute the error of simulation usin field data, field data that is
#' un-bent, and bending parameters. Mainly used from [optimize_bend()].
#'
#' @param field_data      Field data (data.frame of x, y and z coordinates of the points)
#' @param unbent_data     Output from `unbend(field_data)` + [distance_weight_sine()]
#' @param x,y   Either the elasticity modulus (bending, MPa) or Shear modulus (torsion, MPa) (see dtails)
#' @param type            Type of optimization (either "bending","torsion" or "all")
#' @param ...             Further parameters to pass to [bend()]
#'
#' @details x an y depends on the optimization type. `x` is the elasticity modulus if "bending",
#' or shear modulus if "torsion". The `y` is the contrary. This is due to
#' the fact that [stats::optimize()] always use the first argument for optimization.
#' If `type == "all"`, then `x` is the elasticity modulus.
#'
#' @return The quadratic error of simulation
#' @keywords internal
#'
compute_error = function(x,y,field_data,unbent_data,
                         type = c("all","bending","torsion"),...){

  if(type == "bending"){
    elastic_modulus = x
    shear_modulus = y
  }else if(type == "torsion") {
    elastic_modulus = y
    shear_modulus = x
  }else{
    elastic_modulus = x[1]
    shear_modulus = x[2]
  }

  df_bent = bend(unbent_data, elastic_modulus = elastic_modulus,
                 shear_modulus = shear_modulus,...)

  # Quadratic error between simulation and observations (in m):
  npoints = nrow(field_data)

  eZ = sqrt(sum((field_data$z - df_bent$z)^2) / npoints)

  if(type == 'bending'){
    return(eZ)
  }

  eX = sqrt(sum((field_data$x - df_bent$x)^2) / npoints)
  eY = sqrt(sum((field_data$y - df_bent$y)^2) / npoints)

  if(type == 'torsion'){
    return(eX + eY)
  }

  if(type == 'all'){
    return(eX + eY + eZ)
  }

}
