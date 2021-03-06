
#' Optimize bending parameters
#'
#' Optimize the elastic_modulus, the shear_modulus or both using
#' observations.
#'
#' @param field_data      data.frame, or list of data.frames with the x, y and z coordinates of the
#' observed points. If list of data.frames, each element correspond to a separate beam (e.g. each branch)
#' @param elastic_modulus Elasticity modulus (bending, MPa). See details.
#' @param shear_modulus   Shear modulus (torsion, MPa). See details.
#' @param type            Type of optimization (either "bending","torsion" or "all")
#' @param tol             Tolerance for optimization accuracy
#' @param nb_rep          Number of starting points for the optimization algorithm
#' @param method          Method to use when optimizing one parameter (see [stats::optim()])
#' @param ...             Further parameters to pass to [bend()]
#'
#' @details The elastic_modulus and shear_modulus are either provided as a vector
#' of min and max values if optimized, or as a single value to force the parameter if
#' not optimized.
#'
#' @return A list of the outputs:
#' - The best optimized values for each parameter
#' - `init_values`: a data.frame with the initial values used for each repetition
#' - `optim_values`: a data.frame with the optimized values for each step
#' - `min_quadratic_error`: minimum quadratic error of all repetitions
#' - `rep_min_crit`: index of the repetition that gave the minimum quadratic error
#' - `plots`: plots of optimal value ~ initial value for each parameter to analyze
#' the sensitivity of the optimized value to the starting points.
#'
#' @export
#'
#' @examples
#' file_path = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' field_data = read_mat(file_path)
#' # optimize_bend(field_data, type = "all")
#' # Gives:
#' # $elastic_modulus
#' # [1] 1209.622
#
#' # $shear_modulus
#' # [1] 67.49158
#
#' # $init_values
#' # elastic_modulus shear_modulus
#' # 1        8154.915      110.9907
#' # 2        7070.047     4120.5218
#' # 3        9580.339     5798.9579
#' # 4        2980.401     4703.5510
#' # 5        8860.528      545.3304
#
#' # $optim_values
#' # [,1]       [,2]
#' # [1,] 1209.316   67.35572
#' # [2,] 1209.598   67.27172
#' # [3,] 1214.884 8091.98399
#' # [4,] 1209.622   67.49158
#' # [5,] 1223.161 2126.35615
#
#' # $min_quadratic_error
#' # [1] 0.3787925
#
#' # $rep_min_crit
#' # [1] 4
#
#' # $plots
#' # $plots[[1]]
#' # Here comes a ggplot for the first parameter
#
#' # $plots[[2]]
#' # Here comes a ggplot for the second parameter
#'
#' # If only the elastic_modulus is optimized, the shear_modulus has to be fixed:
#' # optimize_bend(field_data, shear_modulus = 100, type = "bending")
#'
#' # If the parameters need to be optimized on several beams at the same time (they share
#' # the same properties), pass their observations as a list:
#' # optimize_bend(list(sim1 = field_data, sim2 = field_data), type = "all")
#'
optimize_bend = function(field_data,
                         elastic_modulus = c(1,10000),
                         shear_modulus = c(1,10000),
                         type = c("all","bending","torsion"),
                         method = "L-BFGS-B",
                         tol = .Machine$double.eps^0.25,
                         nb_rep = 5,
                         ...){

  type = match.arg(type, c("all","bending","torsion"), several.ok = FALSE)

  required_columns =
    c("x","y","z","distance",
      "type","width","height","inclination",
      "torsion","mass","mass_right","mass_left")

  if(is.data.frame(field_data)) field_data = list(sim1 = field_data)

  df_unbent =
    lapply(field_data, function(x){
      if(!all(required_columns %in% colnames(x))){
        stop("Missing columns in field_data: ",
             paste(required_columns[!required_columns %in% colnames(x)],
                   collapse = ", "))
      }
      df_unbent = unbend(x)
      # Adding the distance of application of the left and right weight:
      df_unbent$distance_application = distance_weight_sine(df_unbent$x)
      df_unbent
    })


  if(type != "all"){
    # univariate optimization:
    if(type == "bending"){
      interval = elastic_modulus
      start_values = data.frame(elastic_modulus =
                                  stats::runif(n = nb_rep, min = elastic_modulus[1],
                                               max = elastic_modulus[2]))
      y = shear_modulus[1]
      param_name = "elastic_modulus"
    }else{
      interval = shear_modulus
      start_values = data.frame(shear_modulus =
                                  stats::runif(n = nb_rep, min = shear_modulus[1],
                                               max = shear_modulus[2]))
      y = elastic_modulus[1]
      param_name = "shear_modulus"
    }

    optim = vector("list",nb_rep)

    for (irep in seq_len(nb_rep)){
      if (method=="L-BFGS-B" || method=="Brent") {
        try(optim[[irep]] <- stats::optim(par = as.numeric(start_values[irep,]),
                                          fn = compute_error,
                                          method = method,
                                          lower = interval[1],
                                          upper = interval[2],
                                          control = list(reltol = tol),
                                          y = y,
                                          field_data = field_data,
                                          unbent_data = df_unbent,
                                          type = type,
                                          verbose = FALSE,
                                          ...))
      } else {
        try(optim[[irep]] <- stats::optim(par = as.numeric(start_values[irep,]),
                                          fn = compute_error,
                                          method = method,
                                          control = list(reltol = tol),
                                          y = y,
                                          field_data = field_data,
                                          unbent_data = df_unbent,
                                          type = type,
                                          verbose = FALSE,
                                          ...))
      }
    }

  }else{
    param_name = c("elastic_modulus", "shear_modulus")
    start_values =
      data.frame(elastic_modulus =
                   stats::runif(n = nb_rep, min = elastic_modulus[1],
                                max = elastic_modulus[2]),
                 shear_modulus =
                   stats::runif(n = nb_rep, min = shear_modulus[1],
                                max = shear_modulus[2]))

    optim = vector("list",nb_rep)

    for (irep in seq_len(nb_rep)){
      # multivariate optimization:
      try(optim[[irep]] <- dfoptim::nmkb(fn= compute_error,
                                         par= as.numeric(start_values[irep,]),
                                         lower= c(elastic_modulus[1],
                                                  shear_modulus[1]),
                                         upper= c(elastic_modulus[2],
                                                  shear_modulus[2]),
                                         y = NULL,
                                         field_data = field_data,
                                         unbent_data = df_unbent,
                                         type = "all",
                                         verbose = FALSE,
                                         ...)
      )
    }
  }

  # Get the estimated values
  est_values = t(rbind(sapply(optim,`[[`,"par")))

  # Which repetition has the smallest criterion
  ind_min_crit = which.min(sapply(optim, function(x) {
    if (!is.null(x$value)) x$value
  }))

  params = as.list(est_values[ind_min_crit,])
  names(params) = param_name
  params$init_values = start_values
  params$optim_values = est_values
  params$min_quadratic_error = optim[[ind_min_crit]]$value
  params$rep_min_crit = ind_min_crit

  p =
    lapply(seq_along(param_name), function(x){
      data.frame(start_values = start_values[,x],
                 est_values = est_values[,x])%>%
        ggplot2::ggplot(ggplot2::aes(x = .data$start_values,
                                     y = .data$est_values))+
        ggplot2::geom_point()+
        ggplot2::xlab("Initial value")+
        ggplot2::ylab("Estimated")+
        ggplot2::ggtitle(paste("Optimized parameter: ",param_name[x]))+
        ggplot2::theme(aspect.ratio=1)
    })

  params$plots = p
  return(params)
}

#' Compute error
#'
#' Compute the error of simulation usin field data, field data that is
#' un-bent, and bending parameters. Mainly used from [optimize_bend()].
#'
#' @param field_data      Field data (list of data.frames with x, y and z coordinates of the points)
#' @param unbent_data     List of outputs from `unbend(field_data)` + [distance_weight_sine()]
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

  if(names(field_data) != names(unbent_data)){
    stop("List of field_data and unbent_data does not match")
  }

  df_bent =
    lapply(unbent_data, function(x){
      bend(x, elastic_modulus = elastic_modulus,
           shear_modulus = shear_modulus,...)
    })

  df_bent = data.table::rbindlist(df_bent)
  field_data = data.table::rbindlist(field_data)

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
