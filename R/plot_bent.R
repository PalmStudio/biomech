#' Plot bent
#'
#' @param ... A list of X, Y, Z data.frames for each beam of beam state
#'
#' @return A ggplot or a plotly object (see [plotly::plot_ly()])
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' file_path = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' field_data = read_mat(file_path)
#' # Un-bending the field measurements:
#' df_unbent = unbend(field_data)
#'
#' # Adding the distance of application of the left and right weight:
#' df_unbent$distance_application = distance_weight_sine(df_unbent$x)
#'
#' # (Re-)computing the deformation:
#' df_bent = bend(df_unbent, elastic_modulus = 2000, shear_modulus = 400, step = 0.02,
#'                points = 100, iterations = 15, verbose = TRUE)
#'
#' # And finally, plotting:
#' plot_bending(bent = df_bent, "un-bent" = df_unbent)
#'
#' # And in 3d:
#' # plot_bending_3d(bent = df_bent, "un-bent" = df_unbent)
plot_bending = function(...){
  dot_args = list(...)

  if(is.null(names(dot_args))){
    names(dot_args) = paste("Beam",seq_along(dot_args))
  }

  if(any(names(dot_args) == "")){
    names(dot_args)[names(dot_args) == ""] =
      paste("Beam",which(names(dot_args) == ""))
  }


  df =
    mapply(function(x,y){
      x = data.frame(x = x$x, y = x$y, z = x$z)
      x$name = y
      x
    },
    dot_args, names(dot_args), SIMPLIFY = FALSE)

  df = data.table::rbindlist(df)

  plot_range = range(df$x,df$y,df$z)
  plot_range_max = max(plot_range)

  side_view =
    ggplot2::ggplot(data = df, ggplot2::aes(x = .data$x, y = .data$z,
                                            color = .data$name))+
    ggplot2::geom_line()+
    ggplot2::coord_cartesian(xlim = plot_range,
                             ylim = plot_range)+
    ggplot2::ggtitle("Side view")+
    ggplot2::labs(colour = NULL)

  top_view =
    ggplot2::ggplot(data = df, ggplot2::aes(x = .data$x, y = .data$y,
                                            color = .data$name))+
    ggplot2::geom_line()+
    ggplot2::coord_cartesian(xlim = plot_range,
                             ylim = c(-plot_range_max,
                                      plot_range_max))+
    ggplot2::ggtitle("Top view")+
    ggplot2::labs(colour = NULL)

  patchwork::wrap_plots(side_view, top_view, guides = 'collect')  &
    ggplot2::theme(legend.position='bottom')
}

#' @rdname plot_bending
#' @export
plot_bending_3d = function(...){
  dot_args = list(...)

  if(is.null(names(dot_args))){
    names(dot_args) = paste("Beam",seq_along(dot_args))
  }

  if(any(names(dot_args) == "")){
    names(dot_args)[names(dot_args) == ""] =
      paste("Beam",which(names(dot_args) == ""))
  }


  df =
    mapply(function(x,y){
      x = data.frame(x = x$x, y = x$y, z = x$z)
      x$name = y
      x
    },
    dot_args, names(dot_args), SIMPLIFY = FALSE)

  df = data.table::rbindlist(df)

  plot_range = range(df$x,df$y,df$z)
  plot_range_max = max(plot_range)

  plotly::plot_ly(df, x= ~x, y = ~y, z= ~z, type = 'scatter3d',
                  mode = 'lines', color = ~name)%>%
    plotly::layout(scene = list(
      xaxis = list(range = plot_range),
      yaxis = list(range = c(-plot_range_max,plot_range_max)),
      zaxis = list(range = plot_range))
    )
}
