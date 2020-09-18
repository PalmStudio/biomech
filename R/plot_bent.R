#' Plot bent
#'
#' @param bent   Bent data.frame
#' @param unbent Initial state data.frame (as from [unbend()])
#' @param ...    Further parameters.
#'
#' @return A plotly object, see [plotly::plot_ly()]
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
#' plot_bending(df_bent,df_unbent)
plot_bending = function(bent, unbent = NULL, ...){
  plot_range = range(bent$x,bent$y,bent$z,unbent$x,unbent$y,unbent$z)
  plot_range_max = max(plot_range)

  df = rbind(data.frame(x = bent$x, y = bent$y, z = bent$z, type = "bent"),
             data.frame(x = unbent$x, y = unbent$y, z = unbent$z, type = "unbent"))

  side_view =
    ggplot2::ggplot(data = df, ggplot2::aes(x = .data$x, y = .data$z,
                                            color = .data$type))+
    ggplot2::geom_line()+
    ggplot2::coord_cartesian(xlim = plot_range,
                             ylim = plot_range)+
    ggplot2::ggtitle("Side view")

  top_view =
    ggplot2::ggplot(data = df, ggplot2::aes(x = .data$x, y = .data$y,
                                            color = .data$type))+
    ggplot2::geom_line()+
    ggplot2::coord_cartesian(xlim = plot_range,
                             ylim = c(-plot_range_max,
                                      plot_range_max))+
    ggplot2::ggtitle("Top view")

  patchwork::wrap_plots(side_view, top_view, guides = 'collect')  &
    ggplot2::theme(legend.position='bottom')
}

#' @rdname plot_bending
#' @export
plot_bending_3d = function(bent, unbent = NULL, ...){
  . = NULL
  plot_range = range(bent$x,bent$y,bent$z,unbent$x,unbent$y,unbent$z)
  plot_range_max = max(plot_range)

  plotly::plot_ly(bent, x= ~x, y = ~y, z= ~z, type = 'scatter3d',
                  mode = 'lines', name = "Bent")%>%
    {
      if(!is.null(unbent)){
        plotly::add_trace(p = ., data = unbent, x = ~x, y = ~y, z = ~z,
                          name = "Un-bent")
      }else{
        .
      }
    }%>%
    plotly::layout(scene = list(
      xaxis = list(range = plot_range),
      yaxis = list(range = c(-plot_range_max,plot_range_max)),
      zaxis = list(range = plot_range))
    )
}
