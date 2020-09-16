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
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' # Un-bending the field measurements:
#' df = unbend(2000,400, read_mat(filepath))
#' df_bend = bend(df, step = 0.02, points = 100, iterations = 15, verbose = TRUE)
#' plot_bending(df_bend,df)
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
                          name = "Initial state")
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
