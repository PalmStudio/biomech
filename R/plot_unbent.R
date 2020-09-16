#' Plot unbent
#'
#' @param unbent Unbent data.frame
#' @param meas   Field measurement data.frame (as from [read_mat()])
#' @param ...    Further parameters.
#'
#' @return A plotly object, see [plotly::plot_ly()]
#' @export
#'
#' @examples
#' filepath = system.file("extdata/6_EW01.22_17_kanan.txt", package = "biomech")
#' meas = read_mat(filepath)
#' df_unbend = unbend(MOE = 2000, CIS = 400, df = meas)
#' plot_unbent(df_unbend,meas)
plot_unbent = function(unbent, meas = NULL){
    plot_ly(unbent, x= ~x, y = ~y, z= ~z, type = 'scatter3d',
            mode = 'lines', name = "Un-bent")%>%
    {
      if(!is.null(meas)){
        add_trace(p = ., data = meas, x = ~x, y = ~y, z = ~z,
                  name = "Bent state (field data)")
      }else{
        .
      }
    }
}
