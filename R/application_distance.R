#' Weight distance of application
#'
#' Computes the distance of application of the right/left weights using a sine
#' evolution along the beam.
#'
#' @param x  X position of the segments
#' @param dF Amplitude (m)
#'
#' @return The distance at which the left/right weights are applied from the beam axis. Both
#'  sides are considered equal.
#' @export
#'
#' @examples
#' distance_weight_sine(c(1,2,3))
distance_weight_sine = function(x,dF = 0.1){
  sin(x / x[length(x)] * pi) * dF
}
