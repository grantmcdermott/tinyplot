#' @rdname type_ribbon
#' @export
type_area = function(alpha = NULL) {
  out = list(
    draw = NULL,
    data = data_area(alpha = alpha),
    name = "area"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_area = function(alpha = alpha) {
    ribbon.alpha = if (is.null(alpha)) .tpar[["ribbon.alpha"]] else (alpha)
    fun = function(datapoints, ...) {
        datapoints$ymax = datapoints$y
        datapoints$ymin = rep.int(0, nrow(datapoints))
        out = list(
            datapoints = datapoints,
            ymax = datapoints$ymax,
            ymin = datapoints$ymin,
            type = "ribbon",
            ribbon.alpha = ribbon.alpha
        )
        return(out)
    }
    return(fun)
}
