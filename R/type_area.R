#' @rdname type_ribbon
#' @export
type_area = function() {
  out = list(
    draw = NULL,
    data = data_area(),
    name = "area"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_area = function() {
    fun = function(datapoints, ...) {
        datapoints$ymax = datapoints$y
        datapoints$ymin = rep.int(0, nrow(datapoints))
        out = list(
            datapoints = datapoints,
            ymax = datapoints$ymax,
            ymin = datapoints$ymin,
            type = "ribbon"
        )
        return(out)
    }
    return(fun)
}
