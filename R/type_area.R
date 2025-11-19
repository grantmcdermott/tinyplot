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
    fun = function(settings, ...) {
        env2env(settings, environment(), "datapoints")
        datapoints$ymax = datapoints$y
        datapoints$ymin = rep.int(0, nrow(datapoints))
        ymax = datapoints$ymax
        ymin = datapoints$ymin
        type = "ribbon"
        env2env(environment(), settings, c(
            "datapoints",
            "ymax",
            "ymin",
            "type",
            "ribbon.alpha"
        ))
    }
    return(fun)
}
