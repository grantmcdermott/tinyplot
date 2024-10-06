#' Jittered points type
#'
#' @inheritParams base::jitter
#' @export
type_jitter = function(factor = 1, amount = NULL) {
  out = list(
    draw = draw_points(),
    data = data_jitter(factor = factor, amount = amount),
    name = "p"
  )
  class(out) = "tinyplot_type"
  return(out)
}


data_jitter = function(factor, amount) {
    fun = function(datapoints, ...) {
        x = datapoints$x
        y = datapoints$y
        if (is.character(x)) x = as.factor(x)
        if (is.character(y)) y = as.factor(y)
        if (is.factor(x)) {
            xlvls = levels(x)
            xlabs = seq_along(xlvls)
            names(xlabs) = xlvls
            x = as.integer(x)
        } else {
            xlabs = NULL
        }
        if (is.factor(y)) {
            ylvls = levels(y)
            ylabs = seq_along(ylvls)
            names(ylabs) = ylvls
            y = as.integer(y)
        } else {
            ylabs = NULL
        }
        x = jitter(x, factor = factor, amount = amount)
        y = jitter(y, factor = factor, amount = amount)

        datapoints$x = x
        datapoints$y = y

        out = list(
            datapoints = datapoints,
            x = x,
            y = y,
            xlabs = xlabs,
            ylabs = ylabs
        )
        return(out)
    }
}

