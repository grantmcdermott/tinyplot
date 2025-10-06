#' Jittered points plot type
#'
#' @description Type function for plotting jittered points.
#' Arguments are passed to \code{\link[base]{jitter}}.
#'
#' @inheritParams base::jitter
#' @inherit base::jitter details
#' @examples
#' # "jitter" type convenience string
#' tinyplot(Sepal.Length ~ Species, data = iris, type = "jitter")
#'
#' # Use `type_jitter()` to pass extra arguments for customization
#' tinyplot(Sepal.Length ~ Species, data = iris, type = type_jitter(factor = 0.5))
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
    fun = function(settings, ...) {
        list2env(settings, environment())

        x = datapoints$x
        y = datapoints$y
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

        update_settings(settings,
            datapoints = datapoints,
            x = x,
            y = y,
            xlabs = xlabs,
            ylabs = ylabs
        )
    }
}
