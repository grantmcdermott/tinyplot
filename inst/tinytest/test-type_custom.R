source("helpers.R")
using("tinysnapshot")

# type_log
f = function () {

  # based on the "redux" example in the types vignette
  type_log = function(base = exp(1)) {
    data_log = function() {
      fun = function(settings, ...) {
        datapoints = settings$datapoints
        datapoints$x = log(datapoints$x, base = base)
        datapoints$y = log(datapoints$y, base = base)
        datapoints = datapoints[order(datapoints$x), ]
        settings$datapoints = datapoints
        settings$type = "p"
      }
      return(fun)
    }
    out = list(
      data = data_log(),
      draw = NULL,
      name = "log"
    )
    class(out) = "tinyplot_type"
    return(out)
  }

  tinyplot(mpg ~ wt | factor(am), data = mtcars,
           pch = "by",
           type = type_log(),
           main = "Custom: type_log()")
}
expect_snapshot_plot(f, label = "custom_type_log")