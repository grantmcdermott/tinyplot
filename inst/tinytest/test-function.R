source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(x = -1:6, type = type_function(
    fun = dnorm, args = list(mean = 3),
    col = "pink", type = "p", pch = 3
  ))
}
expect_snapshot_plot(f, "function-01")
