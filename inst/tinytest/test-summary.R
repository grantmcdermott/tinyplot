source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(weight ~ Time, data = ChickWeight, type = type_summary())
}
expect_snapshot_plot(f, "summary_simple")

f = function() {
  tinyplot(
    weight ~ Time | Diet, facet = "by", data = ChickWeight,
    type = type_summary(function(y) quantile(y, probs = 0.9)/max(y))
  )
}
expect_snapshot_plot(f, "summary_complex")
