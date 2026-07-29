source("helpers.R")
using("tinysnapshot")

# Smart x/ylim overrides (#616): scalar coverage, partial-NA limits, and the
# axis-reversal keyword.

# Partial NA: lower pinned at 0, upper driven by data.
fun = function() tinyplot(dist ~ speed, data = cars, ylim = c(0, NA))
expect_snapshot_plot(fun, label = "lim_partial_na")

# Scalar coverage: ensure value is shown alongside the data (in this case will
# be identical to previous plot since y value is not binding)
fun = function() tinyplot(dist ~ speed, data = cars, xlim = 0, ylim = 100)
expect_snapshot_plot(fun, label = "lim_scalar_zero")

# Reversed x-axis via keyword.
fun = function() tinyplot(dist ~ speed, data = cars, xlim = "reverse")
expect_snapshot_plot(fun, label = "lim_reverse")

# Reversed x-axis on free-scale facets (exercises the facet.R path).
fun = function() {
  fast = ifelse(cars$speed > 15, "fast", "slow")
  tinyplot(dist ~ speed, facet = fast, data = cars,
           facet.args = list(free = TRUE, ncol = 1), xlim = "reverse")
}
expect_snapshot_plot(fun, label = "lim_reverse_free_facet")
