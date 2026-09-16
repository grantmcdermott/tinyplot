source("helpers.R")
using("tinysnapshot")

# core geometry, including dodged groups
f = function() {
  plt(len ~ dose | supp, data = ToothGrowth, type = "sina", pch = 16)
}
expect_snapshot_plot(f, label = "sina_groups")

# points are displaced by the violin's own half-width, so a sina layer fits
# inside a violin exactly
f = function() {
  plt(weight ~ feed, data = chickwts, type = "violin")
  plt_add(type = "sina", pch = 16, col = "black")
}
expect_snapshot_plot(f, label = "sina_on_violin")

# unlike type_violin(), a continuous `by` is supported: it colours the points
# (with a gradient legend) rather than keying the density split
f = function() {
  plt(Sepal.Length ~ Species | Petal.Width, data = iris, type = "sina", pch = 16)
}
expect_snapshot_plot(f, label = "sina_by_continuous")

# cyl == 4 & vs == 0 is a single car. No density can be estimated from it, but
# unlike type_violin() the default draws the observation (on its tick) rather
# than discarding it.
f = function() {
  plt(mpg ~ cyl, facet = ~vs, data = mtcars, type = "sina", pch = 16)
}
expect_snapshot_plot(f, label = "sina_singletons_keep")

# the default "quasirandom" method never touches the RNG, so repeated calls are
# identical without a seed (c.f. `type_jitter()`); "random" does draw
set.seed(42)
seed_before = .Random.seed
plt(count ~ spray, data = InsectSprays, type = "sina")
expect_identical(.Random.seed, seed_before)
plt(count ~ spray, data = InsectSprays, type = type_sina(method = "random"))
expect_false(identical(.Random.seed, seed_before))

expect_error(type_sina(method = "nope"))
