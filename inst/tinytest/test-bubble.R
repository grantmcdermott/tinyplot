source("helpers.R")
using("tinysnapshot")

#
## single (combined) legend -----
#

# no by groups & continuous cex
f = function() tinyplot(
  Sepal.Length ~ Petal.Length,
  data = iris,
  cex = iris$Petal.Width
)
expect_snapshot_plot(f, label = "bubble_simple")

# ditto, but legend at bottom
f = function() tinyplot(
  Sepal.Length ~ Petal.Length,
  data = iris,
  cex = iris$Petal.Width,
  legend = "bottom!"
)
expect_snapshot_plot(f, label = "bubble_simple_bottom")

# check custom expansion range and alpha
f = function() tinyplot(
  Sepal.Length ~ Petal.Length,
  data = iris,
  cex = iris$Petal.Width, clim = c(1, 6),
  pch = 19, alpha = 0.3
)
expect_snapshot_plot(f, label = "bubble_simple_clim")

# discrete by groups & cex = "by"
f = function() tinyplot(
  Sepal.Length ~ Petal.Length | Species,
  data = iris,
  cex = "by"
)
expect_snapshot_plot(f, label = "bubble_cex_by")

#
## dual legends -----
#

# discrete by groups & continuous cex
f = function() tinyplot(
  Sepal.Length ~ Petal.Length | Species,
  data = iris,
  cex = iris$Petal.Width
)
expect_snapshot_plot(f, label = "bubble_dual_discrete")

# continuous x continuous
f = function() tinyplot(
  Sepal.Length ~ Petal.Length | Sepal.Width,
  data = iris,
  cex = iris$Petal.Width
)
expect_snapshot_plot(f, label = "bubble_dual_continuous")

# fancy version with a bunch of customizations
f = function() tinyplot(
  Sepal.Length ~ Petal.Length | Sepal.Width,
  data = iris,
  cex = iris$Petal.Width, clim = c(1, 6),
  pch = 22, bg = 0.3,
  legend = "left!"
)
expect_snapshot_plot(f, label = "bubble_dual_fancy")