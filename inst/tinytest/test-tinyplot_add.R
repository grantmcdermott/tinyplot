source("helpers.R")
using("tinysnapshot")

## error message cannot be tested in this suite
# expect_error(tinyplot_add(type = "p"), pattern = "No previous tinyplot")

f = function() {
  tinyplot(Sepal.Width ~ Sepal.Length | Species,
    facet = ~Species,
    data = iris,
    type = "p")
  tinyplot_add(type = type_lm())
}
expect_snapshot_plot(f, label = "tinyplot_add")


f = function() {
  k = seq(-3, 3, length.out = 100)
  tinyplot(x = k, type = type_function(dnorm))
  tinyplot_add(
    x = k[k < -1.96],
    ymax = dnorm(k[k < -1.96]),
    ymin = 0,
    type = "ribbon")
  tinyplot_add(
    x = k[k > 1],
    ymax = dnorm(k[k > 1]),
    ymin = 0,
    type = "ribbon")
}
expect_snapshot_plot(f, label = "tinyplot_add_multiple")


# allow first argument to be unnamed
f = function() {
  tinyplot(mpg ~ hp, type = "lm", data = mtcars)
  tinyplot_add(mpg ~ hp | factor(cyl), type = "p", pch = 16)
}
expect_snapshot_plot(f, label = "tinyplot_add_unnamed")

# type = "rug"
set.seed(48103)
f = function() {
  tinyplot(eruptions ~ waiting, data = faithful, type = "lm")
  tinyplot_add(type = "rug")
  tinyplot_add(type = type_rug(side = 2, jitter = TRUE, amount = 0.1))
}
expect_snapshot_plot(f, label = "tinyplot_add_rug")

# type = "rug" (adding to "density" should default to x variable)
f = function() {
  tinyplot(~eruptions, data = faithful, type = "density")
  tinyplot_add(type = "rug")
}
expect_snapshot_plot(f, label = "tinyplot_add_rug_density")


# type = "rug" (adding to "density" should default to x variable)
f = function() {
  tinyplot(~eruptions, data = faithful, type = "density")
  tinyplot_add(type = "rug")
}
expect_snapshot_plot(f, label = "tinyplot_add_rug_density")


# use tinyplot_add() inside a custom function with local variables
tinyplot_lollipop = function(x, y) {
  tinyplot(x, y, type = "h")
  tinyplot_add(type = "p", pch = 19)
  tinyplot_add(type = "hline")
}
f = function() {
  tinyplot_lollipop(1:5, sin(1:5))
}
expect_snapshot_plot(f, label = "tinyplot_lollipop")


# use tinyplot_add() after do.call(tinyplot)
f = function() {
  d = data.frame(x = 1:5, y = sin(1:5))
  do.call(tinyplot, list(y ~ x, data = d))
  do.call(tinyplot_add, list(type = "h"))
}
expect_snapshot_plot(f, label = "tinyplot_do_call")


# check that we are avoiding recursive margins for facets, by properly restoring
# the original state
f = function() {
  tinyplot(Sepal.Length ~ Petal.Length, facet = ~Species, data = iris)
  tinyplot_add(type = "lm")
  tinyplot(Sepal.Width ~ Sepal.Length, facet = ~Species, data = iris)
  tinyplot_add(type = "lm")
}
expect_snapshot_plot(f, label = "tinyplot_add_no_recursive_margins")
