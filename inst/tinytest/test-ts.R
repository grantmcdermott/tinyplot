source("helpers.R")
using("tinysnapshot")

## five random walks
set.seed(0)
x = c(0, rnorm(100)) |>
  cumsum() |>
  replicate(n = 5) |>
  ts(start = 0)

## univariate

f = function() {
  tinyplot(x[, 1])
}
expect_snapshot_plot(f, label = "ts-univariate")

## multivariate

f = function() {
  tinyplot(x)
}
expect_snapshot_plot(f, label = "ts-multivariate-free")

f = function() {
  tinyplot(x, facet.args = NULL)
}
expect_snapshot_plot(f, label = "ts-multivariate-same")

f = function() {
  tinyplot(x, facet = "by")
}
expect_snapshot_plot(f, label = "ts-multivariate-by-free")

f = function() {
  tinyplot(x, facet = "by", facet.args = NULL)
}
expect_snapshot_plot(f, label = "ts-multivariate-by-same")

f = function() {
  tinyplot(x, facet = NULL)
}
expect_snapshot_plot(f, label = "ts-multivariate-single")

f = function() {
  tinyplot(x, facet.args = list(free = TRUE, ncol = 1))
}
expect_snapshot_plot(f, label = "ts-multivariate-column")
