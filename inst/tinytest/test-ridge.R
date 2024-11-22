source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge", col = "white")
}
expect_snapshot_plot(f, label = "ridge_01")

f = function() {
  tinyplot(Month ~ Ozone,
    data = airquality,
    type = type_ridge(scale = 1),
    bg = "light blue", col = "black")
}
expect_snapshot_plot(f, label = "ridge_02")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = TRUE))
}
expect_snapshot_plot(f, label = "ridge_gradient")

f = function() {
  tinyplot(
    Species ~ Sepal.Width, data = iris,
    type = type_ridge(gradient = TRUE, breaks = seq(2, 4.5, by = 0.5))
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_discrete")

f = function() {
  tinyplot(
    Species ~ Sepal.Width, data = iris,
    type = type_ridge(
      gradient = hcl.colors(250, "Dark Mint")[c(250:1, 1:250)],
      probs = 0:500/500
    )
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_probs")

tinyplot(
  am ~ mpg, facet = ~vs, data = mtcars, type = type_ridge(gradient = "agsunset"),
  col = "white"
)
expect_snapshot_plot(f, label = "ridge_gradient_facet")
