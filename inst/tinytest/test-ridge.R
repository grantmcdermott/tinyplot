source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge")
}
expect_snapshot_plot(f, label = "ridge_basic")
tinytheme("ridge")
expect_snapshot_plot(f, label = "ridge_basic_theme_ridge")
tinytheme("ridge2")
expect_snapshot_plot(f, label = "ridge_basic_theme_ridge2")
tinytheme()

f = function() {
  tinyplot(
    Species ~ Sepal.Width, data = iris,
    type = type_ridge(alpha = 0.5)
  )
}
expect_snapshot_plot(f, label = "ridge_alpha")

f = function() {
  tinyplot(
    Species ~ Sepal.Width, data = iris,
    main = 'joint.bw = "mean"',
    type = type_ridge(joint.bw = "mean")
  )
}
expect_snapshot_plot(f, label = "ridge_joint_mean")

f = function() {
  tinyplot(
    Species ~ Sepal.Width, data = iris,
    main = 'joint.bw = "full"',
    type = type_ridge(joint.bw = "full")
  )
}
expect_snapshot_plot(f, label = "ridge_joint_full")

f = function() {
  tinyplot(
    Species ~ Sepal.Width, data = iris,,
    main = 'joint.bw = "none"',
    type = type_ridge(joint.bw = "none")
  )
}
expect_snapshot_plot(f, label = "ridge_joint_none")


f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris,
    type = type_ridge(scale = 1),
    bg = "light blue")
}
expect_snapshot_plot(f, label = "ridge_scale")

f = function() {
  tinyplot(am ~ mpg | factor(cyl), data = mtcars, type = "ridge")
}
expect_snapshot_plot(f, label = "ridge_by")

# special by cases

## by == y
f = function() {
  tinyplot(Species ~ Sepal.Width | Species, data = iris, type = "ridge")
}
expect_snapshot_plot(f, label = "ridge_by_y")
tinytheme("ridge")
expect_snapshot_plot(f, label = "ridge_by_y_theme_ridge")
tinytheme("ridge2")
expect_snapshot_plot(f, label = "ridge_by_y_theme_ridge2")
tinytheme()

## by == x
f = function() {
  tinyplot(Species ~ Sepal.Width | Sepal.Width, data = iris,
           type = type_ridge(col = "white"), palette = "plasma")
}
expect_snapshot_plot(f, label = "ridge_by_x")
tinytheme("ridge")
expect_snapshot_plot(f, label = "ridge_by_x_theme_ridge")
tinytheme("ridge2")
expect_snapshot_plot(f, label = "ridge_by_x_theme_ridge2")
tinytheme()

# "manual" gradients

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = type_ridge(gradient = TRUE))
}
expect_snapshot_plot(f, label = "ridge_gradient")
tinytheme("ridge")
expect_snapshot_plot(f, label = "ridge_gradient_theme_ridge")
tinytheme("ridge2")
expect_snapshot_plot(f, label = "ridge_gradient_theme_ridge2")
tinytheme()

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

f = function() {
  tinyplot(
    am ~ mpg, facet = ~vs, data = mtcars,
    type = type_ridge(gradient = "agsunset"),
    col = "white"
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_facet")
tinytheme("ridge")
expect_snapshot_plot(f, label = "ridge_gradient_facet_theme_ridge")
tinytheme("ridge2")
expect_snapshot_plot(f, label = "ridge_gradient_facet_theme_ridge2")
tinytheme()

f = function() {
  tinyplot(
    am ~ mpg, facet = ~vs, data = mtcars,
    type = type_ridge(gradient = "agsunset", raster = TRUE, alpha = 0.5),
    col = "white"
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_facet_raster_alpha")



f = function() {
  tinyplot(
    am ~ mpg, facet = ~vs, data = mtcars,
    type = type_ridge(gradient = "agsunset", raster = TRUE, alpha = 0.5),
    col = "white"
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_facet_raster_alpha")