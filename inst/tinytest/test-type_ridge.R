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
           type = type_ridge(col = "white"))
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
    type = type_ridge(gradient = "agsunset")
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_facet")
tinytheme("ridge")
expect_snapshot_plot(f, label = "ridge_gradient_facet_theme_ridge")
tinytheme("ridge2")
expect_snapshot_plot(f, label = "ridge_gradient_facet_theme_ridge2")
tinytheme()

# Dedicated test for a white boundary color between gradient ridges, which is a
# common aesthetic for separating overlapping densities. (#598)
f = function() {
  tinyplot(
    am ~ mpg, facet = ~vs, data = mtcars,
    type = type_ridge(gradient = TRUE),
    col = "white",
    theme = "ridge"
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_white_facet_theme_ridge")


f = function() {
  tinyplot(
    am ~ mpg, facet = ~vs, data = mtcars,
    type = type_ridge(gradient = "agsunset", raster = TRUE, alpha = 0.5)
  )
}
expect_snapshot_plot(f, label = "ridge_gradient_facet_raster_alpha")

# Issue #547: ridge with themed palette should not error
f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge", theme = "clean2")
}
expect_snapshot_plot(f, label = "ridge_theme_palette")

# Issue #547: numeric bg with ridge theme should produce transparent gray
f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge", theme = "ridge2", bg = 0.2)
}
expect_snapshot_plot(f, label = "ridge_theme_bg_numeric")

# Issue #650: ylab = NA should suppress the y-axis title without erroring.
# Ridge draws its own y-axis labels, so the tick-row margin must still be
# reserved under dynamic themes (else mar collapses and par() errors).
f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris,
    type = "ridge", theme = "ridge", ylab = NA)
}
expect_snapshot_plot(f, label = "ridge_ylab_na_issue650")


#
## singleton groups (#300)

# cyl == 4 & vs == 0 is a single car, so no density can be estimated for it.
# The default reports the loss; "drop" does the same thing quietly.
expect_warning(
  plt(cyl ~ mpg, facet = ~vs, data = mtcars, type = "ridge"),
  pattern = "Dropped 1 singleton"
)

f = function() {
  plt(cyl ~ mpg, facet = ~vs, data = mtcars, type = type_ridge(singletons = "drop"))
}
expect_snapshot_plot(f, label = "ridge_singletons_drop")
expect_error(
  plt(cyl ~ mpg, facet = ~vs, data = mtcars, type = type_ridge(singletons = "none")),
  pattern = "at least 2 data points"
)
expect_error(type_ridge(singletons = "nope"))


#
## yord -----

# ridges rank on the continuous `x`, since there is no separate response
f = function() tinyplot(Species ~ Sepal.Length, data = iris, type = type_ridge(yord = "minvar"))
expect_snapshot_plot(f, label = "ridge_yord_minvar")

f = function() tinyplot(Species ~ Sepal.Length, data = iris, type = type_ridge(yord = "rev"))
expect_snapshot_plot(f, label = "ridge_yord_rev")

# a transposed formula leaves nothing numeric to rank on; say so plainly
expect_error(
  tinyplot(Sepal.Length ~ Species, data = iris, type = type_ridge(yord = "minvar")),
  pattern = "ranks on a numeric variable"
)
# ...including when the ranking is a function, which cannot be interpolated
# into the message the way a keyword can
expect_error(
  tinyplot(Sepal.Length ~ Species, data = iris,
           type = type_ridge(yord = function(z) -mean(z))),
  pattern = "ranks on a numeric variable"
)

expect_error(
  tinyplot(Species ~ Sepal.Length, data = iris, type = type_ridge(yord = "start")),
  pattern = "not available for this plot type"
)


#
## yaxl -----

# ridge draws its own y-axis category labels, so `yaxl` has to be carried
# through `type_info` to the tinyAxis() calls rather than picked up by the
# standard axis path
f = function() tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge", yaxl = toupper)
expect_snapshot_plot(f, label = "ridge_yaxl_toupper")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge",
           yaxl = c(setosa = "SET", virginica = "VIR"))
}
expect_snapshot_plot(f, label = "ridge_yaxl_dict")
