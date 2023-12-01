source("helpers.R")
using("tinysnapshot")

op = par(no.readonly = TRUE)

#
## simple scatterplot cases first

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      by = cyl, facet = "by"
    )
  )
}
expect_snapshot_plot(f, label = "facet_by_equal")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      by = am, facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_by")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      by = am, facet = cyl,
      pch = 19, palette = "dark2",
      grid = TRUE, frame = FALSE,
      main = "Car efficiency",
      xlab = "Weight", ylab = "MPG",
      legend = list(title = "Transmission"),
      sub = "Notes: Broken out by cylinder and transmission"
    )
  )
}
expect_snapshot_plot(f, label = "facet_fancy")


#
## Density plot versions

f = function() {
  with(
    mtcars,
    plot2(
      x = mpg,
      type = "density",
      facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_density")

f = function() {
  with(
    mtcars,
    plot2(
      x = mpg,
      type = "density",
      by = cyl, facet = "by"
    )
  )
}
expect_snapshot_plot(f, label = "facet_density_by_equal")

f = function() {
  with(
    mtcars,
    plot2(
      x = mpg,
      type = "density",
      by = am, facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_density_by")

f = function() {
  with(
    mtcars,
    plot2(
      x = mpg,
      type = "density",
      by = am, facet = cyl,
      fill = "by", palette = "dark2",
      grid = TRUE, frame = FALSE,
      main = "Car efficiency",
      # xlab = "Weight", ylab = "MPG",
      legend = list(title = "Transmission"),
      sub = "Notes: Broken out by cylinder and transmission"
    )
  )
}
expect_snapshot_plot(f, label = "facet_density_fancy")


#
## Ribbon plot versions

mod1 = lm(mpg ~ wt * factor(cyl), mtcars)
mtcars1 = cbind(mtcars, predict(mod1, newdata = mtcars, interval = "confidence"))
mod2 = lm(mpg ~ wt * factor(cyl) * factor(am), mtcars)
mtcars2 = cbind(mtcars, predict(mod2, newdata = mtcars, interval = "confidence"))

f = function() {
  with(
    mtcars1,
    plot2(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon",
      facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_ribbon")

f = function() {
  
  with(
    mtcars1,
    plot2(
      x = wt, y = mpg,
      facet = cyl
    )
  )
  with(
    mtcars1,
    plot2(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon",
      facet = cyl,
      add = TRUE
    )
  )
}
expect_snapshot_plot(f, label = "facet_ribbon_add")


f = function() {
  with(
    mtcars1,
    plot2(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon",
      by = cyl, facet = "by"
    )
  )
}
expect_snapshot_plot(f, label = "facet_ribbon_by_equal")

f = function() {
  with(
    mtcars2,
    plot2(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon",
      by = am, facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_ribbon_by")

f = function() {
  with(
    mtcars2,
    plot2(
      x = wt, y = mpg,
      by = am, facet = cyl,
      palette = "dark2",
      grid = TRUE, #frame = FALSE,
      main = "Car efficiency",
      xlab = "Weight", ylab = "MPG",
      legend = list(title = "Transmission"),
      sub = "Notes: Broken out by cylinder and transmission"
    )
  )
  with(
    mtcars2,
    plot2(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon",
      by = am, facet = cyl,
      palette = "dark2",
      add = TRUE
    )
  )
}
expect_snapshot_plot(f, label = "facet_ribbon_fancy_add")





#
# restore original par settings
#

par(op)