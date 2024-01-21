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
      facet = am
    )
  )
}
expect_snapshot_plot(f, label = "facet_1x2")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = am,
      facet.args = list(ncol = 1)
    )
  )
}
expect_snapshot_plot(f, label = "facet_2x1")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = interaction(am, vs)
    )
  )
}
expect_snapshot_plot(f, label = "facet_2x2")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = interaction(cyl, am),
      main = "Facet with interaction"
    )
  )
}
expect_snapshot_plot(f, label = "facet_interaction")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = interaction(cyl, am, sep = "\n"),
      main = "Facet with interaction"
    )
  )
}
expect_snapshot_plot(f, label = "facet_interaction_newline")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = interaction(cyl, am),
      facet.args = list(ncol = 2),
      main = "Facet with user-defined ncol"
    )
  )
}
expect_snapshot_plot(f, label = "facet_args_ncol")

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

## Skip failing test in R devel due to some minor esoteric difference coming up 
## in R 4.4.0. Can revert once it reaches release for local testing.
if (getRversion()  <= "4.3.2") {
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
}


#
## facet margins (fmar)

f = function() {
  ofmar = par2("fmar")
  par2(fmar = c(1,1,0.5,2))
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = interaction(cyl, am)
    )
  )
  par2(fmar = ofmar)
}
expect_snapshot_plot(f, label = "facet_fmar_par2")

f = function() {
  with(
    mtcars,
    plot2(
      x = wt, y = mpg,
      facet = interaction(cyl, am),
      facet.args = list(fmar = c(1,1,0.5,2))
    )
  )
}
expect_snapshot_plot(f, label = "facet_fmar_args")

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


## Skip failing test in R devel due to some minor esoteric difference coming up 
## in R 4.4.0. Can revert once it reaches release for local testing.
if (getRversion()  <= "4.3.2") {
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
}


#
## Density plot versions

# restore original par settings
par(op)

## Sidestep test fails due to new (R 4.4.0) density grid value calculations.
## https://bugs.r-project.org/show_bug.cgi?id=18337
exit_if_not(getRversion()  <= "4.3.2")
## Note: Once 4.4.0 is released we can either generate some new plots or
## test with something like:
# f = function() plot2(density(mtcars$mpg, old.coords=TRUE))

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

## Skip failing test in R devel due to some minor esoteric difference coming up 
## in R 4.4.0. Can revert once it reaches release for local testing.
if (getRversion()  <= "4.3.2") {
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
}


#
# restore original par settings
#

par(op)