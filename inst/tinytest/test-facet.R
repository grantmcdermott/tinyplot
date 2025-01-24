source("helpers.R")
using("tinysnapshot")

op = par(no.readonly = TRUE)
mtcars$am = as.factor(mtcars$am)

#
## simple scatterplot cases first

f = function() {
  with(
    mtcars,
    tinyplot(
      x = wt, y = mpg,
      facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet")

f = function() {
  with(
    mtcars,
    tinyplot(
      x = wt, y = mpg,
      facet = am
    )
  )
}
expect_snapshot_plot(f, label = "facet_1x2")

f = function() {
  with(
    mtcars,
    tinyplot(
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
    tinyplot(
      x = wt, y = mpg,
      facet = interaction(am, vs)
    )
  )
}
expect_snapshot_plot(f, label = "facet_2x2")

f = function() {
  with(
    mtcars,
    tinyplot(
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
    tinyplot(
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
    tinyplot(
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
    tinyplot(
      x = wt, y = mpg,
      by = cyl, facet = "by"
    )
  )
}
expect_snapshot_plot(f, label = "facet_by_equal")

f = function() {
  with(
    mtcars,
    tinyplot(
      x = wt, y = mpg,
      by = am, facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_by")

if (getRversion() >= "4.4.0") {
  f = function() {
    with(
      mtcars,
      tinyplot(
        x = wt, y = mpg,
        by = am,
        facet = cyl, facet.args = list(bg = "grey90"),
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
  ofmar = tpar("fmar")
  tpar(fmar = c(1, 1, 0.5, 2))
  with(
    mtcars,
    tinyplot(
      x = wt, y = mpg,
      facet = interaction(cyl, am)
    )
  )
  tpar(fmar = ofmar)
}
expect_snapshot_plot(f, label = "facet_fmar_par2")

f = function() {
  with(
    mtcars,
    tinyplot(
      x = wt, y = mpg,
      facet = interaction(cyl, am),
      facet.args = list(fmar = c(1, 1, 0.5, 2))
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
    tinyplot(
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
    tinyplot(
      x = wt, y = mpg,
      facet = cyl
    )
  )
  with(
    mtcars1,
    tinyplot(
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
    tinyplot(
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
    tinyplot(
      x = wt, y = fit,
      ymin = lwr, ymax = upr,
      type = "ribbon",
      by = am, facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_ribbon_by")


if (getRversion() >= "4.4.0") {
  f = function() {
    with(
      mtcars2,
      tinyplot(
        x = wt, y = mpg,
        by = am, facet = cyl,
        palette = "dark2",
        grid = TRUE, # frame = FALSE,
        main = "Car efficiency",
        xlab = "Weight", ylab = "MPG",
        legend = list(title = "Transmission"),
        sub = "Notes: Broken out by cylinder and transmission"
      )
    )
    with(
      mtcars2,
      tinyplot(
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

## Avoid test fails on older R versions (pre 4.4.0) due to slight change in
## density grid value calculations.
## https://bugs.r-project.org/show_bug.cgi?id=18337
if (getRversion() < "4.4.0") exit_file("R < 4.4.0")

f = function() {
  with(
    mtcars,
    tinyplot(
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
    tinyplot(
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
    tinyplot(
      x = mpg,
      type = "density",
      by = am, facet = cyl
    )
  )
}
expect_snapshot_plot(f, label = "facet_density_by")


if (getRversion() >= "4.4.0") {
  f = function() {
    with(
      mtcars,
      tinyplot(
        x = mpg,
        type = "density",
        by = am,
        facet = cyl, facet.args = list(bg = "grey90"),
        fill = "by", palette = "dark2",
        grid = TRUE, frame = FALSE,
        main = "Car efficiency",
        legend = list(title = "Transmission"),
        sub = "Notes: Broken out by cylinder and transmission"
      )
    )
  }
  expect_snapshot_plot(f, label = "facet_density_fancy")
}


#
## facet (one-sided) formula versions

f = function() {
  tinyplot(
    mpg ~ wt,
    data = mtcars,
    facet = ~cyl
  )
}
expect_snapshot_plot(f, label = "facet_formula")

f = function() {
  tinyplot(
    mpg ~ wt,
    data = mtcars,
    facet = ~am
  )
}
expect_snapshot_plot(f, label = "facet_1x2_formula")

f = function() {
  tinyplot(
    mpg ~ wt,
    data = mtcars,
    facet = ~am,
    facet.args = list(ncol = 1)
  )
}
expect_snapshot_plot(f, label = "facet_2x1_formula")

f = function() {
  tinyplot(
    mpg ~ wt,
    data = mtcars,
    facet = ~ am:vs
  )
}
expect_snapshot_plot(f, label = "facet_2x2_formula")


if (getRversion() >= "4.4.0") {
  f = function() {
    tinyplot(
      ~ mpg | am, mtcars,
      type = "density",
      facet = ~cyl,
      fill = "by", palette = "dark2",
      grid = TRUE, frame = FALSE,
      main = "Car efficiency",
      legend = list(title = "Transmission"),
      sub = "Notes: Broken out by cylinder and transmission"
    )
  }
  expect_snapshot_plot(f, label = "facet_density_fancy_formula")
}


#
## facet grid (two-sided formula)

f = function() {
  tinyplot(
    mpg ~ wt,
    data = mtcars,
    facet = am ~ cyl,
    main = "facet grid",
    sub = "Notes: Transmission (rows) vs Cylinders (cols)"
  )
}
expect_snapshot_plot(f, label = "facet_grid")

f = function() {
  tinyplot(
    mpg ~ wt,
    data = mtcars,
    facet = am ~ cyl, facet.args = list(bg = adjustcolor("hotpink", 0.5)),
    log = "xy",
    main = "facet grid (logged axes)",
    sub = "Notes: Transmission (rows) vs Cylinders (cols)"
  )
}
expect_snapshot_plot(f, label = "facet_grid_log")

f = function() {
  tinyplot(
    mpg ~ wt,
    data = mtcars,
    facet = am + vs ~ gear,
    main = "facet grid multivar",
    sub = "Notes: Missing combos are still displayed correctly"
  )
}
expect_snapshot_plot(f, label = "facet_grid_multivar")

if (getRversion() >= "4.4.0") {
  f = function() {
    tinyplot(
      mpg ~ wt | factor(gear),
      data = mtcars,
      facet = am ~ cyl,
      facet.args = list(bg = "grey90"),
      pch = 19, palette = "classic",
      legend = list(title = "Gears"),
      main = "facet grid (fancy)",
      sub = "Notes: Transmission (rows) vs Cylinders (cols)",
      grid = TRUE, frame = FALSE,
      xlim = c(1, 6), ylim = c(10, 35)
    )
  }
  expect_snapshot_plot(f, label = "facet_grid_fancy")
}

aq = airquality
aq$hot = ifelse(aq$Temp >= 75, "hot", "cold")
aq$windy = ifelse(aq$Wind >= 15, "windy", "calm")

f = function() {
  tinyplot(
    ~Ozone, aq,
    type = "density",
    facet = ~ hot:windy,
    main = "Ozone pollution is worse on hot, calm days"
  )
}
expect_snapshot_plot(f, label = "facet_density_formula")

f = function() {
  tinyplot(
    ~Ozone, aq,
    type = "density",
    facet = windy ~ hot,
    main = "Ozone pollution is worse on hot, calm days"
  )
}
expect_snapshot_plot(f, label = "facet_density_grid")

f = function() {
  tinyplot(
    ~wt,
    data = mtcars,
    type = "hist",
    facet = cyl ~ am
  )
}
expect_snapshot_plot(f, label = "facet_hist_3x2")

f = function() {
  tinyplot(
    ~wt,
    data = mtcars,
    type = "density",
    facet = cyl ~ am
  )
}
expect_snapshot_plot(f, label = "facet_density_3x2")


#
# Free facet scales
#

f = function() {
  tinyplot(
    Solar.R~Ozone, aq,
    facet = ~ hot:windy,
    facet.args = list(free = TRUE),
    main = "Free facet scales"
  )
}
expect_snapshot_plot(f, label = "facet_free")

f = function() {
  tinyplot(
    Solar.R~Ozone, aq,
    facet = windy ~ hot,
    facet.args = list(free = TRUE),
    main = "Free facet scales (grid)"
  )
}
expect_snapshot_plot(f, label = "facet_free_grid")

#
# restore original par settings
#

par(op)
