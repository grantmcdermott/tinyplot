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
  tinyplot(Sepal.Width ~ Sepal.Length, data = iris,
    facet = ~Species, facet.args = list(cex = 2, ncol = 1),
    theme = "clean")
}
expect_snapshot_plot(f, label = "facet_cex_large")

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
    ~Ozone, aq,
    type = "density",
    facet = 1 ~ hot:windy,
    main = "Ozone pollution is worse on hot, calm days"
  )
}
expect_snapshot_plot(f, label = "facet_density_formula_1row")

f = function() {
  tinyplot(
    ~Ozone, aq,
    type = "density",
    facet = hot:windy ~ 1,
    main = "Ozone pollution is worse on hot, calm days"
  )
}
expect_snapshot_plot(f, label = "facet_density_formula_1col")

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

# Free facets with different y-axis scales (issue #570)
f = function() {
  dat = data.frame(
    x = rep(0:10, times = 2),
    y = c(0:10, 1000000:1000010),
    f = rep(c("A", "B"), each = 11)
  )
  tinyplot(
    y ~ x, data = dat,
    facet = ~f, facet.args = list(free = TRUE),
    theme = "clean",
    main = "Free facets: different y scales"
  )
}
expect_snapshot_plot(f, label = "facet_free_yscale")

# Free facets where a facet has a single distinct (discrete) axis value, which
# collapses the free-scale range to zero width (issue #668)
f = function() {
  dat = data.frame(
    x = c("a", "b", "b"),
    y = c(1, 2, 3),
    g = c("A", "B", "B")
  )
  tinyplot(
    y ~ x, data = dat,
    facet = ~g, facet.args = list(free = TRUE),
    main = "Free facets: single-value facet"
  )
}
expect_snapshot_plot(f, label = "facet_free_single_value")

# Free facets combined with flip: the fixed continuous-axis limit must follow
# the flip and be honoured, rather than the wrong axis being freed (issue #670)
f = function() {
  tinyplot(
    mpg ~ carb, data = mtcars, type = "p",
    facet = ~am, facet.args = list(free = TRUE),
    ylim = c(0, 50),
    flip = TRUE,
    main = "Free facets: flipped"
  )
}
expect_snapshot_plot(f, label = "facet_free_flip")

# `axes = "outer"` must also close up the whitespace that the dropped interior
# axes would have occupied, i.e. match the spacing of a frameless plot rather
# than leaving a gap behind (#637, #673). Testing with "float", which should
# defaults to `facet.axes = "outer"` as part of its tpar settings.
f = function() {
  tinyplot(
    mpg ~ wt, data = mtcars,
    facet = ~am:vs,
    theme = "float"
  )
}
expect_snapshot_plot(f, label = "facet_axes_outer")

# Same, but for a directional `bty` (the L-shaped frame of "classic"), and
# relying on the theme's own `facet.axes = "outer"` default rather than passing
# it per call. The interior frame edges have to be dropped alongside the axes,
# else they float in the gutter without an axis to anchor them.
f = function() {
  tinyplot(
    mpg ~ wt, data = mtcars,
    facet = ~am:vs,
    theme = "classic"
  )
}
expect_snapshot_plot(f, label = "facet_axes_outer_classic")

# The global parameter should work through an ephemeral theme too, i.e. suppress
# the interior axes of an otherwise framed theme.
f = function() {
  tinyplot(
    mpg ~ wt, data = mtcars,
    facet = ~am:vs,
    theme = list("dynamic", facet.axes = "outer")
  )
}
expect_snapshot_plot(f, label = "facet_axes_outer_tpar")

## Opposite for themes that normally suppress; override with all axes
f = function() {
  tinyplot(
    mpg ~ wt, data = mtcars,
    facet = ~am:vs, facet.args = list(axes = "all"),
    theme = "classic"
  )
}
expect_snapshot_plot(f, label = "facet_axes_all_classic_override")

# Free scales are the exception: every panel has its own scale, so each keeps
# its own axes and `axes = "outer"` is (deliberately) a no-op. Guards against
# the outer-axis rule stripping axes that a free panel needs to be readable.
f = function() {
  tinyplot(
    mpg ~ wt, data = mtcars,
    facet = ~am:vs, facet.args = list(free = TRUE),
    theme = "classic"
  )
}
expect_snapshot_plot(f, label = "facet_axes_outer_free")

# Free facets with categories on the y-axis used to error out with
# "'labels' is supplied and not 'at'": the eligible types were listed by name,
# so anything else lost its tick positions but kept the labels. Levels are set
# in data order here, so the snapshot does not depend on how a type orders its
# categories. (#679)
f = function() {
  LOTR = data.frame(
    name = rep(c("Fellowship", "Two Towers", "Return"), 2),
    runtime = c(178, 179, 201, 208, 223, 251),
    cut = rep(c("theatrical", "extended"), each = 3)
  )
  LOTR$name = factor(LOTR$name, levels = unique(LOTR$name))
  tinyplot(
    runtime ~ name, facet = ~cut, data = LOTR, type = "b",
    flip = TRUE, facet.args = list(free = TRUE)
  )
}
expect_snapshot_plot(f, label = "facet_free_categorical_yaxis")

#
# restore original par settings
#

par(op)
