source("helpers.R")
using("tinysnapshot")

tinytheme()

thms = eval(formals(tinytheme)$theme)

for (thm in thms) {
  tinytheme(thm)
  f = function() tinyplot(
    mpg ~ hp | factor(am), data = mtcars,
    main = "Title of the plot",
    sub = paste0('tinytheme("', thm, '")')
  )
  expect_snapshot_plot(f, label = paste0("tinytheme_", thm))
}
rm(thm)

# Single-group (no `by`) displays: safeguard the per-theme default colour
# logic, i.e. col.default and the leading-black palette drop. (#598)
for (thm in thms) {
  tinytheme(thm)
  f = function() tinyplot(
    mpg ~ hp, data = mtcars,
    main = "Title of the plot",
    sub = paste0('tinytheme("', thm, '")')
  )
  expect_snapshot_plot(f, label = paste0("tinytheme_single_", thm))
}
rm(thm)

# legend placement

f = function() {
  tinytheme("clean")
  tinyplot(
    mpg ~ hp | factor(am), data = mtcars,
    main = "Title of the plot",
    sub = 'tinytheme("clean") + legend = "left!"',
    legend = "left!"
  )
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_legend_left")

f = function() {
  tinytheme("clean")
  tinyplot(
    mpg ~ hp | factor(am), data = mtcars,
    main = "Title of the plot",
    sub = 'tinytheme("clean") + legend = "bottom!"',
    legend = "bottom!"
  )
  tinytheme()
}
expect_snapshot_plot(f, label = paste0("tinytheme_legend_bottom"))

#
## Dynamic plots

f = function() {
  tinyplot(
    I(Sepal.Length*1e9) ~ Petal.Length | Species, data = iris,
    main = "Dynamic plot adjustment and whitespace reduction",
    sub = "For themes with las = 1, etc."
  )
}

tinytheme("dynamic")
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_dynamic")

tinytheme("clean")
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_clean")

tinytheme("dark")
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_dark")

# x-axis adjustment
f = function() {
  tinytheme('clean', las = 2)
  tinyplot(weight ~ feed, data = chickwts, type = "boxplot",
           main = "Dynamic plot adjustment and whitespace reduction",
           sub = "Works for perpendicular x-axis labels too")
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_x_boxplot")

# facets
f = function() {
  tinyplot(
    I(mpg*1e3) ~ I(hp*1e2) | disp, data = mtcars, facet = cyl ~ am,
    main = "Dynamic plot adjustment and whitespace reduction",
    sub = "Works with facets too"
  )
}
tinytheme("clean", las = 2)
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_clean_facet")

tinytheme("dark", las = 2)
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_dark_facet")

tinytheme()

# variation with formatted tick labels
f = function() {
  tinytheme("clean")
  plt(
    I(decrease/100) ~ treatment, data = OrchardSprays,
    yaxl = "percent"
  )
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_yaxl")


# flipped jitter and boxplot use special internal logic (because of integer spacing)

f = function() {
  tinytheme('clean')
  set.seed(99)
  tinyplot(weight ~ feed, data = chickwts, type = "jitter", flip = TRUE,
           main = "Dynamic plot adjustment and whitespace reduction",
           sub = "Flipped jitter plot version")
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_jitter_flip")

f = function() {
  tinytheme('clean')
  tinyplot(weight ~ feed, data = chickwts, type = "boxplot", flip = TRUE,
           main = "Dynamic plot adjustment and whitespace reduction",
           sub = "Flipped boxplot version")
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_boxplot_flip")

# ridge and spineplot types (req's extra steps b/c of tinyAxis logic)

f = function() {
  tinytheme('ridge')
  tinyplot(
    Species ~ Petal.Length, data = iris, type = "ridge",
    main = "Dynamic plot adjustment and whitespace reduction",
    sub = "Ridge plot version"
  )
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_ridge")


f = function() {
  tinytheme('clean')
  tinyplot(
    Species ~ Petal.Length, data = iris, type = "spineplot",
    main = "Dynamic plot adjustment and whitespace reduction",
    sub = "Spineplot version"
  )
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_clean_spineplot")

#
## reset

tinytheme()


## palette functions (#593)

pal = colorRampPalette(c("darkblue", "deeppink", "cornsilk"))

f = function () {
  tinytheme("clean", palette.sequential = pal, pch = 21)
  tinyplot(1:9, by = 1:9, cex = 3, lwd = 3, bg = "by")
}
expect_snapshot_plot(f, label = "tinytheme_palette_function_sequential")

f = function () {
  tinytheme("clean", palette.qualitative = pal, pch = 21)
  tinyplot(1:9, by = factor(1:9), cex = 3, lwd = 3, bg = "by")
}
expect_snapshot_plot(f, label = "tinytheme_palette_function_qualitative")


#
## reset

tinytheme()


#
## ephemeral theme

f = function() {
  opar = par(mfrow = c(1, 2))
  plt(Sepal.Length ~ Petal.Length | Species, data = iris,
      main = "Ephemeral theme", theme = "clean", legend = FALSE)
  plt_add(type = "lm")
  plt(Sepal.Length ~ Petal.Length | Species, data = iris,
      main = "Revert to old theme", legend = FALSE)
  plt_add(type = "lm")
  par(opar)
}
expect_snapshot_plot(f, label = "tinytheme_ephemeral")

# Ephemeral "default" theme with by + plt_add should not clip (#557)
f = function() {
  plt(1:3, c(1, 1, 1), by = c("a", "a", "a"), theme = "default", type = "n")
  plt_add(type = "b")
}
expect_snapshot_plot(f, label = "ephemeral_default_theme_add")

# Layers added after an ephemeral theme should not be clipped to the wrong
# region once an intervening annotation has toggled `xpd` (#629)
f = function() {
  plt(0, 0, theme = "classic")
  plt_add(par("usr")[1], y = 0.3, type = type_text(labels = "foo", xpd = NA, pos = 2))
  plt_add(type = type_hline(0.3))
}
expect_snapshot_plot(f, label = "tinytheme_ephemeral_clip_xpd")

## an ephemeral theme should leave no trace (#739)

pdf(NULL)

# a persistent theme survives an ephemeral one
tinytheme("classic")
plt(1, theme = "dark")
expect_equal(tinytheme_get(), "classic")

# as do the user's own tpar() settings
tpar(grid = TRUE)
plt(1, theme = "dark")
expect_true(isTRUE(tpar("grid")))

tinytheme()
invisible(dev.off())


# User mar override respected under dynmar (#587)
f = function() {
  tinytheme("dynamic", mar = c(5, 5, 5, 5))
  plt(0:10, main = "Custom mar override")
  box("inner", lty = 2)
  tinytheme()
}
expect_snapshot_plot(f, label = "tinytheme_dynmar_mar_override")

## palette functions (#593)
pal = colorRampPalette(c("darkblue", "deeppink", "cornsilk"))

f = function () {
  tinyplot(1:9, by = 1:9, cex = 3, lwd = 3, bg = "by",
    theme = list("clean", palette.sequential = pal, pch = 21))
}
expect_snapshot_plot(f, label = "tinytheme_ephemeral_palette_function_sequential")

f = function () {
  tinyplot(1:9, by = factor(1:9), cex = 3, lwd = 3, bg = "by",
    theme = list("clean", palette.qualitative = pal, pch = 21))
}
expect_snapshot_plot(f, label = "tinytheme_ephemeral_palette_function_qualitative")


# User palette override should not be trimmed by a theme's negative
# col.default (e.g. bw's -1), which would silently recycle colours. (#627)
f = function() {
  plt(
    Sepal.Length ~ Petal.Length | Species, iris,
    theme = list("bw", palette.qualitative = c("red", "blue", "green"))
  )
}
expect_silent(f())


## tinytheme_get() (#629)

# a bare reset reports the default theme
tinytheme()
expect_equal(tinytheme_get(), "default")

tinytheme("classic")
expect_equal(tinytheme_get(), "classic")

# the save/restore idiom the accessor exists for
otheme = tinytheme_get()
tinytheme("bw")
expect_equal(tinytheme_get(), "bw")
tinytheme(otheme)
expect_equal(tinytheme_get(), "classic")

# extra tpar overrides don't change the reported theme name
tinytheme("ipsum", las = 2)
expect_equal(tinytheme_get(), "ipsum")

# registered themes report their own name
tinytheme_register("float3", theme = "float", grid = TRUE)
tinytheme("float3")
expect_equal(tinytheme_get(), "float3")
tinytheme_unregister("float3")

tinytheme()
