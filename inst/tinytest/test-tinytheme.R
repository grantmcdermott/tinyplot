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

tinytheme("clean")
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_clean")

tinytheme("dark")
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_dark")

f = function() {
  tinyplot(
    I(mpg*1e3) ~ hp | disp, data = mtcars, facet = cyl ~ am,
    main = "Dynamic plot adjustment and whitespace reduction",
    sub = "Works with facets too"
  )
}
tinytheme("clean")
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_clean_facet")

tinytheme("dark")
f()
expect_snapshot_plot(f, label = "tinytheme_dynamic_dark_facet")

tinytheme()

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
