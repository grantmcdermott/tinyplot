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
tinytheme("clean")

f = function() tinyplot(
  mpg ~ hp | factor(am), data = mtcars,
  main = "Title of the plot",
  sub = 'tinytheme("clean") + legend = "left!"',
  legend = "left!"
)
expect_snapshot_plot(f, label = "tinytheme_legend_left")

f = function() tinyplot(
  mpg ~ hp | factor(am), data = mtcars,
  main = "Title of the plot",
  sub = 'tinytheme("clean") + legend = "bottom!"',
  legend = "bottom!"
)
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

# ridge and spineplot types (req's extra steps b/c of tinyAxis logic)

tinytheme('clean')

f = function() {
  tinyplot(
    Species ~ Petal.Length, data = iris, type = "ridge",
    main = "Dynamic plot adjustment and whitespace reduction",
    sub = "Ridge plot version"
  )
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_clean_ridge")

f = function() {
  tinyplot(
    Species ~ Petal.Length, data = iris, type = "spineplot",
    main = "Dynamic plot adjustment and whitespace reduction",
    sub = "Spineplot version"
  )
}
expect_snapshot_plot(f, label = "tinytheme_dynamic_clean_spineplot")

#
## reset

tinytheme()
