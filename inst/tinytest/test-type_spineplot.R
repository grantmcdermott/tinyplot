source("helpers.R")
using("tinysnapshot")

ttnc = as.data.frame(Titanic)

#
## simple spineplot

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "spineplot")
}
expect_snapshot_plot(f, label = "spineplot_simple")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = type_spineplot(breaks = 4))
}
expect_snapshot_plot(f, label = "spineplot_breaks")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = type_spineplot(breaks = 4), flip = TRUE)
}
expect_snapshot_plot(f, label = "spineplot_breaks_flip")

f = function() {
  tinyplot(Survived ~ Sex, data = ttnc, type = type_spineplot(weights = ttnc$Freq))
}
expect_snapshot_plot(f, label = "spineplot_weights")


#
## grouped boxplots

# special cases for x==by and y==by
f = function() {
  tinyplot(
    Survived ~ Sex | Sex, data = ttnc,
    type = type_spineplot(weights = ttnc$Freq),
    palette = "tableau"
  )
}
expect_snapshot_plot(f, label = "spineplot_xby")
f = function() {
  tinyplot(
    Species ~ Sepal.Width | Species, data = iris,
    type = type_spineplot(breaks = 4),
    palette = "Pastel 1"
  )
}
expect_snapshot_plot(f, label = "spineplot_yby")


#
## facets

f = function() {
  tinyplot(
    Survived ~ Sex, facet = ~Class, data = ttnc,
    type = type_spineplot(weights = ttnc$Freq)
  )
}
expect_snapshot_plot(f, label = "spineplot_facet")

f = function() {
  tinyplot(
    Survived ~ Sex | Class, facet = "by", data = ttnc,
    type = type_spineplot(weights = ttnc$Freq)
  )
}
expect_snapshot_plot(f, label = "spineplot_facet_by")

f = function() {
  tinyplot(
    Survived ~ Sex | Class, facet = "by", data = ttnc,
    type = type_spineplot(weights = ttnc$Freq),
    palette = "Dark 2",  axes = "t", legend = FALSE
  )
}
expect_snapshot_plot(f, label = "spineplot_facet_by_fancy")

#
## factor ~ numeric / factor ~ factor should auto-coerce to spineplot type

f = function() {
  plt(factor(cyl) ~ mpg, data = mtcars)
}
expect_snapshot_plot(f, label = "spineplot_auto_factor")

f = function() {
  plt(factor(cyl) ~ factor(am), data = mtcars)
}
expect_snapshot_plot(f, label = "spineplot_auto_factors")

#
## xlab/ylab = NA should suppress the axis title without clipping the
## self-drawn category/tick labels under dynamic themes (#635)

f = function() {
  tinyplot(Species ~ Sepal.Length, data = iris,
    theme = "dynamic", type = "spineplot", ylab = NA)
}
expect_snapshot_plot(f, label = "spineplot_ylab_na_issue635")

f = function() {
  tinyplot(Species ~ Sepal.Length, data = iris,
    theme = "dynamic", type = "spineplot", xlab = NA)
}
expect_snapshot_plot(f, label = "spineplot_xlab_na_issue635")

#
## outer legend exercises the secondary (RHS) axis margin bump (type_axes_hints
## rhs_axis); guards the spineplot legend-margin path during the #635 refactor

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris,
    type = "spineplot", legend = "right!")
}
expect_snapshot_plot(f, label = "spineplot_legend_outer_right")
