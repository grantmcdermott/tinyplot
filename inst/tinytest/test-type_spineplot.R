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

# equivalent via the top-level `weights` argument, which supports NSE in the
# formula method (#332). Should reproduce "spineplot_weights" above.
f = function() {
  tinyplot(Survived ~ Sex, data = ttnc, type = "spineplot", weights = Freq)
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
    palette = "Dark 2",  axes = "t", legend = FALSE, lwd = 0
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
## spineplots default to saturated fills (lighten = FALSE); `lighten = TRUE`
## opts in to the lighter-but-opaque tint used by the other area types (#646).
## (The default saturated look is already covered by the `spineplot_yby` case
## above.)

f = function() {
  tinyplot(
    Species ~ Sepal.Width | Species, data = iris,
    type = type_spineplot(breaks = 4, lighten = TRUE),
    palette = "Pastel 1"
  )
}
expect_snapshot_plot(f, label = "spineplot_yby_lighten_true")


#
## xord / yord -----

# both spineplot axes are categorical, so the size keywords rank on frequency
f = function() {
  tinyplot(Species ~ cut(Sepal.Length, 3), data = iris, type = "spineplot", xord = "desc")
}
expect_snapshot_plot(f, label = "spineplot_xord_desc")

f = function() {
  tinyplot(Species ~ cut(Sepal.Length, 3), data = iris, type = "spineplot", yord = "rev")
}
expect_snapshot_plot(f, label = "spineplot_yord_rev")

# a spine is a proportion of a count, with no dispersion of its own, so
# "minvar" is not part of this type's vocabulary
expect_error(
  tinyplot(Species ~ cut(Sepal.Length, 3), data = iris, type = "spineplot", xord = "minvar"),
  pattern = "must be NULL"
)


#
## xaxl / yaxl -----

# this type draws its own axes, so it never reaches the standard path where
# `xaxl`/`yaxl` are applied; they are applied inside data_spineplot() instead
f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "spineplot", yaxl = toupper)
}
expect_snapshot_plot(f, label = "spineplot_yaxl_toupper")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "spineplot",
           yaxl = c(setosa = "SET", virginica = "VIR"))
}
expect_snapshot_plot(f, label = "spineplot_yaxl_dict")

# categorical x, and a numeric x whose breaks take a formatting keyword
spine_d = data.frame(
  grp  = factor(rep(c("alpha", "beta"), each = 50)),
  resp = factor(rep(c("lo", "hi"), 50), levels = c("lo", "hi"))
)
f = function() tinyplot(resp ~ grp, data = spine_d, type = "spineplot", xaxl = toupper)
expect_snapshot_plot(f, label = "spineplot_xaxl_toupper")

