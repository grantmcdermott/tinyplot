source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(~ cyl, data = mtcars, type = "barplot")
}
expect_snapshot_plot(f, label = "barplot_simple")

f = function() {
  tinyplot(~ cyl | vs, data = mtcars, type = "barplot")
}
expect_snapshot_plot(f, label = "barplot_group")


f = function() {
  tinyplot(~ cyl | vs, data = mtcars, type = "barplot", beside = TRUE, fill = 0.2)
}
expect_snapshot_plot(f, label = "barplot_group_beside")

f = function() {
  tinyplot(~ cyl | vs, data = mtcars, type = "barplot", fill = 0.2,
           facet = "by")
}
expect_snapshot_plot(f, label = "barplot_facet")

f = function() {
  tinyplot(~ cyl | vs, data = mtcars, type = "barplot", fill = 0.2,
           facet = "by", facet.args = list(free = TRUE))
}
expect_snapshot_plot(f, label = "barplot_facet_free")

f = function() {
  tinyplot(extra ~ ID | group, facet = "by", data = sleep,
    type = "barplot", beside = TRUE, fill = 0.6)
}
expect_snapshot_plot(f, label = "barplot_aggregation")

f = function() {
  tinyplot(Freq ~ Sex | Survived, facet = ~ Class, data = as.data.frame(Titanic),
           type = "barplot", flip = TRUE, fill = 0.6, beside = TRUE)
}
expect_snapshot_plot(f, label = "barplot_flip_fancy")

f = function() {
  tinyplot(~ cyl, data = mtcars, type = "barplot", xlevels = 3:1)
}
expect_snapshot_plot(f, label = "barplot_xlevels_issue430")

f = function() {
  tab = as.data.frame(xtabs(~ cyl, data = mtcars))
  tinyplot(Freq ~ cyl, data = tab, type = "barplot")
  tinyplot_add(type = "text", labels = tab$Freq, pos = 3, xpd = TRUE)
}
expect_snapshot_plot(f, label = "barplot_text_issue469")

#
## Custom axis titles for one-sided barplots (issue #423)

f = function() {
  set.seed(2025)
  n = 100L
  grp = factor(sample(0:1, size = n, replace = TRUE))
  x = rpois(n, 5)
  plt(~ x | grp, type = "barplot", beside = TRUE, xlab = "Custom x title")   
}
expect_snapshot_plot(f, label = "barplot_custom_xtitle")

# issue #423
f = function() {
  set.seed(2025)
  n = 100L
  grp = factor(sample(0:1, size = n, replace = TRUE))
  x = rpois(n, 5)
  plt(~ x | grp, type = "barplot", beside = TRUE, ylab = "Custom y title")   
}
expect_snapshot_plot(f, label = "barplot_custom_ytitle")


# univariate formula: factor(y) ~ 1 infers barplot
f = function() {
  tinyplot(Species ~ 1, data = iris)
}
expect_snapshot_plot(f, label = "barplot_formula_y1")

# univariate formula: ~ factor(x) infers barplot
f = function() {
  tinyplot(~ Species, data = iris)
}
expect_snapshot_plot(f, label = "barplot_formula_univariate")


#
## offset argument

# Scalar offset shifts all bars
f = function() {
  tinyplot(extra ~ ID, data = sleep[sleep$group == 1, ],
    type = type_barplot(offset = 10))
}
expect_snapshot_plot(f, label = "barplot_offset_scalar")

# Vector offset (waterfall pattern)
f = function() {
  d = data.frame(x = factor(LETTERS[1:4]), y = c(10, 5, -3, 8))
  d$off = c(0, cumsum(d$y[-4]))
  tinyplot(y ~ x, data = d, type = type_barplot(offset = d$off))
}
expect_snapshot_plot(f, label = "barplot_offset_waterfall")

# Offset + beside with grouping
f = function() {
  tinyplot(extra ~ ID | group, data = sleep,
    type = type_barplot(beside = TRUE, offset = rep(1, 10)))
}
expect_snapshot_plot(f, label = "barplot_offset_beside_group")

# Offset + stacked
f = function() {
  tinyplot(Freq ~ Sex | Survived, data = as.data.frame(Titanic)[1:8, ],
    type = type_barplot(offset = c(10, 20)))
}
expect_snapshot_plot(f, label = "barplot_offset_stacked")

# Offset + flip
f = function() {
  d = data.frame(x = factor(LETTERS[1:3]), y = c(5, 3, 7))
  tinyplot(y ~ x, data = d, type = type_barplot(offset = c(2, 4, 1)), flip = TRUE)
}
expect_snapshot_plot(f, label = "barplot_offset_flip")

# Offset + center warns and ignores center
expect_warning(
  tinyplot(~ cyl | vs, data = mtcars,
    type = type_barplot(offset = c(5, 10, 15), center = TRUE)),
  "cannot be combined"
)

# Wrong offset length errors
expect_error(
  tinyplot(~ cyl, data = mtcars, type = type_barplot(offset = c(1, 2))),
  "must be length"
)


#
## by-level offset ("set aside" / diverging-Likert)

hec = as.data.frame(proportions(HairEyeColor, 2:3))

# Character offset auto-places a set-aside category (the #420 use case)
f = function() {
  tinyplot(Freq ~ Eye | Hair, facet = Sex ~ 1, data = hec, type = "barplot",
    center = TRUE, flip = TRUE, yaxl = "percent", offset = "Red")
}
expect_snapshot_plot(f, label = "barplot_offset_aside")

# Named numeric offset places a set-aside category at an explicit baseline
f = function() {
  tinyplot(Freq ~ Eye | Hair, facet = Sex ~ 1, data = hec, type = "barplot",
    center = TRUE, flip = TRUE, yaxl = "percent", offset = c(Red = 1.1))
}
expect_snapshot_plot(f, label = "barplot_offset_aside_explicit")

# Invalid by-level offsets error
expect_error( # unknown level
  tinyplot(~ cyl | vs, data = mtcars, type = type_barplot(offset = "nope")),
  "must name levels"
)
expect_error( # no `by` grouping
  tinyplot(~ cyl, data = mtcars, type = type_barplot(offset = "4")),
  "requires a 'by' grouping"
)
expect_error( # requires stacked bars
  tinyplot(~ cyl | vs, data = mtcars,
    type = type_barplot(offset = "0", beside = TRUE)),
  "requires stacked bars"
)

# xlab = NA should suppress the axis title, not error (#635)

f = function() {
  tinyplot(~ Species, data = iris, type = "barplot", xlab = NA)
}
expect_snapshot_plot(f, label = "barplot_xlab_na_issue635")

# sort_y: order bars by aggregated y value (#645)

# ascending (sort() default: smallest bar first)
f = function() {
  tinyplot(extra ~ ID, data = sleep, type = type_barplot(sort_y = TRUE))
}
expect_snapshot_plot(f, label = "barplot_sort_y_increasing")

# decreasing (largest bar first)
f = function() {
  tinyplot(extra ~ ID, data = sleep, type = type_barplot(sort_y = "decreasing"))
}
expect_snapshot_plot(f, label = "barplot_sort_y_decreasing")

# grouped/stacked: sort on the per-x column total
f = function() {
  tinyplot(
    Freq ~ Class | Sex, data = as.data.frame(Titanic),
    type = type_barplot(sort_y = TRUE), fill = 0.6
  )
}
expect_snapshot_plot(f, label = "barplot_sort_y_grouped")

# xlevels takes precedence over sort_y (with a warning)
expect_warning(
  tinyplot(
    ~ cyl, data = mtcars,
    type = type_barplot(sort_y = TRUE, xlevels = c("8", "6", "4"))
  ),
  "takes precedence"
)
