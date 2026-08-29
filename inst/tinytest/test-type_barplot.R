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
  tinyplot(~ cyl | vs, data = mtcars, type = "barplot", beside = TRUE)
}
expect_snapshot_plot(f, label = "barplot_group_beside")

f = function() {
  tinyplot(~ cyl | vs, data = mtcars, type = "barplot", facet = "by")
}
expect_snapshot_plot(f, label = "barplot_facet")

f = function() {
  tinyplot(~ cyl | vs, data = mtcars, type = "barplot",
           facet = "by", facet.args = list(free = TRUE))
}
expect_snapshot_plot(f, label = "barplot_facet_free")

f = function() {
  tinyplot(extra ~ ID | group, facet = "by", data = sleep,
    type = "barplot", beside = TRUE)
}
expect_snapshot_plot(f, label = "barplot_aggregation")

f = function() {
  tinyplot(Freq ~ Sex | Survived, facet = ~ Class, data = as.data.frame(Titanic),
           type = "barplot", flip = TRUE, beside = TRUE)
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
    center = TRUE, flip = TRUE, lighten = FALSE, offset = "Red",
    yaxl = "percent")
}
expect_snapshot_plot(f, label = "barplot_offset_aside")

# Named numeric offset places a set-aside category at an explicit baseline
f = function() {
  tinyplot(Freq ~ Eye | Hair, facet = Sex ~ 1, data = hec, type = "barplot",
    center = TRUE, flip = TRUE, lighten = FALSE, offset = c(Red = 1.1),
    yaxl = "percent")
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

# Lighter opaque grouped fills (#646). The grouped bar fill should default to a
# lighter, opaque tint of the palette colour, and `lighten = FALSE` should fall
# back to the fully-saturated palette colour.
f = function() {
  tinyplot(~ cyl | vs, data = mtcars,
    type = type_barplot(lighten = FALSE), theme = "clean2")
}
expect_snapshot_plot(f, label = "barplot_group_lighten_false")


#
## xord -----

# sort bars by height -- not previously possible without relevelling by hand
f = function() tinyplot(~ cyl, data = mtcars, type = type_barplot(xord = "desc"))
expect_snapshot_plot(f, label = "barplot_xord_desc")

f = function() tinyplot(~ cyl, data = mtcars, type = type_barplot(xord = "rev"))
expect_snapshot_plot(f, label = "barplot_xord_rev")

# `xord` must rank the *aggregated* bars, not the raw cells. With unequal cell
# counts these two order the bars oppositely (means: few>mid>many; sums:
# many>mid>few), so the pair pins the ranking to whatever FUN actually drew.
bars = data.frame(
  g = factor(rep(c("few", "many", "mid"), times = c(1, 6, 3))),
  v = c(10, rep(3, 6), rep(5, 3))
)

f = function() tinyplot(v ~ g, data = bars, type = type_barplot(xord = "desc"))
expect_snapshot_plot(f, label = "barplot_xord_aggregated_mean")

f = function() tinyplot(v ~ g, data = bars, type = type_barplot(xord = "desc", FUN = sum))
expect_snapshot_plot(f, label = "barplot_xord_aggregated_sum")


# `xord` no longer accepts explicit levels; that is what `xlevels` is for
expect_error(
  tinyplot(~ cyl, data = mtcars, type = type_barplot(xord = c("8", "6", "4"))),
  pattern = "must be NULL"
)
# and `xlevels` no longer accepts the ord keywords
# TODO: sanitize_xlevels() warns about the partial match and *then* aborts on
# the complete miss. expect_error() does not muffle the warning, so it escapes
# to R's deferred list and surfaces at the end of a suite run. Restore this and
# the `xlevels = "rev"` case below once the no-match stop() is ordered ahead of
# the partial-match warning.
# expect_warning(
#   tinyplot(~ cyl, data = mtcars, type = type_barplot(xlevels = "asis")),
#   pattern = "correspond to levels"
# )

# "start"/"end" name a position along a secondary axis, which x-categories do
# not have; offering them here would silently alias "desc" (ungrouped) or
# silently re-read as "first/last `by` level" (grouped)
expect_error(
  tinyplot(~ cyl, data = mtcars, type = type_barplot(xord = "end")),
  pattern = "not available for this plot type"
)

# a bar is a single aggregate, so it has no variance to rank on: "minvar" is
# not part of this type's vocabulary at all
expect_error(
  tinyplot(~ cyl | vs, data = mtcars, type = type_barplot(xord = "minvar")),
  pattern = "must be NULL"
)

# a ranking function may not ask for `x` here: bar categories are a flat set,
# so the only thing to hand over would be the `by` level index -- a nominal
# code that lm() would happily regress on and return a meaningless number
expect_error(
  tinyplot(~ cyl | vs, data = mtcars,
           type = type_barplot(xord = function(y, x) coef(lm(y ~ x))[2])),
  pattern = "no secondary axis"
)
# ...but a plain function is fine. "asc" is now the direct route to ascending
# order, and must agree with the function that used to be the only way there
f = function() tinyplot(~ cyl, data = mtcars, type = type_barplot(xord = function(y) sum(y)))
expect_snapshot_plot(f, label = "barplot_xord_ascending")

f = function() tinyplot(~ cyl, data = mtcars, type = type_barplot(xord = "asc"))
expect_snapshot_plot(f, label = "barplot_xord_ascending")

# naming a strict subset of levels silently dropped the rest to NA, taking
# those observations out of the plot without a word (#645 follow-up)
expect_warning(
  tinyplot(~ cyl, data = mtcars, type = type_barplot(xlevels = c("8", "4"))),
  pattern = "omits 1 of the 3 levels"
)
# and a complete miss is fatal, rather than surfacing later as an unrelated
# error about zero-length ranges. (Commented out; see the TODO above.)
# expect_error(
#   tinyplot(~ cyl, data = mtcars, type = type_barplot(xlevels = "rev")),
#   pattern = "matches none of the levels"
# )

asis_dat = data.frame(
  g = factor(c("z", "z", "a", "m", "m", "m")),   # appearance z,a,m; levels a,m,z
  v = c(5, 5, 1, 3, 3, 3)
)
# what "asis" computes: appearance order, not level order
expect_equal(
  levels(tinyplot:::sanitize_ord(asis_dat$g, NULL, NULL, "asis", keywords = tinyplot:::ord_keywords_scalar)),
  c("z", "a", "m")
)
# ...and *where* barplot applies it, which the unit call above cannot check:
# "asis" has to run before aggregate(), which sorts on the grouping columns and
# would otherwise leave it returning plain level order. The ranking keywords
# have the opposite requirement, so the two are applied at different points.
f = function() tinyplot(v ~ g, data = asis_dat, type = type_barplot(xord = "asis"))
expect_snapshot_plot(f, label = "barplot_xord_asis")


# xaxl dictionary relabelling (replaces deprecated, type-specific xaxalabels)
f = function() {
  tinyplot(~ cyl, data = mtcars, type = "barplot",
           xaxl = c("4" = "four", "6" = "six", "8" = "eight"))
}
expect_snapshot_plot(f, label = "barplot_xaxl_dict")

