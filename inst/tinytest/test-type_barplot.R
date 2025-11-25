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