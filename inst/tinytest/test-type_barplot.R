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