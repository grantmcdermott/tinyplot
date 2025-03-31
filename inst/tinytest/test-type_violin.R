source("helpers.R")
using("tinysnapshot")

#
## simple boxplots

f = function() {
  plt(count ~ spray, data = InsectSprays, type = "violin")
}
expect_snapshot_plot(f, label = "violin_simple")

f = function() {
  plt(count ~ spray, data = InsectSprays, type = "violin", flip = TRUE)
}
expect_snapshot_plot(f, label = "boxplot_violin_flip")

f = function() {
  plt(
    decrease ~ treatment,
    data = OrchardSprays, fill = "bisque",
    log = "y", type = "violin", flip = TRUE, trim = TRUE
  )
}
expect_snapshot_plot(f, label = "violin_trim_log_flip")

#
## grouped boxplots

f = function() {
  plt(len ~ dose | supp, data = ToothGrowth, type = "violin")
}
expect_snapshot_plot(f, label = "violin_groups")

# fancier version with arg passing
f = function() {
  plt(len ~ dose | factor(supp, labels = c("Ascorbic acid", "Orange juice")),
    data = ToothGrowth,
    fill = 0.2, 
    main = "Guinea Pigs' Tooth Growth",
    xlab = "Vitamin C dose mg", ylab = "tooth length",
    type = type_violin(trim = TRUE, joint.bw = FALSE),
    legend = list(title = NULL),
    flip = TRUE
  )
}
expect_snapshot_plot(f, label = "violin_groups_argpass")

# don't dodge if by (groups) and x are the same
f = function() {
  plt(Sepal.Length ~ Species | Species, iris, type = "violin", legend = FALSE)
}
expect_snapshot_plot(f, label = "violin_x_by")

# achim's example of "equivalent" x and by
# https://github.com/grantmcdermott/tinyplot/issues/195#issuecomment-2268074228
f = function() {
  d = data.frame(
    outcome = sin(1:40 - 3),
    player1 = factor(rep(c("female", "male"), each = 20)),
    player2 = factor(rep(rep(c("female", "male"), each = 10), 2))
  )
  tinyplot(outcome ~ player1 | player2, data = d, type = "violin", legend = FALSE)
}
expect_snapshot_plot(f, label = "violin_by_x_equivalent")

#
## facets

f = function() {
  plt(weight ~ Time | Diet, ChickWeight, type = "violin", facet = "by", fill = 0.2)
}
expect_snapshot_plot(f, label = "violin_facet_by")

f = function() {
  plt(mpg ~ gear | gear, facet = "by", data = mtcars, type = "violin")
}
expect_snapshot_plot(f, label = "violin_facet_x_by_same")

# f = function() {
#   plt(mpg ~ gear | factor(cyl), data = mtcars, type = "violin", facet = ~am)
# }
# expect_snapshot_plot(f, label = "violin_groups_facets_with_missings")
