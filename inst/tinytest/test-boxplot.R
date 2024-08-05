source("helpers.R")
using("tinysnapshot")

#
## simple boxplots

f = function() {
  plt(count ~ spray, data = InsectSprays, type = "boxplot")
}
expect_snapshot_plot(f, label = "boxplot_simple")

f = function() {
  plt(count ~ spray, data = InsectSprays, type = "boxplot", horizontal = TRUE)
}
expect_snapshot_plot(f, label = "boxplot_simple_horizontal")

f = function() {
  plt(
    decrease ~ treatment, data = OrchardSprays, fill = "bisque",
    log = "y", type = "boxplot", horizontal = TRUE
  )
}
expect_snapshot_plot(f, label = "boxplot_log_horizontal")

#
## grouped boxplots

f = function() {
  plt(len ~ dose | supp,data = ToothGrowth, type = "boxplot")
}
expect_snapshot_plot(f, label = "boxplot_groups")

# fancier version with arg passing
f = function() {
  plt(len ~ dose | factor(supp, labels = c("Ascorbic acid", "Orange juice")), 
      data = ToothGrowth,
      boxwex = 0.5,
      staplewex = 0,
      lty  = 1, pch = 16,
      main = "Guinea Pigs' Tooth Growth",
      xlab = "Vitamin C dose mg", ylab = "tooth length",
      ylim = c(0, 35),
      type = "boxplot",
      legend = list(title = NULL),
      horizontal = TRUE
  )
}
expect_snapshot_plot(f, label = "boxplot_groups_argpass")

# don't dodge if by (groups) and x are the same
f = function() {
  plt(Sepal.Length ~ Species | Species, iris, type = "boxplot")
}
expect_snapshot_plot(f, label = "boxplot_groups_x_same")

# variation where legend is switched off
f = function() {
  plt(Sepal.Length ~ Species | Species, iris, type = "boxplot", legend = FALSE)
}
expect_snapshot_plot(f, label = "boxplot_groups_x_same_legendFALSE")

# achim's example of "equivalent" x and by
# https://github.com/grantmcdermott/tinyplot/issues/195#issuecomment-2268074228
f = function() {
  d = data.frame(
    outcome = sin(1:40 - 3),
    player1 = factor(rep(c("female", "male"), each = 20)),
    player2 = factor(rep(rep(c("female", "male"), each = 10), 2))
  )
  tinyplot(outcome ~ player1 | player2, data = d)
}
expect_snapshot_plot(f, label = "boxplot_groups_x_equivalent")

# variation where legend if switched off
f = function() {
  d = data.frame(
    outcome = sin(1:40 - 3),
    player1 = factor(rep(c("female", "male"), each = 20)),
    player2 = factor(rep(rep(c("female", "male"), each = 10), 2))
  )
  tinyplot(outcome ~ player1 | player2, data = d, legend = FALSE)
}
expect_snapshot_plot(f, label = "boxplot_groups_x_equivalent_legendFALSE")


#
## facets

f = function() {
  plt(weight ~ Time | Diet, ChickWeight, type = "boxplot", facet = "by")
}
expect_snapshot_plot(f, label = "boxplot_facet_by")

f = function() {
  plt(mpg ~ gear | factor(cyl), data = mtcars, type = "boxplot", facet = ~am)
}
expect_snapshot_plot(f, label = "boxplot_groups_facets_with_missings")

#
## numeric ~ factor should auto-coerce to boxplot type

f = function() {
  suppressWarnings(plt(mpg ~ factor(am), data = mtcars))
}
expect_snapshot_plot(f, label = "boxplot_auto_factor")
