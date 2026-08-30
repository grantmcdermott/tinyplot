source("helpers.R")
using("tinysnapshot")


#
## xord -----

f = function() tinyplot(mpg ~ factor(cyl), data = mtcars, type = type_points(xord = "desc"))
expect_snapshot_plot(f, label = "points_xord_desc")

f = function() tinyplot(mpg ~ factor(cyl), data = mtcars, type = type_points(xord = "rev"))
expect_snapshot_plot(f, label = "points_xord_rev")

# The ranking statistic follows what the type draws. A distribution type ranks
# on the mean, so a category of many small values must not outrank one of few
# large ones -- which is exactly what summing would do here.
d = data.frame(g = factor(rep(c("A", "B"), c(100, 5))), y = c(rep(1, 100), rep(10, 5)))
expect_equal(levels(tinyplot:::sanitize_ord(d$g, d$y, NULL, "desc", stat = "mean")), c("B", "A"))
expect_equal(levels(tinyplot:::sanitize_ord(d$g, d$y, NULL, "desc", stat = "sum")), c("A", "B"))


#
## numeric x cannot be reordered -----

# a numeric x is plotted at its own values, so the request is dropped -- but
# silently dropping it is the failure mode worth surfacing
expect_warning(
  tinyplot(mpg ~ cyl, data = mtcars, type = type_points(xord = "desc")),
  pattern = "only categorical"
)
expect_warning(
  tinyplot(mpg ~ cyl, data = mtcars, type = type_points(xlevels = c("4", "6"))),
  pattern = "only categorical"
)
# ...but a factor x is fine, and so is not asking in the first place
expect_silent(tinyplot(mpg ~ factor(cyl), data = mtcars, type = type_points(xord = "desc")))
expect_silent(tinyplot(mpg ~ cyl, data = mtcars, type = type_points()))
