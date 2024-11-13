source("helpers.R")
using("tinysnapshot")

f = function() {
  tinyplot(Species ~ Sepal.Width, data = iris, type = "ridge", col = "white")
}
expect_snapshot_plot(f, label = "ridge_01")

f = function() {
  tinyplot(Month ~ Ozone,
    data = airquality,
    type = type_ridge(offset = .5),
    bg = "light blue", col = "black")
}
expect_snapshot_plot(f, label = "ridge_02")
