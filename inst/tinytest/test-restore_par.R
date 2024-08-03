source("helpers.R")
using("tinysnapshot")

# ## Avoid test fails on older R versions (pre 4.4.0) due to slight change in
# ## density grid value calculations.
# ## https://bugs.r-project.org/show_bug.cgi?id=18337
if (getRversion() < "4.4.0") exit_file("R < 4.4.0")


op = par(no.readonly = TRUE)

f1 = function() {
  tinyplot(
    Sepal.Width ~ Sepal.Length | Species, iris,
    grid = grid(),
    legend = legend("bottom!", bty = "o")
  )
  points(6,3, pch = 17, col = "hotpink", cex = 1.5)
}
expect_snapshot_plot(f1, label = "restore_par_bottom")

f2 = function() {
  tinyplot(
    mpg ~ wt | cyl, mtcars,
    pch = 19,
    grid = grid(),
    legend = legend("right!", title = "How many cylnders do you have?")
  )
  lines(lowess(mtcars[["wt"]], mtcars[["mpg"]]))
  plot(1:10)
}
expect_snapshot_plot(f2, label = "restore_par_FALSE")

# restore original par settings and then rerun with restore.par=TRUE
par(op)

f3 = function() {
  tinyplot(
    mpg ~ wt | cyl, mtcars,
    pch = 19,
    grid = grid(),
    legend = legend("right!", title = "How many cylnders do you have?"),
    restore.par = TRUE
  )
  lines(lowess(mtcars[["wt"]], mtcars[["mpg"]]))
  plot(1:10)
}
expect_snapshot_plot(f3, label = "restore_par_TRUE")
