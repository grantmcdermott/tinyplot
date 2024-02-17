source("helpers.R")
using("tinysnapshot")
if (ON_CRAN) exit_file("CRAN")

## Skip failing test in R devel due to some minor esoteric difference coming up 
## in R 4.4.0. Can revert once it reaches release for local testing.
exit_if_not(getRversion()  <= "4.3.2")


op = par(no.readonly = TRUE)

f1 = function() {
  tinyplot(
    Sepal.Width ~ Sepal.Length | Species, iris,
    grid = grid(),
    legend = legend("bottom!", bty = "o")
  )
  points(6,3, pch = 17, col = "hotpink", cex = 1.5)
}
expect_snapshot_plot(f1, label = "par_restore_bottom")

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
expect_snapshot_plot(f2, label = "par_restore_FALSE")

# restore original par settings and then rerun with par_restore=TRUE
par(op)

f3 = function() {
  tinyplot(
    mpg ~ wt | cyl, mtcars,
    pch = 19,
    grid = grid(),
    legend = legend("right!", title = "How many cylnders do you have?"),
    par_restore = TRUE
  )
  lines(lowess(mtcars[["wt"]], mtcars[["mpg"]]))
  plot(1:10)
}
expect_snapshot_plot(f3, label = "par_restore_TRUE")
