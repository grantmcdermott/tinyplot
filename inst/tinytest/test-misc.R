source("helpers.R")
using("tinysnapshot")

# empty plot(s)
f = function () {
  tinyplot(Sepal.Length ~ Petal.Length, data = iris, type = "n")
}
expect_snapshot_plot(f, label = "type_n")
f = function () {
  tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, type = "n")
}
expect_snapshot_plot(f, label = "type_n_by")
f = function () {
  tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, type = "l", empty = TRUE)
}
expect_snapshot_plot(f, label = "type_l_empty")


# jittered points
f = function() {
  set.seed(42)
  tinyplot(Temp ~ Month | ordered(Month), airquality, type = "j", pch = 16)
}
expect_snapshot_plot(f, label = "type_j")

f = function() {
  set.seed(42)
  tinyplot(Species ~ Sepal.Length, data = iris, type = "j")
}
expect_snapshot_plot(f, label = "type_j_y")

# log axes
f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "x")
  tinyplot(Temp ~ Day, data = airquality, log = "x")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_x")

f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "y")
  tinyplot(Temp ~ Day, data = airquality, log = "y")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_y")

f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "xy")
  tinyplot(Temp ~ Day, data = airquality, log = "xy")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_xy")

f = function() {
  op = tpar(mfrow = c(1, 2))
  plot(Temp ~ Day, data = airquality, log = "yx")
  tinyplot(Temp ~ Day, data = airquality, log = "yx")
  tpar(op)
} 
expect_snapshot_plot(f, label = "arg_log_yx")

f = function() {
  op = tpar(mfrow = c(1, 1))
  m = transform(mtcars, cyl = factor(cyl))
  pred = predict(lm(mpg ~ wt + cyl, m), interval = "confidence")
  m = cbind(m, pred)
  with(
    m,
    tinyplot(wt, fit, ymin = lwr, ymax = upr, by = cyl, type = "ribbon", grid = TRUE)
  )
  with(
    m,
    tinyplot(wt, mpg, by = cyl, pch = 16, add = TRUE)
  )
  tpar(op)
}
expect_snapshot_plot(f, label = "addTRUE")

# formatting axis tick labels
f = function() plt(
  I(decrease/100) ~ treatment, data = OrchardSprays,
  xaxl = tolower, yaxl = "percent"
)
expect_snapshot_plot(f, label = "xaxl_yaxl")

# formatting axis breaks and tick labels at the same time
f = function() plt(
  I(decrease/100) ~ treatment, data = OrchardSprays,
  xaxb = c("A", "C", "E", "G"), xaxl = tolower, 
  yaxb = c(0, 0.2, 0.5, 1, 1.4), yaxl = "percent",
  grid = TRUE
)
expect_snapshot_plot(f, label = "xaxb_yaxb_xaxl_yaxl")


# saving files (here: png)
if (requireNamespace("png", quietly = TRUE)) {
  f = function() {
    tmp_path = tempfile(fileext = ".png")
    suppressWarnings(tinyplot(
      Sepal.Length ~ Petal.Length, data = iris,
      file = tmp_path, width = 4, height = 4
    ))
    obj = png::readPNG(tmp_path, info = TRUE)
    unlink(tmp_path)
    # dims = attr(obj, "dim")
    dims = attr(obj, "info")[["dim"]]
    return(dims)
  }
  # expect_equal(f(), c(1200, 1200, 4), label = "png_size")
  expect_equal(f(), c(1200, 1200), label = "png_size")
}

