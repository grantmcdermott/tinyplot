source("helpers.R")
using("tinysnapshot")

# selection of flipped plot types

f = function () {
  tinyplot(
    Sepal.Length ~ Petal.Length | Species, data = iris,
    grid = TRUE,
    flip = TRUE,
    main = "Flipped scatterplot"
  )
}
expect_snapshot_plot(f, label = "flip_p")

f = function () {
  tinyplot(
    Sepal.Length ~ Petal.Length | Species, data = iris,
    grid = TRUE,
    flip = TRUE,
    log = "x",
    main = "Flipped scatterplot (log x)"
  )
}
expect_snapshot_plot(f, label = "flip_p_logx")

f = function () {
  tinyplot(
    Sepal.Length ~ Petal.Length | Species, data = iris,
    facet = "by",
    grid = TRUE,
    flip = TRUE,
    main = "Flipped + facetted scatterplot"
  )
}
expect_snapshot_plot(f, label = "flip_facet_by")

f = function () {
  tinyplot(
    ~ Petal.Length | Species, data = iris,
    type = "histogram",
    grid = TRUE,
    flip = TRUE,
    main = "Flipped histogram"
  )
}
expect_snapshot_plot(f, label = "flip_hist")

f = function () {
  tinyplot(
    ~ Petal.Length | Species, data = iris,
    type = "density", fill = 0.5,
    grid = TRUE,
    flip = TRUE,
    main = "Flipped density"
  )
}
expect_snapshot_plot(f, label = "flip_density")

# flipped boxplot (equivalent to horizontal = TRUE)
f = function() {
  plt(count ~ spray, data = InsectSprays, type = "boxplot", flip = TRUE, grid = TRUE)
}
expect_snapshot_plot(f, label = "flip_boxplot")

f = function() {
  m = transform(mtcars, gear = factor(gear))
  mod = lm(mpg ~ wt*gear, m)
  coefs = data.frame(names(coef(mod)), coef(mod), confint(mod))
  coefs = setNames(coefs, c("term", "estimate", "lwr", "upr"))
  op = tpar(las = 1, cex.axis = 0.75)
  with(
    coefs,
    tinyplot(
      term, estimate, ymin = lwr, ymax = upr,
      type = "pointrange",
      pch = 19,
      flip = TRUE,
      grid = TRUE, axes = "l", xlab = NA,
      main = "Flipped coefplot"
    )
  )
  abline(v = 0, lty  = 4, col = "hotpink")
  tpar(op)
}
expect_snapshot_plot(f, label = "flip_pointrange")

f = function() {
  m = transform(mtcars, cyl = factor(cyl))
  pred = predict(lm(mpg ~ wt + cyl, m), interval = "confidence")
  m = cbind(m, pred)
  with(
    m,
    tinyplot(
      wt, fit, ymin = lwr, ymax = upr, by = cyl,
      type = "ribbon",
      flip = TRUE,
      grid = TRUE,
      main = "Flipped prediction plot"
    )
  )
  with(
    m,
    tinyplot(wt, mpg, by = cyl, pch = 16, flip = TRUE, add = TRUE)
  )
}
expect_snapshot_plot(f, label = "flip_ribbon_pred")


f = function() {
  x_dt = as.Date(strptime(20010101:20010110, format="%Y%m%d", tz = "UTC"))
  y = 1:10
  tinyplot(x_dt, y, grid = TRUE, flip = TRUE)
}
expect_snapshot_plot(f, label = "flip_date")


# flipped single-letter line types (#675)

f = function() {
  tinyplot(1:10, (1:10)^2, type = "h", flip = TRUE, main = "Flipped type = \"h\"")
}
expect_snapshot_plot(f, label = "flip_type_h")

f = function() {
  tinyplot(1:10, (1:10)^2, type = "s", flip = TRUE, main = "Flipped type = \"s\"")
}
expect_snapshot_plot(f, label = "flip_type_step")

f = function() {
  tinyplot(1:10, (1:10)^2, type = "S", flip = TRUE, main = "Flipped type = \"S\"")
}
expect_snapshot_plot(f, label = "flip_type_step_rev")

f = function() {
  tinyplot(
    circumference ~ age | Tree, data = Orange,
    type = "h", flip = TRUE, grid = TRUE,
    main = "Flipped grouped type = \"h\""
  )
}
expect_snapshot_plot(f, label = "flip_type_h_grouped")


# flipped single-letter line types keep their geometry (#675)
# These checks read the SVG output directly, so they run on every OS.

svg_of = function(...) {
  tf = tempfile(fileext = ".svg")
  svglite::svglite(tf)
  tinyplot(...)
  dev.off()
  readLines(tf)
}

num_attr = function(el, attr) {
  as.numeric(sub(sprintf(".*%s='([^']*)'.*", attr), "\\1", el))
}

first_step = function(svg) {
  pl = grep("<polyline ", svg, value = TRUE)[1]
  pts = strsplit(sub(".*points='([^']*)'.*", "\\1", pl), " ")[[1]]
  xy = do.call(rbind, lapply(strsplit(pts, ","), as.numeric))
  xy[2, ] - xy[1, ]
}

x = 1:5
y = c(10, 20, 30, 40, 50)

# type = "h" with flip draws one horizontal segment per point
svg = svg_of(x, y, type = "h", flip = TRUE, axes = FALSE)
seg = grep("<line ", svg, value = TRUE)
expect_equal(length(seg), length(x))
expect_true(all(abs(num_attr(seg, "y1") - num_attr(seg, "y2")) < 1e-6))
expect_true(all(abs(num_attr(seg, "x2") - num_attr(seg, "x1")) > 1))

# unflipped "h" still draws vertical drops
svg = svg_of(x, y, type = "h", axes = FALSE)
seg = grep("<line ", svg, value = TRUE)
expect_equal(length(seg), length(x))
expect_true(all(abs(num_attr(seg, "x1") - num_attr(seg, "x2")) < 1e-6))

# flipping swaps which coordinate moves first in the step types
d = first_step(svg_of(x, y, type = "s", flip = TRUE, axes = FALSE))
expect_true(abs(d[1]) < 1e-6 && abs(d[2]) > 1)

d = first_step(svg_of(x, y, type = "S", flip = TRUE, axes = FALSE))
expect_true(abs(d[1]) > 1 && abs(d[2]) < 1e-6)

d = first_step(svg_of(x, y, type = "s", axes = FALSE))
expect_true(abs(d[1]) > 1 && abs(d[2]) < 1e-6)
