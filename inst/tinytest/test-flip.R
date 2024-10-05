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
    main = "Flipped histogram"
  )
}
expect_snapshot_plot(f, label = "flip_density")

# flipped boxplot (equivalent to horizontal = TRUE)
f = function() {
  plt(count ~ spray, data = InsectSprays, type = "boxplot", flip = TRUE)
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
  tinyplot(x_dtt, y, grid = TRUE, flip = TRUE)
}
expect_snapshot_plot(f, label = "flip_date_time")
