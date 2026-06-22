source("helpers.R")
using("tinysnapshot")

# confidence intervals
for (l in c(0.5, 0.8, 0.95)) {
  f = function() {
    plt(I(Temp > 80) ~ Wind, data = airquality, 
      type = type_glm(family = binomial, level = l))
  }
  lab = paste0("model_glm_", l * 100)
  expect_snapshot_plot(f, label = lab)
}

for (l in c(0.5, 0.8, 0.95)) {
  f = function() {
    plt(I(Temp > 80) ~ Wind, data = airquality, 
      type = type_lm(level = l))
  }
  lab = paste0("model_lm_", l * 100)
  expect_snapshot_plot(f, label = lab)
}

# by
f = function() {
  plt(I(Temp > 80) ~ Wind | factor(Month), data = airquality, 
    type = type_glm(family = binomial))
}
expect_snapshot_plot(f, label = "model_glm_by")

f = function() {
  plt(I(Temp > 80) ~ Wind | factor(Month), data = airquality, 
    type = type_lm())
}
expect_snapshot_plot(f, label = "model_lm_by")

f = function() {
  plt(Temp ~ Day | factor(Month), data = airquality, type = type_loess())
}
expect_snapshot_plot(f, label = "model_loess_by")

f = function() {
  plt(Temp ~ Day | factor(Month), data = airquality, type = type_spline())
}
expect_snapshot_plot(f, label = "model_spline_by")

# facet
f = function() {
  plt(I(Temp > 80) ~ Wind, facet = ~Month, data = airquality, 
    type = type_glm(family = binomial))
}
expect_snapshot_plot(f, label = "model_glm_facet")

f = function() {
  plt(I(Temp > 80) ~ Wind, facet = ~Month, data = airquality, 
    type = type_lm())
}
expect_snapshot_plot(f, label = "model_lm_facet")

f = function() {
  plt(Temp ~ Day | Month, data = airquality, facet = "by", type = type_loess())
}
expect_snapshot_plot(f, label = "model_loess_facet")

f = function() {
  plt(Temp ~ Day | Month, data = airquality, facet = "by", type = type_spline())
}
expect_snapshot_plot(f, label = "model_spline_facet")


# weights (#332) -----------------------------------------------------------

s77 = as.data.frame(state.x77)

# weighted vs unweighted lm should produce visibly different fits. `weights`
# can be passed as a top-level argument (with NSE in the formula method) or
# directly to the type constructor.
f = function() {
  plt(`Life Exp` ~ Income, data = s77)
  plt_add(type = "lm")
  plt_add(type = "lm", weights = Population, col = "red")
}
expect_snapshot_plot(f, label = "model_lm_weights")

# logical checks for behaviours not covered by the snapshot above
# top-level NSE weights also work for glm and loess (lm is snapshotted above)
expect_silent(plt(`Life Exp` ~ Income, data = s77, type = "glm", weights = Population))
expect_silent(plt(Temp ~ Day, data = airquality, type = "loess", weights = Wind))
# constructor-level weights (evaluated vector) also work
expect_silent(plt(`Life Exp` ~ Income, data = s77, type = type_lm(weights = s77$Population)))
expect_silent(plt(am ~ mpg, data = mtcars, type = type_glm(weights = mtcars$wt)))
expect_silent(plt(dist ~ speed, data = cars, type = type_loess(weights = cars$speed)))
# weights supplied to an unsupported type warn (but are otherwise ignored)
expect_warning(
  plt(Temp ~ Wind, data = airquality, type = "p", weights = Day),
  pattern = "ignored by this plot type"
)