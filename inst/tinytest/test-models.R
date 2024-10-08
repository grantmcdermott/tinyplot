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
  plt(I(Temp > 80) ~ Wind | factor(Month), data = airquality, 
    type = type_loess())
}
expect_snapshot_plot(f, label = "model_loess_by")


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
  plt(I(Temp > 80) ~ Wind, facet = ~Month, data = airquality, 
    type = type_loess())
}
expect_snapshot_plot(f, label = "model_loess_facet")
