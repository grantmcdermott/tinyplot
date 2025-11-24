source("helpers.R")
using("tinysnapshot")

# Test data
models = list(
    "Model A" = lm(mpg ~ wt + cyl, data = mtcars),
    "Model B" = lm(mpg ~ wt + hp + cyl, data = mtcars),
    "Model C" = lm(mpg ~ wt, data = mtcars)
)
results = lapply(names(models), function(m) {
    data.frame(
        model = m,
        term = names(coef(models[[m]])),
        estimate = coef(models[[m]]),
        setNames(data.frame(confint(models[[m]])), c("conf.low", "conf.high"))
    )
})
results = do.call(rbind, results)


#
## Basic examples

# Pointrange dodge with numeric value
fun = function() {
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    data = results,
    type = "pointrange",
    dodge = 0.1,
    theme = "basic")
  # tinyplot_add(type = "pointrange")
}
expect_snapshot_plot(fun, label = "dodge_pointrange")

# As above, but flipped
fun = function() {
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    data = results,
    type = "pointrange",
    dodge = 0.1,
    flip = TRUE,
    theme = "basic")
}
expect_snapshot_plot(fun, label = "dodge_pointrange_flip")

# Pointrange dodge with logical TRUE (automatic calculation)
fun = function() {
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    data = results,
    type = "pointrange",
    dodge = TRUE,
    theme = "basic")
}
expect_snapshot_plot(fun, label = "dodge_pointrange_true")

# Pointrange dodge with logical FALSE (no dodging)
fun = function() {
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    data = results,
    type = "pointrange",
    dodge = FALSE,
    theme = "basic")
}
expect_snapshot_plot(fun, label = "dodge_pointrange_false")


#
## Dodge + layering

fun = function() {
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    data = results,
    type = "errorbar",
    dodge = 0.1,
    theme = "basic")
  tinyplot_add(type = "ribbon")
}
expect_snapshot_plot(fun, label = "dodge_errorbar_add_ribbon")


#
## Warning and errors

# Warning when dodge > 0.5
expect_warning(
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    data = results,
    type = type_pointrange(dodge = 0.6),
    theme = "basic"),
  pattern = "exceeds 0.5"
)

# Error when dodge >= 1
expect_error(
  tinyplot(estimate ~ term | model,
    ymin = conf.low, ymax = conf.high,
    data = results,
    type = type_pointrange(dodge = 1)),
  pattern = "must be in the range"
)


