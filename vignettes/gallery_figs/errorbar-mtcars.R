library(tinyplot)

# fit a model and extract coefficients
mod = lm(mpg ~ wt * factor(am), mtcars)
coefs = data.frame(names(coef(mod)), coef(mod), confint(mod))
colnames(coefs) = c("term", "est", "lwr", "upr")

# plot the coefficients with error bars
tinyplot(
    est ~ term, data = coefs,
    ymin = lwr, ymax = upr,
    type = "errorbar",
    flip = TRUE,
    draw = abline(v = 0, lty = 2, col = "grey50"),
    theme = "classic"
)
