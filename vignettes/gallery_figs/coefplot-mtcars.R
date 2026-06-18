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
    xlab = NA, ylab = "Estimate",
    flip = TRUE,
    draw = abline(v = 0, lty = 2, col = "hotpink"),
    main = "Coefficient plot",
    sub = "Determinants of fuel efficiency",
    cap = expression(Model: mpg == beta[0] + beta[1] %.% wt + beta[2] %.% am + beta[3] %.% wt %.% am),
    theme = "socviz"
)
