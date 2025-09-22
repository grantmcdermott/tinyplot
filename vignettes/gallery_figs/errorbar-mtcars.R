library(tinyplot)

# fit a model and extract coefficients
mod = lm(mpg ~ wt * factor(am), mtcars)
coefs = data.frame(names(coef(mod)), coef(mod), confint(mod))
colnames(coefs) = c("term", "est", "lwr", "upr")

# plot the coefficients with error bars
tinyplot(
    est ~ term, data = coefs,
    ymin = lwr, ymax = upr,
    type = type_errorbar(length = 0.1),
    theme = "minimal"
)
