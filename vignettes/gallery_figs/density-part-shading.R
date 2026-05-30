library("tinyplot")

x_lo = seq(-5, -1.64, length.out = 100)
x_hi = seq(1.64, 5, length.out = 100)

tinyplot(x = -5:5, 
    type = type_function(dnorm), 
    xlab = "x",
    ylab =expression(f(x) == frac(1, sigma * sqrt(2 * pi)) ~ e^{frac(-(x - mu)^2, 2 * sigma^2)}), "Density",
    main = expression(Standard~normal~distribution: X %~% N(mu, sigma^2)),
    cap = "Shaded regions denotes top/bottom 10%",
    theme = "dynamic"
)

tinyplot_add(
    x = x_lo,
    ymin = 0,
    ymax = dnorm(x_lo),
    type = type_ribbon())

tinyplot_add(
    x = x_hi,
    ymin = 0,
    ymax = dnorm(x_hi),
    type = type_ribbon())
