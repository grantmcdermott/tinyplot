library("tinyplot")

x_lo = seq(-5, -1.64, length.out = 100)
x_hi = seq(1.64, 5, length.out = 100)

tinyplot(x = -5:5, 
    type = type_function(dnorm), 
    xlab = "X", ylab = "Density",
    main = "Standard normal distribution with 10% shaded region.")

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
