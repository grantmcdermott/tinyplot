library("tinyplot")

cols = c("black", "green", "orange")
tinyplot(mpg ~ hp | factor(cyl), facet = ~ factor(cyl), data = mtcars, col = cols)
tinyplot_add(type = type_vline(v = c(100, 150, 200)), lty = 3, lwd = 3)
