library("tinyplot")

tinyplot(mpg ~ hp | cyl, facet = "by", data = mtcars) 
tinyplot_add(type = type_hline(with(mtcars, tapply(mpg, cyl, mean))), lty = 2)
tinyplot_add(type = type_vline(with(mtcars, tapply(hp, cyl, mean))), lty = 2)
