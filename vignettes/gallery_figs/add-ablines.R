library("tinyplot")

tinyplot(mpg ~ hp | cyl, facet = "by", data = mtcars,
         legend = FALSE,
         theme = "classic",
         main = "mtcars: Mileage vs horsepower",
         sub = "Faceted by no. of cylinders", 
         cap = "Note: Dotted lines denote means") 
tinyplot_add(type = type_hline(with(mtcars, tapply(mpg, cyl, mean))), lty = 2)
tinyplot_add(type = type_vline(with(mtcars, tapply(hp, cyl, mean))), lty = 2)
