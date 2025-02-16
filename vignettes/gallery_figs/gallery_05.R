library(tinyplot)
tinytheme("ipsum")

tinyplot(Sepal.Width ~ Petal.Width | Species,
    data = iris,
    type = "lm")
tinyplot_add(type = "p", alpha = .2)

tinytheme() # reset theme (optional)