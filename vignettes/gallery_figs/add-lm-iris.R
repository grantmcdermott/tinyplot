library(tinyplot)

tinyplot(
    Sepal.Width ~ Petal.Width | Species,
    data = iris,
    theme = "ipsum"
)
tinyplot_add(type = "lm")
