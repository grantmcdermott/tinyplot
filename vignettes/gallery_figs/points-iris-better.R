library(tinyplot)

tinyplot(
  Sepal.Length ~ Petal.Length | Species,
  data = iris,
  theme = 'float', grid = TRUE,
  main = "A more exciting scatterplot"
)
