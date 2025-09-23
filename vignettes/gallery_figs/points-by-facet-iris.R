library(tinyplot)

tinyplot(
  Sepal.Length ~ Petal.Length | Sepal.Length,
  data = iris,
  facet = ~Species,
  pch = 19
)
