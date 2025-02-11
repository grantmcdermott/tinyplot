library(tinyplot)

plt(
  Sepal.Length ~ Petal.Length | Sepal.Length,
  data = iris,
  facet = ~Species,
  pch = 19,
  main = "Faceted flowers",
  sub = "Brought to you by tinyplot"
)
