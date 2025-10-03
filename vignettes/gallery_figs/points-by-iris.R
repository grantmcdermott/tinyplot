library(tinyplot)

tinyplot(
  Sepal.Length ~ Petal.Length | Species,
  data = iris,
  palette = "dark",
  pch = 16,
  grid = TRUE,
  frame = FALSE
)
