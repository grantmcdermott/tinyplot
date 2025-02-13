library(tinyplot)

tinyplot(
  ~Petal.Length | Species,
  data = iris,
  type = "density",
  fill = "by",
  main = "Distribution of petal lengths",
  sub = "Grouped by species"
)
