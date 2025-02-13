library(tinyplot)

plt(
  ~Petal.Length | Species,
  data = iris,
  type = "density",
  fill = "by",
  main = "Distribution of petal lengths",
  sub = "Grouped by species"
)
