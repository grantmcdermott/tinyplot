data("penguins", package = "datasets")
pal = c("darkorange", "purple", "cyan4")

library("tinyplot")
tinytheme("minimal", las = 1)
tinyplot(
  ~ island | sex + species,
  facet = species ~ 1,
  data = penguins,
  type = "barplot",
  flip = TRUE,
  col = "transparent",
  grid = "X",
  bg = rbind(pal, colorspace::lighten(pal, 0.6))
)
