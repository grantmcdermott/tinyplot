library("tinyplot")

pal = c("darkorange","purple","cyan4")

plt(
  ~flipper_len | species,
  data = penguins, 
  type = "hist",
  breaks = 30,
  theme = "bw",
  palette = pal,
  main = "Histogram of flipper length",
  sub = "Grouped by species"
)