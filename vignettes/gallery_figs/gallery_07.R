library("tinyplot")
tinytheme("clean")

i = 4*(0:10)

tinyplot(
  xmin = 100+i, ymin = 300+i, xmax = 150+i, ymax = 380+i,
  by = i, fill = 0.2,
  type = "rect",
  palette = "viridis"
)
