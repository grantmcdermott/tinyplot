library(tinyplot)

i = seq(0, 2*pi, by = 0.01)
x = 16*sin(i)^3
y = 13*cos(i) - 5*cos(2*i) - 2*cos(3*i) - cos(4*i)

plt(
  x, y,
  type = "polygon",
  col = "red", fill = adjustcolor("firebrick", 0.8),
  xlab = NA, ylab = NA,
  main = "Be my tinyplot Valentine!",
  theme = list("void", family = "HersheyScript", adj.main = 0.5, cex.main = 3)
)
