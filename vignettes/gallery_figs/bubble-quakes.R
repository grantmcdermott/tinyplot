library(tinyplot)

magnitude = quakes$mag
plt(
  lat ~ long | depth, data = quakes,
  cex = magnitude, clim = c(0.5, 4.5),
  col = "#3D3532",
  fill = 0.7, pch = 21,
  main = "Earthquakes off Fiji",
  xlab = "Longitutde",
  ylab = "Latitude",
  sub = "Data courtesy of the Harvard PRIM-H project",
  palette = "sunsetdark",
  # custom ephemeral theme
  theme = list(
    "dynamic",
    bty = "n",
    bg = "#E0D9D2",
    cex = 1.2, cex.main = 1.5, cex.lab = 1.2,
    col = "#3D3532",
    family = "HersheyScript",
    grid = TRUE, grid.col = "gray70",
    lwd = 0.5
  )
)