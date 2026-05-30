library(tinyplot)

plt(
  lat ~ long | depth, data = quakes,
  main = "Earthquakes off Fiji",
  xlab = "Longitutde",
  ylab = "Latitude",
  cap = "Data courtesy of the Harvard PRIM-H project",
  palette = "mako",
  # custom ephemeral theme
  theme = list(
    "dynamic",
    bty = "n",
    bg = "#E0D9D2",# "#F7F3EF",
    cex = 1.2, cex.main = 1.5, cex.lab = 1.2,
    col = "#3D3532",
    family = "HersheyScript",
    grid = TRUE, grid.col = "gray95"
  )
)
