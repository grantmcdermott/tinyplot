library(tinyplot)

# Register a custom "pirate" theme that builds on top of "clean"
tinytheme_register(
  "pirate",
  theme = "clean",
  family = "HersheyScript",
  bg = "#f5e6c8", fg = "#3b2209",
  cex.lab = 1.5, cex.main = 1.5, cex.sub = 1.2,
  col = "#3b2209", col.axis = "#5c3a1e", col.cap = "#7a5230", 
  col.lab = "#3b2209", col.main = "#1a0f04", col.sub = "#7a5230",
  grid = TRUE, grid.col = "#c9a96e", grid.lty = "dotted",
  facet.bg = "#e8d4a8", facet.border = "#5c3a1e",
  pch = 4,
  palette.qualitative = c(
    "#8b0000", "#1a5276", "#196f3d", "#7d6608",
    "#6c3483", "#a04000", "#1b4f72", "#145a32"
  )
)

# Use it ephemerally
plt(
  Sepal.Length ~ Petal.Length | Species, iris,
  main = 'Avast, me hearties!',
  sub  = 'Here be a "pirate" theme',
  cap  = '"x" marks the spot',
  theme = 'pirate'
)