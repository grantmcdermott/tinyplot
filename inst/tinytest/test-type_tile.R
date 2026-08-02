source("helpers.R")
using("tinysnapshot")

# shared fixture: correlation matrix of the base `attitude` dataset, in the same
# "long" form used by the type_tile() examples
catt = as.data.frame(as.table(cor(attitude)), responseName = "Correlation")

# "tile" and "heatmap" are aliases, as are type_tile() and type_heatmap(),
# so all four spellings must produce an identical plot.
f = function() {
  tinyplot(
    Var1 ~ Var2 | Correlation, data = catt, type = "tile",
    theme = "heatmap"
  )
}
expect_snapshot_plot(f, label = "tile_basic")

# "heatmap" alias (should be identical to the above)
f = function() {
  tinyplot(
    Var1 ~ Var2 | Correlation, data = catt, type = "heatmap",
    theme = "heatmap"
  )
}
expect_snapshot_plot(f, label = "tile_basic")

# fancy version, including gridded spacing and added labels
f = function() {
  tinyplot(
    Var1 ~ Var2 | Correlation, data = catt,
    type = type_tile(width = 0.9, height = 0.9),
    theme = "heatmap",
    legend = FALSE, xlab = NA, ylab = NA,
    main = "Correlation matrix of base attitude dataset"
  )
  tinyplot_add(
    type = "text",
    labels = round(catt$Correlation, 2),
    col = "white"
  )
}
expect_snapshot_plot(f, label = "tile_fancy")

# numeric axes: no factor conversion, ticks stay numeric
volc = data.frame(
  x         = as.vector(row(volcano)),
  y         = as.vector(col(volcano)),
  elevation = as.vector(volcano)
)
f = function() {
  tinyplot(
    y ~ x | elevation, data = volc,
    type = "tile",
    theme = "void",
    xlab = NA, ylab = NA,
    main = "Maunga Whau volcano"
  )
}
expect_snapshot_plot(f, label = "tile_numeric_axes")

# faceting: categorical tick labels must survive on both axes in every panel
d = expand.grid(
  a = factor(c("x", "y", "z")),
  b = factor(c("p", "q")),
  g = factor(c("G1", "G2"))
)
d$v = seq_len(nrow(d))
f = function() {
  tinyplot(b ~ a | v, facet = ~g, data = d, type = "tile", theme = "heatmap")
}
expect_snapshot_plot(f, label = "tile_facet")
